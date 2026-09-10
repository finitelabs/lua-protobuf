-- @test-name Math fallbacks
-- @test-modes native
--
-- Native only: this module clears the globals and re-requires the module itself
-- to reach the fallbacks, and it needs the native functions surviving as the
-- oracle to compare them against.
--
-- Differential check of the module's math.frexp and math.ldexp fallbacks
-- against the interpreter's native implementations.
--
-- The native functions are the oracle, so this needs no generated vectors and
-- no Python. It runs wherever both natives exist, which is every entry in the
-- CI matrix: 5.1 and 5.2 have them, and 5.3/5.4/5.5 are built with
-- LUA_COMPAT_5_3 or ship them anyway. The fallbacks are only selected on a
-- 5.3/5.4 built without that flag, so nothing here would otherwise be reached.
--
-- The corpus is enumerated rather than sampled, so the expectations do not
-- depend on a random seed or on the host's RNG.

local testlib = dofile(debug.getinfo(1, "S").source:match("^@(.*[/\\])") .. "testlib.lua")
local report = testlib.new("Math fallbacks")

local real_frexp, real_ldexp = math.frexp, math.ldexp

-- Every matrix entry supplies both natively. Their absence means the run is not
-- one of the configurations this suite is meant to cover, and silently passing
-- would leave the fallbacks with no assertion at all, which is the state this
-- test exists to end.
if not (real_frexp and real_ldexp) then
  report:abort("no native math.frexp/math.ldexp to compare against")
end

-- Bind a copy of the module against its own fallbacks regardless of which mode
-- run_tests.sh invoked us in, so this file asserts the same thing in both.
package.loaded["protobuf"] = nil
math.frexp, math.ldexp = nil, nil
local ok, fallback_pb = pcall(require, "protobuf")
math.frexp, math.ldexp = real_frexp, real_ldexp
package.loaded["protobuf"] = nil

if not ok then
  report:abort("could not load the module with the globals cleared: " .. tostring(fallback_pb))
end

local fb_frexp = fallback_pb._math and fallback_pb._math.frexp
local fb_ldexp = fallback_pb._math and fallback_pb._math.ldexp

-- Without this the comparison below would run native against native and pass
-- without exercising a single line of the fallbacks.
if fb_frexp == nil or fb_ldexp == nil then
  report:abort("pb._math is not exposed")
end
if fb_frexp == real_frexp or fb_ldexp == real_ldexp then
  report:abort("the module bound the native functions, not its fallbacks")
end

-- `abort` exits, but that is a method call rather than an inline `os.exit`, so
-- the type checker cannot see the guard above narrows these.
--- @cast fb_frexp -nil
--- @cast fb_ldexp -nil

-- == treats the two zeros as equal and every NaN as unequal, neither of which
-- is the comparison this test wants.
local same = testlib.same_number

local function check_frexp(x, label)
  report:count()
  local want_m, want_e = real_frexp(x)
  local got_m, got_e = fb_frexp(x)
  if not same(want_m, got_m) or want_e ~= got_e then
    -- %s throughout: a broken fallback can return a non-integer or infinite
    -- exponent, and %d raises on those, which would end the run here rather
    -- than reporting this comparison and continuing.
    report:record(string.format("frexp(%s) [%s] want (%s, %s) got (%s, %s)", tostring(x), label, tostring(want_m), tostring(want_e), tostring(got_m), tostring(got_e)))
  end
end

local function check_ldexp(m, e, label)
  report:count()
  local want = real_ldexp(m, e)
  local got = fb_ldexp(m, e)
  if not same(want, got) then
    report:record(string.format("ldexp(%s, %d) [%s] want %s got %s", tostring(m), e, label, tostring(want), tostring(got)))
  end
end

-- Powers of two are built with native ldexp rather than 2 ^ k: LuaJIT 2.0
-- evaluates 2 ^ k as zero below the normal range, which would silently replace
-- the subnormal half of the corpus with zeros on that entry alone.
for k = -1074, 1023 do
  local power = real_ldexp(1.0, k)
  check_frexp(power, "pow2")
  check_frexp(-power, "pow2 negated")
  check_frexp(power * 1.5, "pow2 scaled")
  check_frexp(power * (1 - 2 ^ -53), "pow2 just under")
end

-- The two reported defects, kept as named cases so a regression names itself.
check_frexp(2.350988701644575e-38, "encode_double factor of two")
check_frexp(1.7976931348623157e+308, "top binade collapse")
check_frexp(4.4501477170144028e-308, "largest subnormal boundary")
check_frexp(2.2250738585072014e-308, "smallest normal")

local negative_zero = -1 / math.huge
check_frexp(0.0, "positive zero")
check_frexp(negative_zero, "negative zero")
check_frexp(math.huge, "positive infinity")
check_frexp(-math.huge, "negative infinity")
check_frexp(0 / 0, "nan")

-- ldexp has to stay exact where the result is representable but 2 ^ e alone is
-- not, which is the whole subnormal and top-binade range.
local mantissas = {
  1.0,
  -1.0,
  0.5,
  0.75,
  1.5,
  1 - 2 ^ -53,
  real_ldexp(1.0, -1074),
  real_ldexp(1.0, -1022),
  1.7976931348623157e+308,
}
for _, m in ipairs(mantissas) do
  for e = -2200, 2200 do
    check_ldexp(m, e, "sweep")
  end
end
check_ldexp(0.0, 0, "positive zero")
check_ldexp(negative_zero, 10, "negative zero")
check_ldexp(math.huge, -10, "infinity")

report:finish(
  string.format(
    "%d comparisons, math.frexp %s, 2 ^ -1024 = %.17g",
    report.checked,
    real_frexp and "native present" or "native absent",
    2 ^ -1024
  ),
  "fallbacks match native on every comparison"
)
