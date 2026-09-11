-- @test-name Math fallbacks
-- @test-modes native
--
-- Differential check of the math.frexp/math.ldexp fallbacks against the natives.

local testlib = dofile(debug.getinfo(1, "S").source:match("^@(.*[/\\])") .. "testlib.lua")
local report = testlib.new("Math fallbacks")

local real_frexp, real_ldexp = math.frexp, math.ldexp

-- Fail rather than pass vacuously when there is no oracle.
if not (real_frexp and real_ldexp) then
  report:abort("no native math.frexp/math.ldexp to compare against")
end

-- A second copy of the module, loaded with the globals cleared, binds the fallbacks.
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

if fb_frexp == nil or fb_ldexp == nil then
  report:abort("pb._math is not exposed")
end
-- Otherwise the comparison below would be native against native.
if fb_frexp == real_frexp or fb_ldexp == real_ldexp then
  report:abort("the module bound the native functions, not its fallbacks")
end

-- The type checker cannot see that `abort` exits.
--- @cast fb_frexp -nil
--- @cast fb_ldexp -nil

local same = testlib.same_number

local function check_frexp(x, label)
  report:count()
  local want_m, want_e = real_frexp(x)
  local got_m, got_e = fb_frexp(x)
  if not same(want_m, got_m) or want_e ~= got_e then
    -- %s, not %d: a broken fallback can return a non-integer exponent.
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

-- Native ldexp, not 2 ^ k: LuaJIT 2.0 returns 0 for 2 ^ k below the normal range.
for k = -1074, 1023 do
  local power = real_ldexp(1.0, k)
  check_frexp(power, "pow2")
  check_frexp(-power, "pow2 negated")
  check_frexp(power * 1.5, "pow2 scaled")
  check_frexp(power * (1 - 2 ^ -53), "pow2 just under")
end

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
