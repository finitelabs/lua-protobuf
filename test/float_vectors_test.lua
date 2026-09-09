-- Differential check of the float and double codecs against vectors generated
-- from Python's struct module. See tools/gen_float_vectors.
--
-- Mismatches are grouped by the defect class that explains them and compared
-- against exact counts. A count that moves in either direction fails: upward is
-- a regression, downward means a defect was fixed without the expectation being
-- updated, which is the only signal that a known gap has closed.

local pb = require("protobuf")

local vectors = assert(loadfile((os.getenv("PB_TEST_DIR") or "test") .. "/float_vectors.lua"))()

-- Measured on 6d9e624 against every interpreter in the matrix. Both classes are
-- FL-16 Part 1 and both go to zero when the subnormal and half-even work lands.
local EXPECTED = {
  f32_subnormal_encode = 141,
  f32_tie_encode = 124,
  f64_subnormal_encode = 171,
  f32_subnormal_decode = 141,
  f64_subnormal_decode = 171,
}

local function to_hex(s)
  return (s:gsub(".", function(c)
    return string.format("%02X", c:byte())
  end))
end

local function from_hex(hex)
  return (hex:gsub("%x%x", function(byte)
    return string.char(tonumber(byte, 16))
  end))
end

local function byte_at(hex, index)
  return tonumber(hex:sub(index * 2 - 1, index * 2), 16)
end

-- IEEE exponent field read from the oracle's own bytes, so the classification
-- never depends on what the codec under test produced.
local function f32_is_subnormal(hex)
  local exponent = (byte_at(hex, 4) % 128) * 2 + math.floor(byte_at(hex, 3) / 128)
  return exponent == 0
end

local function f64_is_subnormal(hex)
  local exponent = (byte_at(hex, 8) % 128) * 16 + math.floor(byte_at(hex, 7) / 16)
  return exponent == 0
end

-- Equality that separates the two zeros, which compare equal under ==.
local function same_number(a, b)
  if a ~= a or b ~= b then
    return a ~= a and b ~= b
  end
  if a == 0 and b == 0 then
    return (1 / a) == (1 / b)
  end
  return a == b
end

local counts = {}
local unexplained = {}

local function record(class, detail)
  if class then
    counts[class] = (counts[class] or 0) + 1
  elseif #unexplained < 10 then
    unexplained[#unexplained + 1] = detail
  else
    unexplained.overflow = (unexplained.overflow or 0) + 1
  end
end

for i = 1, #vectors do
  local value, f32_hex, f32_value, f64_hex, group, is_tie = vectors[i][1], vectors[i][2], vectors[i][3], vectors[i][4], vectors[i][5], vectors[i][6]
  local where = string.format("vector %d (%s, %.17g)", i, group, value)

  if f32_hex then
    local got = to_hex(pb.encode_float(value))
    if got ~= f32_hex then
      local class = f32_is_subnormal(f32_hex) and "f32_subnormal_encode"
        or (is_tie and "f32_tie_encode" or nil)
      record(class, string.format("%s encode_float want %s got %s", where, f32_hex, got))
    end

    local decoded = pb.decode_float(from_hex(f32_hex), 1)
    if not same_number(decoded, f32_value) then
      local class = f32_is_subnormal(f32_hex) and "f32_subnormal_decode" or nil
      record(class, string.format("%s decode_float want %.17g got %.17g", where, f32_value, decoded))
    end
  end

  local got = to_hex(pb.encode_double(value))
  if got ~= f64_hex then
    local class = f64_is_subnormal(f64_hex) and "f64_subnormal_encode" or nil
    record(class, string.format("%s encode_double want %s got %s", where, f64_hex, got))
  end

  local decoded = pb.decode_double(from_hex(f64_hex), 1)
  if not same_number(decoded, value) then
    local class = f64_is_subnormal(f64_hex) and "f64_subnormal_decode" or nil
    record(class, string.format("%s decode_double want %.17g got %.17g", where, value, decoded))
  end
end

print(string.format("Float codec vectors: %d, math.frexp %s", #vectors, math.frexp and "native" or "fallback"))

local failed = false

for _, detail in ipairs(unexplained) do
  print("  FAIL: " .. detail)
  failed = true
end
if unexplained.overflow then
  print(string.format("  FAIL: and %d further unexplained mismatches", unexplained.overflow))
  failed = true
end

local classes = {}
for class in pairs(EXPECTED) do
  classes[#classes + 1] = class
end
for class in pairs(counts) do
  if EXPECTED[class] == nil then
    classes[#classes + 1] = class
  end
end
table.sort(classes)

for _, class in ipairs(classes) do
  local actual, expected = counts[class] or 0, EXPECTED[class] or 0
  if actual == expected then
    print(string.format("  PASS: %s, %d known mismatches", class, actual))
  else
    print(string.format("  FAIL: %s, expected %d mismatches, saw %d", class, expected, actual))
    failed = true
  end
end

if failed then
  print("Float codec vectors: FAILED")
  os.exit(1)
end

print("Float codec vectors: all vectors accounted for")
