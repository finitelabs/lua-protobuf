-- @test-name Float vectors
--
-- Differential IEEE 754 coverage against test/generated/float_vectors.lua.
--
-- Both directions are compared strictly: a float or double encoding is canonical, so
-- unlike the wire vectors there is no representation freedom to allow for.

local pb = require("protobuf")

-- Relative to this file, not the working directory.
local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"

local vectors = dofile(here .. "generated/float_vectors.lua")
local testlib = dofile(here .. "testlib.lua")
local report = testlib.new("Float vectors")

local ENCODE = { [4] = pb.encode_float, [8] = pb.encode_double }
local DECODE = { [4] = pb.decode_float, [8] = pb.decode_double }

local function hex(bytes)
  return (bytes:gsub(".", function(char)
    return string.format("%02x", char:byte())
  end))
end

-- Ordered, so a bucket that runs zero vectors is visible rather than absent.
local bucket_order, buckets = {}, {}

local function bucket_of(name)
  local head, tail = name:match("^(%S+)%s+(%S+)")
  local key = head .. " " .. tail
  if not buckets[key] then
    buckets[key] = { vectors = 0, checks = 0, failures = 0 }
    bucket_order[#bucket_order + 1] = key
  end
  return buckets[key]
end

local function check(bucket, ok, detail)
  report:count()
  bucket.checks = bucket.checks + 1
  if not ok then
    bucket.failures = bucket.failures + 1
    report:record(detail)
  end
end

--- Asserts a decimal literal parsed to the exact rational the generator meant.
---
--- `strtod` is the one part of the corpus no assertion on the codec can pin: a
--- midpoint that reads back as one of its neighbours still encodes to the golden, so
--- it would pass while testing nothing.
local function check_literal(bucket, name, label, value, units, scale)
  if units == nil then
    return
  end
  -- Two factors, because 2 ^ 1074 is infinity and a subnormal needs that scale.
  local scaled = value * 2 ^ scale[1] * 2 ^ scale[2]
  check(
    bucket,
    scaled == units,
    string.format("%s | %s literal: %.17g scales to %.17g, want %d", name, label, value, scaled, units)
  )
end

for _, vector in ipairs(vectors) do
  local bucket = bucket_of(vector.name)
  bucket.vectors = bucket.vectors + 1
  local width = vector.width

  check_literal(bucket, vector.name, "value", vector.value, vector.units, vector.scale)
  if vector.input ~= nil then
    check_literal(bucket, vector.name, "input", vector.input, vector.input_units, vector.input_scale)
  end

  check(
    bucket,
    #vector.golden == width,
    string.format("%s | golden: want %d bytes, got %d", vector.name, width, #vector.golden)
  )

  local ok, decoded, new_pos = pcall(DECODE[width], vector.golden, 1)
  if not ok then
    check(bucket, false, string.format("%s | decode raised: %s", vector.name, tostring(decoded)))
  else
    check(
      bucket,
      testlib.same_number(decoded, vector.value),
      string.format(
        "%s | decode %s: want %.17g, got %.17g",
        vector.name,
        hex(vector.golden),
        vector.value,
        decoded
      )
    )
    check(
      bucket,
      new_pos == 1 + width,
      string.format("%s | decode position: want %d, got %s", vector.name, 1 + width, tostring(new_pos))
    )
  end

  if not vector.decode_only then
    local input = vector.input
    if input == nil then
      input = vector.value
    end
    local encoded_ok, encoded = pcall(ENCODE[width], input)
    if not encoded_ok then
      check(bucket, false, string.format("%s | encode raised: %s", vector.name, tostring(encoded)))
    else
      check(
        bucket,
        encoded == vector.golden,
        string.format(
          "%s | encode %.17g: want %s, got %s",
          vector.name,
          input,
          hex(vector.golden),
          hex(encoded)
        )
      )
    end
  end
end

for _, key in ipairs(bucket_order) do
  local bucket = buckets[key]
  report:note(string.format(
    "%-22s %4d vectors, %5d assertions, %d failed",
    key,
    bucket.vectors,
    bucket.checks,
    bucket.failures
  ))
end

report:finish(
  string.format("%d assertions over %d vectors", report.checked, #vectors),
  "the oracle agrees with both directions on every vector"
)
