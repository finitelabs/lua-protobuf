-- Prints what Lua makes of each float vector, for tools/check_float_vectors. One
-- record per line, tab separated:
--
--   <name> <golden hex> <value> <input> <decode> <encode hex | ERROR | SKIP>
--
-- Numbers go out as %.17g with the non-finite cases spelled out, because "nan" and
-- "-nan" and "inf" are all platform-dependent under the C formatter.

local repo = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
repo = repo .. "../"

local pb = require("protobuf")
local vectors = dofile(repo .. "test/generated/float_vectors.lua")

local ENCODE = { [4] = pb.encode_float, [8] = pb.encode_double }
local DECODE = { [4] = pb.decode_float, [8] = pb.decode_double }

local function hex(bytes)
  return (bytes:gsub(".", function(char)
    return string.format("%02x", char:byte())
  end))
end

local function num(value)
  if value ~= value then
    return "nan"
  elseif value == math.huge then
    return "inf"
  elseif value == -math.huge then
    return "-inf"
  elseif value == 0 then
    return 1 / value < 0 and "-0" or "0"
  end
  return string.format("%.17g", value)
end

for _, vector in ipairs(vectors) do
  local input = vector.input
  if input == nil then
    input = vector.value
  end

  local decoded = "ERROR"
  local ok, result = pcall(DECODE[vector.width], vector.golden, 1)
  if ok then
    decoded = num(result)
  else
    decoded = "ERROR:" .. tostring(result):gsub("%s+", " ")
  end

  local encoded = "SKIP"
  if not vector.decode_only then
    local encode_ok, bytes = pcall(ENCODE[vector.width], input)
    if encode_ok then
      encoded = hex(bytes)
    else
      encoded = "ERROR:" .. tostring(bytes):gsub("%s+", " ")
    end
  end

  print(table.concat({
    vector.name,
    hex(vector.golden),
    num(vector.value),
    num(input),
    decoded,
    encoded,
  }, "\t"))
end
