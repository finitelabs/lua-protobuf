-- Emits this library's own encoding of every wire vector, for the reference
-- implementation to parse back in tools/check_wire_vectors.
--
-- One tab-separated record per vector: "<name>\t<status>\t<payload>", where
-- status is OK with the encoding as hex, GAP or VERSION with the ticket, or
-- ERROR with the message the encoder raised.

local repo = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
repo = repo .. "../"

local pb = require("protobuf")
local schema = dofile(repo .. "test/generated/test_messages_proto3_schema.lua")
local vectors = dofile(repo .. "test/generated/wire_vectors.lua")
local known_gaps = dofile(repo .. "test/known_gaps.lua")

local root = schema.Message["protobuf_test_messages.proto3.TestAllTypesProto3"]

for _, vector in ipairs(vectors) do
  local key = vector.name .. " | encode"
  local gap = known_gaps.strict[key]
  local unstable = known_gaps.version_dependent[key]
  if gap then
    print(vector.name .. "\tGAP\t" .. gap)
  elseif unstable then
    print(vector.name .. "\tVERSION\t" .. unstable)
  else
    local ok, encoded = pcall(pb.encode, schema, root, vector.expected)
    if ok then
      print(vector.name .. "\tOK\t" .. (encoded:gsub(".", function(char)
        return string.format("%02x", char:byte())
      end)))
    else
      print(vector.name .. "\tERROR\t" .. tostring(encoded))
    end
  end
end
