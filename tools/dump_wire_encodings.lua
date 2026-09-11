-- Prints this library's encoding of each vector for tools/check_wire_vectors, one
-- record per line: "<name>\t<OK|ERROR>\t<hex|error message>".

local repo = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
repo = repo .. "../"

local pb = require("protobuf")
local schema = dofile(repo .. "test/generated/test_messages_proto3_schema.lua")
local vectors = dofile(repo .. "test/generated/wire_vectors.lua")

local root = schema.Message["protobuf_test_messages.proto3.TestAllTypesProto3"]

for _, vector in ipairs(vectors) do
  local ok, encoded = pcall(pb.encode, schema, root, vector.expected)
  if ok then
    print(vector.name .. "\tOK\t" .. (encoded:gsub(".", function(char)
      return string.format("%02x", char:byte())
    end)))
  else
    print(vector.name .. "\tERROR\t" .. tostring(encoded))
  end
end
