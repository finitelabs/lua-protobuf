-- Emits each vector's golden bytes as Lua actually reads them, so
-- tools/check_wire_vectors can compare them to what Python wrote. The escape
-- encoding in the generated file is the thing under test here.

local repo = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
repo = repo .. "../"

for _, vector in ipairs(dofile(repo .. "test/generated/wire_vectors.lua")) do
  print(vector.name .. "\t" .. (vector.golden:gsub(".", function(char)
    return string.format("%02x", char:byte())
  end)))
end
