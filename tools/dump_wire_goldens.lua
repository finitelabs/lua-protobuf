-- Prints each vector's golden bytes as Lua reads them, for tools/check_wire_vectors.

local repo = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
repo = repo .. "../"

for _, vector in ipairs(dofile(repo .. "test/generated/wire_vectors.lua")) do
  print(vector.name .. "\t" .. (vector.golden:gsub(".", function(char)
    return string.format("%02x", char:byte())
  end)))
end
