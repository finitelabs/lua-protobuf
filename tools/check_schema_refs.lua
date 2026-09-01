-- Asserts a generated schema is internally consistent: it loads, and every
-- `subschema` a field points at is a message the same file registered.
--
-- Loading is half the assertion, not setup: a qualified name emitted as bare dot
-- syntax (`ProtoSchema.Message.pkg.Name`) is a chain of table indexes that raises
-- at load time rather than parsing into a single key.

local path = ...
if not path then
  io.stderr:write("usage: check_schema_refs.lua <generated-schema.lua>\n")
  os.exit(1)
end

local ok, schema = pcall(dofile, path)
if not ok then
  io.stderr:write("FAIL: " .. path .. " does not load: " .. tostring(schema) .. "\n")
  os.exit(1)
end

local messages = schema.Message or {}
local dangling, checked = {}, 0

for message_name, message in pairs(messages) do
  for number, field in pairs(message.fields or {}) do
    if field.subschema then
      checked = checked + 1
      if messages[field.subschema] == nil then
        dangling[#dangling + 1] = string.format(
          "%s.%s (field %d) -> %q",
          message_name,
          field.name or "?",
          number,
          field.subschema
        )
      end
    end
  end
end

local count = 0
for _ in pairs(messages) do
  count = count + 1
end

if #dangling > 0 then
  table.sort(dangling)
  io.stderr:write("FAIL: " .. #dangling .. " subschema reference(s) resolve to no message:\n")
  for _, entry in ipairs(dangling) do
    io.stderr:write("  " .. entry .. "\n")
  end
  os.exit(1)
end

print(string.format("%s: %d messages, %d subschema references all resolve", path, count, checked))
