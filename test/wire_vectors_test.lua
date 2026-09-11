-- @test-name Wire vectors
--
-- Differential wire-format coverage against test/generated/wire_vectors.lua.
--
-- Decode is compared strictly. Encode is compared by re-decoding, never bytewise:
-- this encoder never packs, so its bytes differ from the reference's for repeated
-- scalars even when correct.

local pb = require("protobuf")
local bit64 = require("bitn").bit64

-- Relative to this file, not the working directory.
local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"

local schema = dofile(here .. "generated/test_messages_proto3_schema.lua")
local vectors = dofile(here .. "generated/wire_vectors.lua")

local MESSAGE = "protobuf_test_messages.proto3.TestAllTypesProto3"
local root = schema.Message[MESSAGE]

local testlib = dofile(here .. "testlib.lua")
local report = testlib.new("Wire vectors")

local function is_int64_field(field)
  local t = schema.DataType
  return field.type == t.INT64
    or field.type == t.UINT64
    or field.type == t.SINT64
    or field.type == t.FIXED64
    or field.type == t.SFIXED64
end

local function describe(value)
  if type(value) == "table" then
    if bit64.is_int64(value) then
      return "Int64(" .. bit64.to_hex(value) .. ")"
    end
    return "table"
  end
  if type(value) == "string" then
    return string.format("%q", value)
  end
  return tostring(value)
end

-- Forward-declared: compare_value recurses into it.
local compare_message

--- @return boolean equal
--- @return string? detail
local function compare_value(field, got, want, path)
  if is_int64_field(field) then
    if type(got) ~= "table" or not bit64.is_int64(got) then
      return false, path .. ": want an Int64 table, got " .. describe(got)
    end
    if not bit64.eq(got, want) then
      return false, path .. ": want " .. describe(want) .. ", got " .. describe(got)
    end
    return true
  end

  if field.type == schema.DataType.MESSAGE then
    local sub = schema.Message[field.subschema]
    if not sub then
      return false, path .. ": schema has no message " .. tostring(field.subschema)
    end
    if type(got) ~= "table" then
      return false, path .. ": want a table, got " .. describe(got)
    end
    return compare_message(sub, got, want, path)
  end

  if got ~= want then
    return false, path .. ": want " .. describe(want) .. ", got " .. describe(got)
  end
  return true
end

--- Matches keys pairwise: int64 map keys decode to Int64 tables, which cannot be
--- looked up by value.
local function compare_map(field, got, want, path)
  local entry = schema.Message[field.subschema]
  local key_field, value_field = entry.fields[1], entry.fields[2]

  local function keys_equal(a, b)
    return (compare_value(key_field, a, b, path))
  end

  local matched = {}
  local want_count = 0
  for want_key, want_value in pairs(want) do
    want_count = want_count + 1
    local found = nil
    for got_key in pairs(got) do
      if not matched[got_key] and keys_equal(got_key, want_key) then
        found = got_key
        break
      end
    end
    if found == nil then
      return false, path .. ": no entry with key " .. describe(want_key)
    end
    matched[found] = true
    local ok, detail = compare_value(
      value_field,
      got[found],
      want_value,
      path .. "[" .. describe(want_key) .. "]"
    )
    if not ok then
      return false, detail
    end
  end

  local got_count = 0
  for _ in pairs(got) do
    got_count = got_count + 1
  end
  if got_count ~= want_count then
    return false, string.format("%s: want %d entries, got %d", path, want_count, got_count)
  end
  return true
end

--- Iterates the schema, not either table, so missing and unexpected fields both fail.
function compare_message(messageSchema, got, want, path)
  for _, field in pairs(messageSchema.fields) do
    local got_value = got[field.name]
    local want_value = want[field.name]
    local where = path .. "." .. field.name

    if got_value == nil and want_value == nil then -- luacheck: ignore
      -- Absent on both sides: a proto3 zero value.
    elseif want_value == nil then
      return false, where .. ": unexpected value " .. describe(got_value)
    elseif got_value == nil then
      return false, where .. ": missing, want " .. describe(want_value)
    elseif field.map then
      if type(got_value) ~= "table" then
        return false, where .. ": want a map table, got " .. describe(got_value)
      end
      local ok, detail = compare_map(field, got_value, want_value, where)
      if not ok then
        return false, detail
      end
    elseif field.repeated then
      if type(got_value) ~= "table" then
        return false, where .. ": want a list, got " .. describe(got_value)
      end
      if #got_value ~= #want_value then
        return false, string.format("%s: want %d elements, got %d", where, #want_value, #got_value)
      end
      for i = 1, #want_value do
        local ok, detail = compare_value(field, got_value[i], want_value[i], where .. "[" .. i .. "]")
        if not ok then
          return false, detail
        end
      end
    else
      local ok, detail = compare_value(field, got_value, want_value, where)
      if not ok then
        return false, detail
      end
    end
  end
  return true
end

local function assert_case(name, direction, ok, detail)
  report:count()
  if not ok then
    report:record(string.format("%s | %s: %s", name, direction, detail or "failed"))
  end
end

local function key_less(a, b)
  local ta, tb = type(a), type(b)
  if ta ~= tb then
    return ta < tb
  elseif ta == "table" then
    return a[1] < b[1] or (a[1] == b[1] and a[2] < b[2])
  elseif ta == "boolean" then
    return b and not a
  end
  return a < b
end

-- Neither comparison above can see emission order: both re-parse into messages.
local function check_order(bytes)
  local pos, last_field, last_key = 1, 0, {}
  while pos <= #bytes do
    local tag
    tag, pos = pb.decode_varint(bytes, pos)
    local field_number, wire_type = math.floor(tag / 8), tag % 8
    if field_number < last_field then
      return false, string.format("field %d emitted after field %d", field_number, last_field)
    end
    last_field = field_number
    local field = root.fields[field_number]
    if wire_type == schema.WireType.VARINT then
      pos = select(2, pb.decode_varint(bytes, pos))
    elseif wire_type == schema.WireType.FIXED64 then
      pos = pos + 8
    elseif wire_type == schema.WireType.FIXED32 then
      pos = pos + 4
    else
      local data
      data, pos = pb.decode_length_delimited(bytes, pos)
      if field and field.map then
        local entry_schema = schema.Message[field.subschema]
        local key = pb.decode(schema, entry_schema, data)[entry_schema.fields[1].name]
        local previous = last_key[field_number]
        if previous ~= nil and not key_less(previous, key) then
          return false, string.format("%s: key %s emitted after %s", field.name, describe(key), describe(previous))
        end
        last_key[field_number] = key
      end
    end
  end
  return true
end

for _, vector in ipairs(vectors) do
  -- reference -> Lua
  local ok, decoded = pcall(pb.decode, schema, root, vector.golden)
  if not ok then
    assert_case(vector.name, "decode", false, "decode raised: " .. tostring(decoded))
  else
    local equal, detail = compare_message(root, decoded, vector.expected, MESSAGE)
    assert_case(vector.name, "decode", equal, detail)
  end

  -- Lua -> reference, semantically
  local encoded_ok, encoded = pcall(pb.encode, schema, root, vector.expected)
  if not encoded_ok then
    assert_case(vector.name, "encode", false, "encode raised: " .. tostring(encoded))
  else
    local redecoded_ok, redecoded = pcall(pb.decode, schema, root, encoded)
    if not redecoded_ok then
      assert_case(vector.name, "encode", false, "re-decode raised: " .. tostring(redecoded))
    else
      local equal, detail = compare_message(root, redecoded, vector.expected, MESSAGE)
      assert_case(vector.name, "encode", equal, detail)
      local ordered, why = check_order(encoded)
      assert_case(vector.name, "order", ordered, why)
    end
  end
end

report:finish(
  string.format("%d assertions over %d vectors", report.checked, #vectors),
  "reference and Lua agree on every vector"
)
