-- @test-name Wire vectors
--
-- Both math modes: the float and double fields in the corpus run through
-- whichever frexp/ldexp the module bound, so the fallback path gets driven by
-- the whole wire suite rather than by the math test alone.
--
-- Differential wire-format coverage against the reference protobuf
-- implementation, driven by test/generated/wire_vectors.lua.
--
-- Targets the four field-level defects no suite caught: DRV-104 (nested types
-- dropped by the generator), DRV-105 (maps unimplemented), DRV-106 (a
-- length-delimited field silently truncated) and DRV-107 (packed repeated
-- scalars not unpacked). Every one was found in the field or in review.
--
-- Two directions, asserted differently:
--
--   reference -> Lua   the golden bytes decode to the expected table. Input is
--                      byte-exact, so this is strict.
--   Lua -> reference   re-decoding Lua's own encoding reproduces the expected
--                      table. This is semantic, not bytewise, because the
--                      encoder never packs: comparing its output to the
--                      reference bytes would fail on correct code for every
--                      repeated scalar. The decoder is a fair judge here only
--                      because the first direction pins it to the oracle
--                      independently. `make check-wire-vectors` closes the
--                      remaining gap by parsing Lua's bytes in Python.
--
-- Float values in the corpus are exactly representable. Subnormals and exact
-- ties belong to FL-16 Part 1 and to test/math_fallback_test.lua; asserting
-- them here would make this suite red for a defect it is not the one fixing.

local pb = require("protobuf")
local bit64 = require("bitn").bit64

-- Resolved from this file's own path so the suite runs both as
-- `lua test/wire_vectors_test.lua` and as the absolute dofile run_tests.sh issues.
local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"

local schema = dofile(here .. "generated/test_messages_proto3_schema.lua")
local vectors = dofile(here .. "generated/wire_vectors.lua")

local MESSAGE = "protobuf_test_messages.proto3.TestAllTypesProto3"
local root = schema.Message[MESSAGE]

local known_gaps = dofile(here .. "known_gaps.lua")
local testlib = dofile(here .. "testlib.lua")
local report = testlib.new("Wire vectors")

local version_dependent = 0
local expected_failures = 0

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

-- Forward declaration: a message field compares by recursing back into this.
local compare_message

--- Compares one field value, dispatching on the field's declared type.
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

--- Compares two map values by matching keys pairwise.
---
--- A `map<int64, ...>` decodes its keys to Int64 tables, so the destination is
--- keyed by table identity and a key cannot be looked up by value. Every map is
--- compared this way rather than only the 64-bit ones, so the comparison does
--- not depend on which key types happen to be lookupable.
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

--- Compares a decoded message against the expected table, driven by the schema.
---
--- Iterating the schema rather than either table is what makes a missing field
--- and an unexpected one both visible: a field absent from `want` but present
--- in `got` is a mismatch in the same way as the reverse.
function compare_message(messageSchema, got, want, path)
  for _, field in pairs(messageSchema.fields) do
    local got_value = got[field.name]
    local want_value = want[field.name]
    local where = path .. "." .. field.name

    if got_value == nil and want_value == nil then -- luacheck: ignore
      -- Absent on both sides, which is what a proto3 zero value looks like.
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

--- Runs one assertion, routing it through the known-gap lists.
local function assert_case(name, direction, ok, detail)
  report:count()
  local key = name .. " | " .. direction

  local unstable = known_gaps.version_dependent[key]
  if unstable then
    version_dependent = version_dependent + 1
    report:note(string.format("version dependent: %s (%s): %s", key, unstable, ok and "agrees here" or "differs here"))
    return
  end

  local gap = known_gaps.strict[key]
  if gap then
    if ok then
      report:record(string.format("%s: listed as a known gap (%s) but PASSED, remove the entry", key, gap))
    else
      expected_failures = expected_failures + 1
    end
    return
  end

  if not ok then
    report:record(string.format("%s: %s", key, detail or "failed"))
  end
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
    end
  end
end

report:finish(
  string.format(
    "%d assertions over %d vectors, %d known gaps, %d version dependent",
    report.checked,
    #vectors,
    expected_failures,
    version_dependent
  ),
  "reference and Lua agree on every vector"
)
