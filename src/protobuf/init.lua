--- A lightweight Protocol Buffers implementation for Lua.
--- This module provides encoding and decoding functions for Protocol Buffers data format.

local bitn = require("vendor.bitn")
local bit32 = bitn.bit32
local bit64 = bitn.bit64

--- Check if a value is a list (sequential table).
--- @param t any The value to check.
--- @return boolean is_list True if the value is a list.
local function IsList(t)
  if type(t) ~= "table" then
    return false
  end
  local count = 0
  for _ in pairs(t) do
    count = count + 1
  end
  for i = 1, count do
    if t[i] == nil then
      return false
    end
  end
  return count > 0
end

--- @class Protobuf
--- A class providing Protocol Buffers encoding and decoding functionality.
local Protobuf = {}

-- Version
local VERSION = "dev"

--- Returns the library version.
--- @return string version The version string.
function Protobuf.version()
  return VERSION
end

--- Encodes an integer into a varint byte sequence.
--- @param value number|boolean|Int64HighLow The value to encode. Can be a number, boolean, or {high, low} pair for 64-bit values.
--- @return string bytes The encoded varint byte sequence.
function Protobuf.encode_varint(value)
  if type(value) == "boolean" then
    value = value and 1 or 0
  end

  -- If value is a table, assume it's {high, low} format for 64-bit
  if type(value) == "table" then
    --- @cast value Int64HighLow
    local bytes = {}
    --- @type Int64HighLow
    local v = { value[1], value[2] } -- Copy the input

    repeat
      -- Extract low 7 bits
      local byte = v[2] % 128

      -- Right shift by 7 bits using bit64
      v = bit64.shr(v, 7)

      -- Set continue bit if more bytes remain
      if v[1] ~= 0 or v[2] ~= 0 then
        byte = byte + 0x80
      end
      table.insert(bytes, string.char(byte))
    until v[1] == 0 and v[2] == 0

    return table.concat(bytes)
  end

  -- For values that fit in 32 bits, use bit operations (fast path)
  if value >= 0 and value < 0x100000000 then
    local bytes = {}
    repeat
      local byte = bit32.band(value, 0x7F)
      value = bit32.rshift(value, 7)
      if value > 0 then
        byte = bit32.bor(byte, 0x80)
      end
      table.insert(bytes, string.char(byte))
    until value == 0
    return table.concat(bytes)
  end

  -- For large values (> 32 bits), convert to {high, low} and use bit64
  local low_32 = value % 0x100000000
  local high_32 = math.floor(value / 0x100000000)
  --- @type Int64HighLow
  local v = { high_32, low_32 }
  local bytes = {}

  repeat
    local byte = v[2] % 128
    v = bit64.shr(v, 7)
    if v[1] ~= 0 or v[2] ~= 0 then
      byte = byte + 0x80
    end
    table.insert(bytes, string.char(byte))
  until v[1] == 0 and v[2] == 0

  return table.concat(bytes)
end

--- Decodes a varint byte sequence into a {high, low} pair.
--- Always returns a Int64HighLow table for full 64-bit precision.
--- Use this for uint64/int64 fields that may exceed 53-bit precision.
--- @param buffer string The buffer containing the encoded varint.
--- @param pos integer The position in the buffer to start decoding from.
--- @return Int64HighLow value The decoded value as {high_32, low_32}.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_varint64(buffer, pos)
  local result = bit64.new(0, 0)
  local shift = 0
  local byte

  repeat
    byte = string.byte(buffer, pos)
    local value_bits = bit32.band(byte, 0x7F)

    -- Create a Int64 for this 7-bit chunk and shift it
    local chunk = bit64.new(0, value_bits)
    local shifted = bit64.lsl(chunk, shift)

    -- OR with result
    result = bit64.bor(result, shifted)

    shift = shift + 7
    pos = pos + 1
  until byte < 128

  return result, pos
end

--- Decodes a varint byte sequence into an integer.
--- Always returns a Lua number. Values exceeding 53-bit precision are truncated.
--- Use decode_varint64 for fields that need full 64-bit precision.
--- @param buffer string The buffer containing the encoded varint.
--- @param pos integer The position in the buffer to start decoding from.
--- @return integer value The decoded value as a number.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_varint(buffer, pos)
  local result, new_pos = Protobuf.decode_varint64(buffer, pos)
  return Protobuf.int64_to_number(result), new_pos
end

--- Encodes a 32-bit integer into a fixed-length 4-byte sequence.
--- @param value integer The 32-bit integer to encode.
--- @return string bytes The encoded 4-byte sequence.
function Protobuf.encode_fixed32(value)
  local b1 = value % 256
  local b2 = math.floor(value / 256) % 256
  local b3 = math.floor(value / 65536) % 256
  local b4 = math.floor(value / 16777216)
  return string.char(b1, b2, b3, b4)
end

--- Decodes a fixed-length 4-byte sequence into a 32-bit integer.
--- @param buffer string The buffer containing the encoded fixed32.
--- @param pos integer The position in the buffer to start decoding from.
--- @return integer value The decoded 32-bit integer value.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_fixed32(buffer, pos)
  local b1, b2, b3, b4 = string.byte(buffer, pos, pos + 3)
  local value = b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
  --- @cast value integer
  return value, pos + 4
end

--- Encodes a 64-bit integer into a fixed-length 8-byte sequence.
--- @param value Int64HighLow|number The 64-bit integer as {high, low} or number.
--- @return string bytes The encoded 8-byte sequence.
function Protobuf.encode_fixed64(value)
  local high, low
  if type(value) == "table" then
    high, low = value[1], value[2]
  else
    low = value % 0x100000000
    high = math.floor(value / 0x100000000)
  end
  local b1 = low % 256
  local b2 = math.floor(low / 256) % 256
  local b3 = math.floor(low / 65536) % 256
  local b4 = math.floor(low / 16777216) % 256
  local b5 = high % 256
  local b6 = math.floor(high / 256) % 256
  local b7 = math.floor(high / 65536) % 256
  local b8 = math.floor(high / 16777216) % 256
  return string.char(b1, b2, b3, b4, b5, b6, b7, b8)
end

--- Decodes a fixed-length 8-byte sequence into a 64-bit integer.
--- @param buffer string The buffer containing the encoded fixed64.
--- @param pos integer The position in the buffer to start decoding from.
--- @return Int64HighLow value The decoded 64-bit value as {high_32, low_32}.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_fixed64(buffer, pos)
  local b1, b2, b3, b4, b5, b6, b7, b8 = string.byte(buffer, pos, pos + 7)
  local low = b1 + b2 * 256 + b3 * 65536 + b4 * 16777216
  local high = b5 + b6 * 256 + b7 * 65536 + b8 * 16777216
  return { high, low }, pos + 8
end

--- Encodes a floating-point number into a 4-byte IEEE 754 single-precision format.
--- @param value number The floating-point number to encode.
--- @return string bytes The encoded 4-byte sequence.
function Protobuf.encode_float(value)
  if value == 0 then
    return string.char(0, 0, 0, 0)
  end

  local sign = 0
  if value < 0 then
    sign = 1
    value = -value
  end

  local mantissa, exponent = math.frexp(value)
  exponent = exponent - 1
  mantissa = mantissa * 2 - 1

  local e = exponent + 127
  if e < 0 then
    e = 0
    mantissa = 0
  elseif e > 255 then
    e = 255
    mantissa = 0
  end

  local m = math.floor(mantissa * 0x800000 + 0.5)

  local b1 = m % 256
  local b2 = math.floor(m / 256) % 256
  local b3 = bit32.bor(math.floor(m / 65536), bit32.lshift(e % 2, 7))
  local b4 = bit32.bor(bit32.rshift(e, 1), bit32.lshift(sign, 7))

  return string.char(b1, b2, b3, b4)
end

--- Decodes a 4-byte IEEE 754 single-precision format into a floating-point number.
--- @param buffer string The buffer containing the encoded float.
--- @param pos integer The position in the buffer to start decoding from.
--- @return number value The decoded floating-point value.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_float(buffer, pos)
  local b1, b2, b3, b4 = string.byte(buffer, pos, pos + 3)

  local sign = bit32.rshift(b4, 7)
  local e = bit32.lshift(bit32.band(b4, 0x7F), 1) + bit32.rshift(b3, 7)
  local m = bit32.band(b3, 0x7F) * 65536 + b2 * 256 + b1

  if e == 0 and m == 0 then
    return 0, pos + 4
  end

  local result = math.ldexp(1 + m / 0x800000, e - 127)
  if sign == 1 then
    result = -result
  end

  return result, pos + 4
end

--- Encodes a double-precision floating-point number into an 8-byte IEEE 754 format.
--- @param value number The double-precision floating-point number to encode.
--- @return string bytes The encoded 8-byte sequence.
function Protobuf.encode_double(value)
  if value == 0 then
    return string.char(0, 0, 0, 0, 0, 0, 0, 0)
  end

  local sign = 0
  if value < 0 then
    sign = 1
    value = -value
  end

  local mantissa, exponent = math.frexp(value)
  exponent = exponent - 1
  mantissa = mantissa * 2 - 1

  local e = exponent + 1023
  if e < 0 then
    e = 0
    mantissa = 0
  elseif e > 2047 then
    e = 2047
    mantissa = 0
  end

  -- Mantissa is 52 bits, split across bytes
  local m = mantissa * 0x10000000000000 -- 2^52
  local m_low = m % 0x100000000
  local m_high = math.floor(m / 0x100000000) % 0x100000 -- 20 bits

  local b1 = m_low % 256
  local b2 = math.floor(m_low / 256) % 256
  local b3 = math.floor(m_low / 65536) % 256
  local b4 = math.floor(m_low / 16777216) % 256
  local b5 = m_high % 256
  local b6 = math.floor(m_high / 256) % 256
  local b7 = bit32.bor(math.floor(m_high / 65536), bit32.lshift(e % 16, 4))
  local b8 = bit32.bor(bit32.rshift(e, 4), bit32.lshift(sign, 7))

  return string.char(b1, b2, b3, b4, b5, b6, b7, b8)
end

--- Decodes an 8-byte IEEE 754 double-precision format into a floating-point number.
--- @param buffer string The buffer containing the encoded double.
--- @param pos integer The position in the buffer to start decoding from.
--- @return number value The decoded double-precision floating-point value.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_double(buffer, pos)
  local b1, b2, b3, b4, b5, b6, b7, b8 = string.byte(buffer, pos, pos + 7)

  local sign = bit32.rshift(b8, 7)
  local e = bit32.lshift(bit32.band(b8, 0x7F), 4) + bit32.rshift(b7, 4)
  local m_high = bit32.band(b7, 0x0F) * 65536 + b6 * 256 + b5
  local m_low = b4 * 16777216 + b3 * 65536 + b2 * 256 + b1
  local m = m_high * 0x100000000 + m_low

  if e == 0 and m == 0 then
    return 0, pos + 8
  end

  local result = math.ldexp(1 + m / 0x10000000000000, e - 1023)
  if sign == 1 then
    result = -result
  end

  return result, pos + 8
end

-- ============================================================================
-- Zigzag Encoding (for sint32/sint64)
-- ============================================================================

--- Encodes a signed 32-bit integer using zigzag encoding.
--- @param value integer The signed integer to encode.
--- @return integer encoded The zigzag-encoded value.
function Protobuf.zigzag_encode32(value)
  return bit32.bxor(bit32.lshift(value, 1), bit32.arshift(value, 31))
end

--- Decodes a zigzag-encoded 32-bit integer.
--- @param value integer The zigzag-encoded value.
--- @return integer decoded The signed integer.
function Protobuf.zigzag_decode32(value)
  local result = bit32.bxor(bit32.rshift(value, 1), -bit32.band(value, 1))
  -- Convert unsigned to signed if high bit is set
  if result >= 0x80000000 then
    result = result - 0x100000000
  end
  return result
end

--- Encodes a signed 64-bit integer using zigzag encoding.
--- @param value Int64HighLow The signed 64-bit integer as {high, low}.
--- @return Int64HighLow encoded The zigzag-encoded value as {high, low}.
function Protobuf.zigzag_encode64(value)
  -- (n << 1) ^ (n >> 63)
  local shifted = bit64.lsl(value, 1)
  local sign_extended = bit64.asr(value, 63)
  return bit64.bxor(shifted, sign_extended)
end

--- Decodes a zigzag-encoded 64-bit integer.
--- @param value Int64HighLow The zigzag-encoded value as {high, low}.
--- @return Int64HighLow decoded The signed 64-bit integer as {high, low}.
function Protobuf.zigzag_decode64(value)
  -- (n >>> 1) ^ -(n & 1)
  local shifted = bit64.shr(value, 1)
  local sign_bit = { 0, bit32.band(value[2], 1) }
  -- Negate: if sign_bit is 1, result is {0xFFFFFFFF, 0xFFFFFFFF}, else {0, 0}
  local neg_sign
  if sign_bit[2] == 1 then
    neg_sign = { 0xFFFFFFFF, 0xFFFFFFFF }
  else
    neg_sign = { 0, 0 }
  end
  return bit64.bxor(shifted, neg_sign)
end

--- Encodes a length-delimited field (string or nested message).
--- @param data string The data to encode.
--- @return string bytes The encoded length-delimited data.
function Protobuf.encode_length_delimited(data)
  return Protobuf.encode_varint(#data) .. data
end

--- Decodes a length-delimited field.
--- @param buffer string The buffer containing the encoded length-delimited data.
--- @param pos integer The position in the buffer to start decoding from.
--- @return string data The decoded data.
--- @return integer new_pos The new position in the buffer after decoding.
function Protobuf.decode_length_delimited(buffer, pos)
  local length, new_pos = Protobuf.decode_varint(buffer, pos)
  local data = string.sub(buffer, new_pos, new_pos + length - 1)
  return data, new_pos + length
end

--- Encodes a message according to a schema.
--- @param protoSchema ProtoSchema The complete proto schema.
--- @param messageSchema ProtoMessageSchema The message schema to use for encoding.
--- @param message table<string, any> The message body to encode.
--- @return string buffer The encoded message.
function Protobuf.encode(protoSchema, messageSchema, message)
  local buffer = ""

  for field_number, field in pairs(messageSchema.fields) do
    local values = message[field.name]
    if values ~= nil then
      if field.repeated then
        if not IsList(values) or bit64.isInt64(values) then
          error("Field '" .. field.name .. "' is repeated but received a non-list value.")
        end
      else
        if IsList(values) and not bit64.isInt64(values) then
          error("Field '" .. field.name .. "' is not repeated but received a list.")
        end
        values = { values } -- Wrap single value in a list for uniform processing
      end
      for _, value in ipairs(values) do
        -- Compute the key (field number and wire type)
        local key = bit32.lshift(field_number, 3) + field.wireType
        buffer = buffer .. Protobuf.encode_varint(key)

        local fieldType = field.type
        if field.wireType == protoSchema.WireType.VARINT then
          -- Handle zigzag encoding for signed types
          if fieldType == protoSchema.DataType.SINT32 then
            buffer = buffer .. Protobuf.encode_varint(Protobuf.zigzag_encode32(value))
          elseif fieldType == protoSchema.DataType.SINT64 then
            buffer = buffer .. Protobuf.encode_varint(Protobuf.zigzag_encode64(value))
          else
            buffer = buffer .. Protobuf.encode_varint(value)
          end
        elseif field.wireType == protoSchema.WireType.FIXED64 then
          if fieldType == protoSchema.DataType.DOUBLE then
            buffer = buffer .. Protobuf.encode_double(value)
          else
            -- FIXED64, SFIXED64
            buffer = buffer .. Protobuf.encode_fixed64(value)
          end
        elseif field.wireType == protoSchema.WireType.FIXED32 then
          if fieldType == protoSchema.DataType.FLOAT then
            buffer = buffer .. Protobuf.encode_float(value)
          else
            -- FIXED32, SFIXED32
            buffer = buffer .. Protobuf.encode_fixed32(value)
          end
        elseif field.wireType == protoSchema.WireType.LENGTH_DELIMITED then
          if type(value) == "string" then
            buffer = buffer .. Protobuf.encode_length_delimited(value)
          elseif type(value) == "table" then
            if field.subschema == nil then
              error(
                "Field '"
                  .. messageSchema.name
                  .. "."
                  .. field.name
                  .. "' is a nested message but has no subschema defined."
              )
            end
            -- For nested messages
            local nested_message = Protobuf.encode(protoSchema, field.subschema, value)
            buffer = buffer .. Protobuf.encode_length_delimited(nested_message)
          end
        else
          error("Unsupported wire type: " .. tostring(field.wireType))
        end
      end
    end
  end

  return buffer
end

--- Decodes a message according to a schema.
--- @param protoSchema ProtoSchema The complete proto schema.
--- @param messageSchema ProtoMessageSchema The schema defining the message structure.
--- @param buffer string The encoded message bytes.
--- @return table<string, any> message The decoded message.
--- @return number pos The position in the buffer after decoding.
function Protobuf.decode(protoSchema, messageSchema, buffer)
  --- @type integer
  local pos = 1
  local message = {}

  local key
  while pos <= #buffer do
    -- Decode the key (field number and wire type)
    key, pos = Protobuf.decode_varint(buffer, pos)
    local field_number = bit32.rshift(key, 3)
    local wire_type = bit32.band(key, 0x7)

    -- Find the corresponding field in the schema
    local field = messageSchema.fields[field_number]
    if not field then
      -- Skip unknown field based on wire type
      if wire_type == protoSchema.WireType.VARINT then
        -- Decode and discard the varint
        local _
        _, pos = Protobuf.decode_varint(buffer, pos)
      elseif wire_type == protoSchema.WireType.FIXED64 then
        -- Skip 8 bytes
        pos = pos + 8
      elseif wire_type == protoSchema.WireType.FIXED32 then
        -- Skip 4 bytes
        pos = pos + 4
      elseif wire_type == protoSchema.WireType.LENGTH_DELIMITED then
        -- Decode length and skip that many bytes
        local length
        length, pos = Protobuf.decode_varint(buffer, pos)
        pos = pos + length
      else
        error("Unknown wire type: " .. wire_type)
      end
    else
      -- Known field - decode and store the value
      local value
      local fieldType = field.type
      -- Decode the value based on the wire type
      if wire_type == protoSchema.WireType.VARINT then
        -- Use 64-bit decoder for types that need full precision
        if fieldType == protoSchema.DataType.UINT64 or fieldType == protoSchema.DataType.INT64 then
          value, pos = Protobuf.decode_varint64(buffer, pos)
        elseif fieldType == protoSchema.DataType.SINT64 then
          local raw
          raw, pos = Protobuf.decode_varint64(buffer, pos)
          value = Protobuf.zigzag_decode64(raw)
        elseif fieldType == protoSchema.DataType.SINT32 then
          local raw
          raw, pos = Protobuf.decode_varint(buffer, pos)
          value = Protobuf.zigzag_decode32(raw)
        elseif fieldType == protoSchema.DataType.BOOL then
          value, pos = Protobuf.decode_varint(buffer, pos)
          value = value ~= 0 -- Convert to boolean
        else
          -- INT32, UINT32, ENUM, etc.
          value, pos = Protobuf.decode_varint(buffer, pos)
        end
      elseif wire_type == protoSchema.WireType.FIXED64 then
        if fieldType == protoSchema.DataType.DOUBLE then
          value, pos = Protobuf.decode_double(buffer, pos)
        else
          -- FIXED64, SFIXED64
          value, pos = Protobuf.decode_fixed64(buffer, pos)
        end
      elseif wire_type == protoSchema.WireType.FIXED32 then
        if fieldType == protoSchema.DataType.FLOAT then
          value, pos = Protobuf.decode_float(buffer, pos)
        else
          -- FIXED32, SFIXED32
          value, pos = Protobuf.decode_fixed32(buffer, pos)
        end
      elseif wire_type == protoSchema.WireType.LENGTH_DELIMITED then
        local data
        data, pos = Protobuf.decode_length_delimited(buffer, pos)
        if field.subschema then
          value = Protobuf.decode(protoSchema, protoSchema.Message[field.subschema], data)
        else
          value = data
        end
      else
        error("Unsupported wire type: " .. wire_type)
      end

      if field.repeated then
        if message[field.name] == nil then
          message[field.name] = {}
        end
        table.insert(message[field.name], value)
      else
        message[field.name] = value
      end
    end
  end

  return message, pos
end

-- ============================================================================
-- 64-bit {high, low} Utility Functions
-- ============================================================================

--- Converts a {high, low} pair to a hexadecimal string.
--- @param value Int64HighLow The {high_32, low_32} pair.
--- @return string hex The 16-character hexadecimal string (e.g., "0000180000001000").
function Protobuf.int64_to_hex(value)
  return bit64.to_hex(value)
end

--- Converts a {high, low} pair to a Lua number.
--- Warning: Values exceeding 53-bit precision will lose precision.
--- @param value Int64HighLow The {high_32, low_32} pair.
--- @param strict? boolean If true, errors when value exceeds 53-bit precision.
--- @return number result The value as a Lua number (may lose precision for large values unless strict).
function Protobuf.int64_to_number(value, strict)
  return bit64.to_number(value, strict)
end

--- Creates a {high, low} pair from a Lua number.
--- @param value number The number to convert.
--- @return Int64HighLow pair The {high_32, low_32} pair.
function Protobuf.int64_from_number(value)
  return bit64.from_number(value)
end

--- Checks if two {high, low} pairs are equal.
--- @param a Int64HighLow The first {high_32, low_32} pair.
--- @param b Int64HighLow The second {high_32, low_32} pair.
--- @return boolean equal True if the values are equal.
function Protobuf.int64_equals(a, b)
  return bit64.eq(a, b)
end

--- Checks if a {high, low} pair is zero.
--- @param value Int64HighLow The {high_32, low_32} pair.
--- @return boolean is_zero True if the value is zero.
function Protobuf.int64_is_zero(value)
  return bit64.is_zero(value)
end

--- Runs self-tests to verify the functionality of the Protobuf module.
--- Test vectors based on official Protocol Buffers encoding specification.
--- @see https://protobuf.dev/programming-guides/encoding/
--- @return boolean success True if all tests passed.
function Protobuf.selftest()
  print("Running Protobuf test vectors...")
  local passed = 0
  local failed = 0

  --- Helper to convert string to hex for display
  local function to_hex(s)
    local hex = {}
    for i = 1, #s do
      table.insert(hex, string.format("%02X", string.byte(s, i)))
    end
    return table.concat(hex, " ")
  end

  --- Assert helper
  local function assert_eq(actual, expected, msg)
    if actual == expected then
      passed = passed + 1
      print("  PASS: " .. msg)
      return true
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual))
      return false
    end
  end

  --- Assert helper for bytes
  local function assert_bytes(actual, expected_hex, msg)
    local expected = ""
    for byte in expected_hex:gmatch("%x%x") do
      expected = expected .. string.char(tonumber(byte, 16) or 0)
    end
    if actual == expected then
      passed = passed + 1
      print("  PASS: " .. msg)
      return true
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected " .. expected_hex .. ", got " .. to_hex(actual))
      return false
    end
  end

  --- Assert helper for floats
  local function assert_close(actual, expected, epsilon, msg)
    if math.abs(actual - expected) <= epsilon then
      passed = passed + 1
      print("  PASS: " .. msg)
      return true
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual))
      return false
    end
  end

  --- Assert helper for {high, low} pairs
  local function assert_int64_eq(actual, expected_high, expected_low, msg)
    if type(actual) == "table" and actual[1] == expected_high and actual[2] == expected_low then
      passed = passed + 1
      print("  PASS: " .. msg)
      return true
    else
      failed = failed + 1
      local actual_str = type(actual) == "table" and string.format("{%d, %d}", actual[1], actual[2]) or tostring(actual)
      print("  FAIL: " .. msg .. ": expected {" .. expected_high .. ", " .. expected_low .. "}, got " .. actual_str)
      return false
    end
  end

  -- ==========================================
  -- VARINT ENCODING TESTS (from official spec)
  -- ==========================================

  -- Test: 0 -> 0x00
  assert_bytes(Protobuf.encode_varint(0), "00", "varint(0)")

  -- Test: 1 -> 0x01
  assert_bytes(Protobuf.encode_varint(1), "01", "varint(1)")

  -- Test: 127 -> 0x7F (max single byte)
  assert_bytes(Protobuf.encode_varint(127), "7F", "varint(127)")

  -- Test: 128 -> 0x80 0x01 (first two-byte value)
  assert_bytes(Protobuf.encode_varint(128), "8001", "varint(128)")

  -- Test: 150 -> 0x96 0x01 (from official docs)
  assert_bytes(Protobuf.encode_varint(150), "9601", "varint(150)")

  -- Test: 300 -> 0xAC 0x02
  assert_bytes(Protobuf.encode_varint(300), "AC02", "varint(300)")

  -- Test: 16383 -> 0xFF 0x7F (max two-byte)
  assert_bytes(Protobuf.encode_varint(16383), "FF7F", "varint(16383)")

  -- Test: 16384 -> 0x80 0x80 0x01 (first three-byte)
  assert_bytes(Protobuf.encode_varint(16384), "808001", "varint(16384)")

  -- Test roundtrip for various values
  for _, v in ipairs({ 0, 1, 127, 128, 150, 300, 16383, 16384, 65535, 2097151, 268435455 }) do
    local enc = Protobuf.encode_varint(v)
    local dec, _ = Protobuf.decode_varint(enc, 1)
    assert_eq(dec, v, "varint roundtrip " .. v)
  end

  -- ==========================================
  -- VARINT64 TESTS
  -- ==========================================

  -- Test: Large value that fits in 53 bits
  local large_val = 2 ^ 40 + 12345
  local enc64 = Protobuf.encode_varint(large_val)
  local dec64, _ = Protobuf.decode_varint(enc64, 1)
  assert_eq(dec64, large_val, "varint64 roundtrip 2^40+12345")

  -- Test: decode_varint64 always returns {high, low}
  local result64, _ = Protobuf.decode_varint64(Protobuf.encode_varint(150), 1)
  assert_int64_eq(result64, 0, 150, "decode_varint64(150)")

  -- Test: {high, low} encoding
  local hl_val = { 0x12345678, 0x9ABCDEF0 }
  local hl_enc = Protobuf.encode_varint(hl_val)
  local hl_dec, _ = Protobuf.decode_varint64(hl_enc, 1)
  assert_int64_eq(hl_dec, hl_val[1], hl_val[2], "varint {high,low} roundtrip")

  -- ==========================================
  -- FIXED32 TESTS
  -- ==========================================

  -- Test: 0 -> 00 00 00 00
  assert_bytes(Protobuf.encode_fixed32(0), "00000000", "fixed32(0)")

  -- Test: 1 -> 01 00 00 00 (little-endian)
  assert_bytes(Protobuf.encode_fixed32(1), "01000000", "fixed32(1)")

  -- Test: 12345 -> 39 30 00 00 (little-endian: 0x3039)
  assert_bytes(Protobuf.encode_fixed32(12345), "39300000", "fixed32(12345)")

  -- Test: max uint32 -> FF FF FF FF
  assert_bytes(Protobuf.encode_fixed32(0xFFFFFFFF), "FFFFFFFF", "fixed32(max)")

  -- Roundtrip
  for _, v in ipairs({ 0, 1, 12345, 0xFFFFFFFF, 1234567890 }) do
    local enc = Protobuf.encode_fixed32(v)
    local dec, _ = Protobuf.decode_fixed32(enc, 1)
    assert_eq(dec, v, "fixed32 roundtrip " .. v)
  end

  -- ==========================================
  -- FIXED64 TESTS
  -- ==========================================

  -- Test: 0 -> 00 00 00 00 00 00 00 00
  assert_bytes(Protobuf.encode_fixed64({ 0, 0 }), "0000000000000000", "fixed64(0)")

  -- Test: 1 -> 01 00 00 00 00 00 00 00
  assert_bytes(Protobuf.encode_fixed64({ 0, 1 }), "0100000000000000", "fixed64(1)")

  -- Roundtrip with {high, low}
  local test_vals = {
    { 0, 0 },
    { 0, 1 },
    { 0, 0xFFFFFFFF },
    { 1, 0 },
    { 0xFFFFFFFF, 0xFFFFFFFF },
    { 0x12345678, 0x9ABCDEF0 },
  }
  for _, v in ipairs(test_vals) do
    local enc = Protobuf.encode_fixed64(v)
    local dec, _ = Protobuf.decode_fixed64(enc, 1)
    assert_int64_eq(dec, v[1], v[2], string.format("fixed64 roundtrip {0x%X, 0x%X}", v[1], v[2]))
  end

  -- ==========================================
  -- FLOAT TESTS
  -- ==========================================

  -- Test: 0.0 -> 00 00 00 00
  assert_bytes(Protobuf.encode_float(0), "00000000", "float(0)")

  -- Test: 1.0 -> 00 00 80 3F (IEEE 754: 0x3F800000)
  assert_bytes(Protobuf.encode_float(1.0), "0000803F", "float(1.0)")

  -- Roundtrip
  for _, v in ipairs({ 0.0, 1.0, -1.0, 3.14159, 100.5, -1234.5678 }) do
    local enc = Protobuf.encode_float(v)
    local dec, _ = Protobuf.decode_float(enc, 1)
    assert_close(dec, v, 1e-4, "float roundtrip " .. v)
  end

  -- ==========================================
  -- DOUBLE TESTS
  -- ==========================================

  -- Test: 0.0 -> 00 00 00 00 00 00 00 00
  assert_bytes(Protobuf.encode_double(0), "0000000000000000", "double(0)")

  -- Test: 1.0 -> 00 00 00 00 00 00 F0 3F (IEEE 754: 0x3FF0000000000000)
  assert_bytes(Protobuf.encode_double(1.0), "000000000000F03F", "double(1.0)")

  -- Roundtrip
  for _, v in ipairs({ 0.0, 1.0, -1.0, 3.141592653589793, 1e100, -1e-100 }) do
    local enc = Protobuf.encode_double(v)
    local dec, _ = Protobuf.decode_double(enc, 1)
    assert_close(dec, v, 1e-10, "double roundtrip " .. v)
  end

  -- ==========================================
  -- ZIGZAG TESTS (from official spec)
  -- ==========================================

  -- Official test vectors from protobuf spec
  local zigzag32_tests = {
    { 0, 0 },
    { -1, 1 },
    { 1, 2 },
    { -2, 3 },
    { 2147483647, 4294967294 }, -- 0x7FFFFFFF -> 0xFFFFFFFE
    { -2147483648, 4294967295 }, -- 0x80000000 -> 0xFFFFFFFF
  }
  for _, test in ipairs(zigzag32_tests) do
    local input, expected = test[1], test[2]
    local actual = Protobuf.zigzag_encode32(input)
    -- Handle signed result from bit.bxor
    if actual < 0 then
      actual = actual + 0x100000000
    end
    assert_eq(actual, expected, string.format("zigzag_encode32(%d)", input))
  end

  -- Roundtrip zigzag32
  for _, v in ipairs({ 0, 1, -1, 100, -100, 2147483647, -2147483648 }) do
    local enc = Protobuf.zigzag_encode32(v)
    local dec = Protobuf.zigzag_decode32(enc)
    assert_eq(dec, v, "zigzag32 roundtrip " .. v)
  end

  -- Roundtrip zigzag64
  local zigzag64_tests = {
    { { 0, 0 }, { 0, 0 } }, -- 0 -> 0
    { { 0xFFFFFFFF, 0xFFFFFFFF }, { 0, 1 } }, -- -1 -> 1
    { { 0, 1 }, { 0, 2 } }, -- 1 -> 2
  }
  for _, test in ipairs(zigzag64_tests) do
    local input, expected = test[1], test[2]
    local actual = Protobuf.zigzag_encode64(input)
    assert_int64_eq(actual, expected[1], expected[2], string.format("zigzag_encode64({%d,%d})", input[1], input[2]))
  end

  -- ==========================================
  -- LENGTH-DELIMITED TESTS
  -- ==========================================

  -- Test: "testing" from official docs (12 07 74 65 73 74 69 6e 67)
  local test_str = "testing"
  local enc_str = Protobuf.encode_length_delimited(test_str)
  assert_bytes(enc_str, "0774657374696E67", "length_delimited('testing')")

  -- Roundtrip
  local dec_str, _ = Protobuf.decode_length_delimited(enc_str, 1)
  assert_eq(dec_str, test_str, "length_delimited roundtrip")

  -- ==========================================
  -- INT64 UTILITY TESTS
  -- ==========================================

  -- to_hex
  local hex = Protobuf.int64_to_hex({ 0x12345678, 0x9ABCDEF0 })
  assert_eq(hex, "123456789ABCDEF0", "int64_to_hex")

  -- to_number / from_number roundtrip
  local num = 123456789012345
  local hl = Protobuf.int64_from_number(num)
  local back = Protobuf.int64_to_number(hl)
  assert_eq(back, num, "int64_from_number/to_number roundtrip")

  -- equals
  assert_eq(Protobuf.int64_equals({ 1, 2 }, { 1, 2 }), true, "int64_equals same")
  assert_eq(Protobuf.int64_equals({ 1, 2 }, { 1, 3 }), false, "int64_equals diff")

  -- is_zero
  assert_eq(Protobuf.int64_is_zero({ 0, 0 }), true, "int64_is_zero true")
  assert_eq(Protobuf.int64_is_zero({ 0, 1 }), false, "int64_is_zero false")

  -- ==========================================
  -- INT64 METATABLE MARKER TESTS
  -- ==========================================

  -- Test: decode_varint64 returns marked Int64 values
  local decoded_int64, _ = Protobuf.decode_varint64(Protobuf.encode_varint(12345), 1)
  assert_eq(bit64.isInt64(decoded_int64), true, "decode_varint64 returns marked Int64")

  -- Test: int64_from_number returns marked Int64 values
  local from_num = Protobuf.int64_from_number(9876543210)
  assert_eq(bit64.isInt64(from_num), true, "int64_from_number returns marked Int64")

  -- Test: IsList correctly distinguishes Int64 from arrays
  local int64_val = bit64.new(0, 42)
  local array_val = { 1, 2 }
  assert_eq(IsList(int64_val), true, "IsList sees Int64 as list-like (2 elements)")
  assert_eq(bit64.isInt64(int64_val), true, "bit64.isInt64 identifies Int64")
  assert_eq(bit64.isInt64(array_val), false, "bit64.isInt64 rejects plain array")

  -- Test: Encode with uint64 field using Int64 value doesn't error
  -- Create a minimal schema for testing
  local testSchema = {
    WireType = { VARINT = 0, FIXED64 = 1, LENGTH_DELIMITED = 2, FIXED32 = 5 },
    DataType = { UINT64 = 4 },
    Message = {},
  }
  local testMessageSchema = {
    name = "TestMessage",
    fields = {
      [1] = {
        name = "address",
        type = testSchema.DataType.UINT64,
        wireType = testSchema.WireType.VARINT,
        repeated = false,
      },
    },
  }

  -- This should NOT error - Int64 values should not be treated as repeated fields
  local success, err = pcall(function()
    local int64_address = bit64.new(0, 0x12345678)
    Protobuf.encode(testSchema, testMessageSchema, { address = int64_address })
  end)
  if success then
    passed = passed + 1
    print("  PASS: encode with Int64 uint64 field")
  else
    failed = failed + 1
    print("  FAIL: encode with Int64 uint64 field: " .. tostring(err))
  end

  -- Test: Encode/decode roundtrip with uint64 field
  local original_address = bit64.new(0x00001800, 0x00001000)
  local encoded = Protobuf.encode(testSchema, testMessageSchema, { address = original_address })
  local decoded, _ = Protobuf.decode(testSchema, testMessageSchema, encoded)
  assert_int64_eq(decoded.address, original_address[1], original_address[2], "uint64 field encode/decode roundtrip")

  -- ==========================================
  -- SUMMARY
  -- ==========================================
  print(string.format("\nProtobuf operations: %d/%d tests passed\n", passed, passed + failed))
  return failed == 0
end

return Protobuf
