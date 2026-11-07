--- Tests for protobuf: `make test` runs them, and so does the with-tests build's selftest().

local pb = require("protobuf")
local bitn = require("bitn")
local bit64 = bitn.bit64
local bit64_is_int64 = bit64.is_int64
local bit64_new = bit64.new
local INF = math.huge
local math_ldexp = pb._math.ldexp
local NAN = 0 / 0
-- Lua 5.1 constant-folds the literal -0.0 to +0.0, and LuaJIT does the same to
-- 0.0 * -1. Dividing into an infinity is the one form that survives everywhere.
local NEG_ZERO = -1 / INF

--- Runs self-tests to verify the functionality of the protobuf module.
--- Test vectors based on official Protocol Buffers encoding specification.
--- @see https://protobuf.dev/programming-guides/encoding/
--- @return boolean success True if all tests passed.
local function selftest()
  print("Running protobuf test vectors...")
  local passed = 0
  local failed = 0

  -- ============================================================================
  -- TEST HELPERS
  -- ============================================================================

  local function to_hex(s)
    local hex = {}
    for i = 1, #s do
      table.insert(hex, string.format("%02X", string.byte(s, i)))
    end
    return table.concat(hex, " ")
  end

  local function assert_eq(actual, expected, msg)
    if actual == expected then
      passed = passed + 1
      print("  PASS: " .. msg)
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual))
    end
  end

  local function from_hex(hex)
    local bytes = ""
    for byte in hex:gmatch("%x%x") do
      bytes = bytes .. string.char(tonumber(byte, 16) or 0)
    end
    -- gmatch skips anything that is not a hex pair, so a typo silently yields a
    -- short buffer, which the decoders then read past the end of.
    assert(#bytes * 2 == #hex, "malformed hex literal: " .. hex)
    return bytes
  end

  local function assert_bytes(actual, expected_hex, msg)
    local expected = from_hex(expected_hex)
    if actual == expected then
      passed = passed + 1
      print("  PASS: " .. msg)
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected " .. expected_hex .. ", got " .. to_hex(actual))
    end
  end

  local function assert_close(actual, expected, epsilon, msg)
    if math.abs(actual - expected) <= epsilon then
      passed = passed + 1
      print("  PASS: " .. msg)
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected " .. tostring(expected) .. ", got " .. tostring(actual))
    end
  end

  local function assert_nan(actual, msg)
    if type(actual) == "number" and actual ~= actual then
      passed = passed + 1
      print("  PASS: " .. msg)
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. ": expected NaN, got " .. tostring(actual))
    end
  end

  local function assert_int64(actual, high, low, msg)
    if type(actual) == "table" and actual[1] == high and actual[2] == low then
      passed = passed + 1
      print("  PASS: " .. msg)
    else
      failed = failed + 1
      local actual_str = type(actual) == "table" and string.format("{0x%X, 0x%X}", actual[1], actual[2])
        or tostring(actual)
      print("  FAIL: " .. msg .. string.format(": expected {0x%X, 0x%X}, got %s", high, low, actual_str))
    end
  end

  local function assert_error(fn, pattern, msg)
    local ok, err = pcall(fn)
    if not ok and type(err) == "string" and string.find(err, pattern) then
      passed = passed + 1
      print("  PASS: " .. msg)
    else
      failed = failed + 1
      print("  FAIL: " .. msg .. (ok and " (no error thrown)" or ": " .. tostring(err)))
    end
  end

  -- ============================================================================
  -- TEST SCHEMA (shared across encode/decode tests)
  -- ============================================================================

  --- @type ProtoSchema
  local Schema = {
    WireType = { VARINT = 0, FIXED64 = 1, LENGTH_DELIMITED = 2, FIXED32 = 5 },
    DataType = {
      DOUBLE = 1,
      FLOAT = 2,
      INT64 = 3,
      UINT64 = 4,
      INT32 = 5,
      FIXED64 = 6,
      FIXED32 = 7,
      BOOL = 8,
      STRING = 9,
      MESSAGE = 11,
      BYTES = 12,
      UINT32 = 13,
      ENUM = 14,
      SFIXED32 = 15,
      SFIXED64 = 16,
      SINT32 = 17,
      SINT64 = 18,
    },
    Enum = {},
    Message = {},
    RPC = {},
  }

  -- Helper to create simple single-field message schemas
  local function make_schema(name, field_name, data_type, wire_type, opts)
    opts = opts or {}
    return {
      name = name,
      options = {},
      fields = {
        [1] = {
          name = field_name,
          type = data_type,
          wireType = wire_type,
          repeated = opts.repeated,
          map = opts.map,
          subschema = opts.subschema,
        },
      },
    }
  end

  -- ============================================================================
  -- VERSION
  -- ============================================================================

  assert_eq(type(pb.version()), "string", "version() returns string")

  -- ============================================================================
  -- VARINT ENCODING (official protobuf spec test vectors)
  -- ============================================================================

  local varint_vectors = {
    { 0, "00" },
    { 1, "01" },
    { 127, "7F" },
    { 128, "8001" },
    { 150, "9601" }, -- from official docs
    { 300, "AC02" },
    { 16383, "FF7F" }, -- max 2-byte
    { 16384, "808001" }, -- first 3-byte
  }
  for _, t in ipairs(varint_vectors) do
    assert_bytes(pb.encode_varint(t[1]), t[2], "varint encode " .. t[1])
  end

  -- Varint roundtrip
  for _, v in ipairs({ 0, 1, 127, 128, 150, 300, 16383, 16384, 65535, 2097151, 268435455, 2 ^ 40 + 12345 }) do
    local enc = pb.encode_varint(v)
    local dec = pb.decode_varint(enc, 1)
    assert_eq(dec, v, "varint roundtrip " .. v)
  end

  -- Varint64 with Int64 values
  local v64 = bit64_new(0x12345678, 0x9ABCDEF0)
  local enc64 = pb.encode_varint(v64)
  local dec64 = pb.decode_varint64(enc64, 1)
  assert_int64(dec64, v64[1], v64[2], "varint64 Int64 roundtrip")
  assert_eq(bit64_is_int64(dec64), true, "decode_varint64 returns marked Int64")

  -- Varint 32-bit boundary values (tests raw bit op signed/unsigned handling)
  for _, v in ipairs({ 0x7FFFFFFF, 0x80000000, 0xFFFFFFFF }) do
    local enc = pb.encode_varint(v)
    local dec = pb.decode_varint(enc, 1)
    assert_eq(dec, v, string.format("varint 0x%X roundtrip", v))
  end

  -- Varint64 with high bits set (tests raw 64-bit op handling)
  local high_bit_values = {
    { 0x80000000, 0x00000000 }, -- bit 63 set
    { 0x80000000, 0x00000001 }, -- bit 63 + low bit
    { 0xFFFFFFFF, 0x00000000 }, -- high word all 1s
    { 0x00000000, 0x80000000 }, -- bit 31 set
  }
  for _, v in ipairs(high_bit_values) do
    local val = bit64_new(v[1], v[2])
    local enc = pb.encode_varint(val)
    local dec = pb.decode_varint64(enc, 1)
    assert_int64(dec, v[1], v[2], string.format("varint64 {0x%X, 0x%X} roundtrip", v[1], v[2]))
  end

  -- ============================================================================
  -- FIXED32 ENCODING
  -- ============================================================================

  local fixed32_vectors = {
    { 0, "00000000" },
    { 1, "01000000" },
    { 12345, "39300000" },
    { 0xFFFFFFFF, "FFFFFFFF" },
  }
  for _, t in ipairs(fixed32_vectors) do
    assert_bytes(pb.encode_fixed32(t[1]), t[2], "fixed32 encode " .. t[1])
    local dec = pb.decode_fixed32(pb.encode_fixed32(t[1]), 1)
    assert_eq(dec, t[1], "fixed32 roundtrip " .. t[1])
  end

  -- ============================================================================
  -- FIXED64 ENCODING
  -- ============================================================================

  local fixed64_vectors = {
    { bit64_new(0, 0), "0000000000000000" },
    { bit64_new(0, 1), "0100000000000000" },
    { bit64_new(0, 0xFFFFFFFF), "FFFFFFFF00000000" },
    { bit64_new(1, 0), "0000000001000000" },
    { bit64_new(0xFFFFFFFF, 0xFFFFFFFF), "FFFFFFFFFFFFFFFF" },
  }
  for _, t in ipairs(fixed64_vectors) do
    assert_bytes(pb.encode_fixed64(t[1]), t[2], string.format("fixed64 encode {0x%X, 0x%X}", t[1][1], t[1][2]))
    local dec = pb.decode_fixed64(pb.encode_fixed64(t[1]), 1)
    assert_int64(dec, t[1][1], t[1][2], string.format("fixed64 roundtrip {0x%X, 0x%X}", t[1][1], t[1][2]))
  end

  -- ============================================================================
  -- FLOAT ENCODING
  -- ============================================================================

  assert_bytes(pb.encode_float(0), "00000000", "float encode 0")
  assert_bytes(pb.encode_float(1.0), "0000803F", "float encode 1.0")

  for _, v in ipairs({ 0.0, 1.0, -1.0, 3.14159, 100.5, -1234.5678, 1e10, -1e-10 }) do
    local dec = pb.decode_float(pb.encode_float(v), 1)
    assert_close(dec, v, 1e-4, "float roundtrip " .. v)
  end

  assert_bytes(pb.encode_float(NAN), "0000C07F", "float encode NaN")
  assert_bytes(pb.encode_float(math.huge), "0000807F", "float encode +infinity")
  assert_bytes(pb.encode_float(-math.huge), "000080FF", "float encode -infinity")

  -- Decoded from the canonical wire patterns rather than from this encoder's own
  -- output, so the assertions still hold if both sides break together. The
  -- signalling and non-canonical mantissas are the ones a real producer varies.
  assert_nan(pb.decode_float(from_hex("0000C07F"), 1), "float decode quiet NaN")
  assert_nan(pb.decode_float(from_hex("0100807F"), 1), "float decode signalling NaN")
  assert_nan(pb.decode_float(from_hex("FFFFFFFF"), 1), "float decode negative NaN")
  assert_eq(pb.decode_float(from_hex("0000807F"), 1), math.huge, "float decode +infinity")
  assert_eq(pb.decode_float(from_hex("000080FF"), 1), -math.huge, "float decode -infinity")

  -- The largest finite float still decodes finite: the guard keys on the
  -- all-ones exponent, not on magnitude.
  assert_close(pb.decode_float(from_hex("FFFF7F7F"), 1), 3.4028234663853e38, 1e30, "float decode max finite")

  -- A finite double too large for a float saturates to infinity. Expectations
  -- come from a C (float) cast, not from this encoder.
  assert_bytes(pb.encode_float(3.5e38), "0000807F", "float encode just over max to +infinity")
  assert_bytes(pb.encode_float(-3.5e38), "000080FF", "float encode just under min to -infinity")
  assert_bytes(pb.encode_float(1e39), "0000807F", "float encode far over max to +infinity")
  -- Below the midpoint to 2^128 it rounds down to the largest finite float
  -- instead, so the saturation above is not simply "big magnitude wins".
  assert_bytes(pb.encode_float(3.40282349e38), "FFFF7F7F", "float encode under midpoint to max finite")

  -- Mantissa rounding that carries into the exponent. The odd-exponent cases are
  -- the ones where the carry bit collides with the exponent's low bit.
  assert_bytes(pb.encode_float(2 - 2 ^ -25), "00000040", "float encode carry into odd exponent")
  assert_bytes(pb.encode_float(32 * (1 - 2 ^ -25)), "00000042", "float encode carry into odd exponent, larger")
  assert_bytes(pb.encode_float(4 - 2 ^ -24), "00008040", "float encode carry into even exponent")

  -- Subnormals, where there is no implicit leading one and the exponent is pinned.
  -- Expectations from a C (float) cast; test/float_vectors_test.lua sweeps the band.
  assert_bytes(pb.encode_float(2 ^ -149), "01000000", "float encode smallest subnormal")
  assert_bytes(pb.encode_float(2 ^ -126 - 2 ^ -149), "FFFF7F00", "float encode largest subnormal")
  assert_eq(pb.decode_float(from_hex("01000000"), 1), 2 ^ -149, "float decode smallest subnormal")
  assert_eq(pb.decode_float(from_hex("FFFF7F00"), 1), 2 ^ -126 - 2 ^ -149, "float decode largest subnormal")

  -- Exact midpoints, which IEEE 754 breaks toward the even mantissa rather than
  -- away from zero. Half of these go to the wrong neighbour under floor(x + 0.5).
  assert_bytes(pb.encode_float(2 ^ -150), "00000000", "float encode subnormal tie down to zero")
  assert_bytes(pb.encode_float(3 * 2 ^ -150), "02000000", "float encode subnormal tie up")
  assert_bytes(pb.encode_float((2 ^ 24 - 1) * 2 ^ -150), "00008000", "float encode tie carrying to normal")
  assert_bytes(pb.encode_float(1 + 2 ^ -24), "0000803F", "float encode normal tie down")
  assert_bytes(pb.encode_float(1 + 3 * 2 ^ -24), "0200803F", "float encode normal tie up")

  assert_bytes(pb.encode_float(NEG_ZERO), "00000080", "float encode negative zero")
  assert_bytes(pb.encode_float(0), "00000000", "float encode positive zero")
  local neg_zero_f = pb.decode_float(from_hex("00000080"), 1)
  assert_eq(1 / neg_zero_f, -INF, "float decode negative zero")
  assert_eq(1 / pb.decode_float(from_hex("00000000"), 1), INF, "float decode positive zero")

  -- ============================================================================
  -- DOUBLE ENCODING
  -- ============================================================================

  assert_bytes(pb.encode_double(0), "0000000000000000", "double encode 0")
  assert_bytes(pb.encode_double(1.0), "000000000000F03F", "double encode 1.0")

  for _, v in ipairs({ 0.0, 1.0, -1.0, 3.141592653589793, 1e100, -1e-100 }) do
    local dec = pb.decode_double(pb.encode_double(v), 1)
    assert_close(dec, v, 1e-10, "double roundtrip " .. v)
  end

  assert_bytes(pb.encode_double(NAN), "000000000000F87F", "double encode NaN")
  assert_bytes(pb.encode_double(math.huge), "000000000000F07F", "double encode +infinity")
  assert_bytes(pb.encode_double(-math.huge), "000000000000F0FF", "double encode -infinity")

  assert_nan(pb.decode_double(from_hex("000000000000F87F"), 1), "double decode quiet NaN")
  assert_nan(pb.decode_double(from_hex("010000000000F07F"), 1), "double decode signalling NaN")
  assert_nan(pb.decode_double(from_hex("FFFFFFFFFFFFFFFF"), 1), "double decode negative NaN")
  assert_eq(pb.decode_double(from_hex("000000000000F07F"), 1), math.huge, "double decode +infinity")
  assert_eq(pb.decode_double(from_hex("000000000000F0FF"), 1), -math.huge, "double decode -infinity")

  -- Subnormals, as in the float section. math_ldexp rather than 2 ^ -1074: LuaJIT 2.0
  -- returns zero for a power of two below the normal range, and it is in the matrix.
  local smallest_double = math_ldexp(1.0, -1074)
  local largest_subnormal_double = math_ldexp(2 ^ 52 - 1, -1074)
  assert_bytes(pb.encode_double(smallest_double), "0100000000000000", "double encode smallest subnormal")
  assert_bytes(pb.encode_double(largest_subnormal_double), "FFFFFFFFFFFF0F00", "double encode largest subnormal")
  assert_eq(pb.decode_double(from_hex("0100000000000000"), 1), smallest_double, "double decode smallest subnormal")
  assert_eq(
    pb.decode_double(from_hex("FFFFFFFFFFFF0F00"), 1),
    largest_subnormal_double,
    "double decode largest subnormal"
  )

  assert_bytes(pb.encode_double(NEG_ZERO), "0000000000000080", "double encode negative zero")
  assert_eq(1 / pb.decode_double(from_hex("0000000000000080"), 1), -INF, "double decode negative zero")
  assert_eq(1 / pb.decode_double(from_hex("0000000000000000"), 1), INF, "double decode positive zero")

  -- ============================================================================
  -- ZIGZAG ENCODING (official protobuf spec test vectors)
  -- ============================================================================

  local zigzag32_vectors = {
    { 0, 0 },
    { -1, 1 },
    { 1, 2 },
    { -2, 3 },
    { 2147483647, 4294967294 },
    { -2147483648, 4294967295 },
  }
  for _, t in ipairs(zigzag32_vectors) do
    assert_eq(pb.zigzag_encode32(t[1]), t[2], "zigzag32 encode " .. t[1])
  end

  for _, v in ipairs({ 0, 1, -1, 100, -100, 2147483647, -2147483648 }) do
    local dec = pb.zigzag_decode32(pb.zigzag_encode32(v))
    assert_eq(dec, v, "zigzag32 roundtrip " .. v)
  end

  -- Additional zigzag32 edge cases (values that produce high-bit-set results)
  for _, v in ipairs({ 0x3FFFFFFF, 0x40000000, -0x40000000, -0x40000001 }) do
    local dec = pb.zigzag_decode32(pb.zigzag_encode32(v))
    assert_eq(dec, v, string.format("zigzag32 edge case %d roundtrip", v))
  end

  local zigzag64_vectors = {
    { bit64_new(0, 0), bit64_new(0, 0) },
    { bit64_new(0xFFFFFFFF, 0xFFFFFFFF), bit64_new(0, 1) },
    { bit64_new(0, 1), bit64_new(0, 2) },
  }
  for _, t in ipairs(zigzag64_vectors) do
    local enc = pb.zigzag_encode64(t[1])
    assert_int64(enc, t[2][1], t[2][2], string.format("zigzag64 encode {0x%X, 0x%X}", t[1][1], t[1][2]))
  end

  local zigzag64_roundtrip = {
    bit64_new(0, 0),
    bit64_new(0xFFFFFFFF, 0xFFFFFFFF),
    bit64_new(0, 1),
    bit64_new(0xFFFFFFFF, 0xFFFFFFFE),
    bit64_new(0x7FFFFFFF, 0xFFFFFFFF),
  }
  for _, v in ipairs(zigzag64_roundtrip) do
    local dec = pb.zigzag_decode64(pb.zigzag_encode64(v))
    assert_int64(dec, v[1], v[2], string.format("zigzag64 roundtrip {0x%X, 0x%X}", v[1], v[2]))
  end

  -- ============================================================================
  -- LENGTH-DELIMITED ENCODING
  -- ============================================================================

  assert_bytes(pb.encode_length_delimited("testing"), "0774657374696E67", "length_delimited 'testing'")

  for _, s in ipairs({ "", "hello", string.rep("x", 1000) }) do
    local dec = pb.decode_length_delimited(pb.encode_length_delimited(s), 1)
    assert_eq(dec, s, "length_delimited roundtrip len=" .. #s)
  end

  -- ============================================================================
  -- INT64 UTILITIES
  -- ============================================================================

  assert_eq(pb.int64_to_hex({ 0x12345678, 0x9ABCDEF0 }), "123456789ABCDEF0", "int64_to_hex")

  local num = 123456789012345
  assert_eq(pb.int64_to_number(pb.int64_from_number(num)), num, "int64 from/to number roundtrip")
  assert_eq(bit64_is_int64(pb.int64_from_number(num)), true, "int64_from_number returns marked Int64")

  assert_eq(pb.int64_equals({ 1, 2 }, { 1, 2 }), true, "int64_equals same")
  assert_eq(pb.int64_equals({ 1, 2 }, { 1, 3 }), false, "int64_equals diff")
  assert_eq(pb.int64_is_zero({ 0, 0 }), true, "int64_is_zero true")
  assert_eq(pb.int64_is_zero({ 0, 1 }), false, "int64_is_zero false")

  assert_error(function()
    pb.int64_to_number(bit64_new(0x00200000, 0), true)
  end, "53%-bit", "int64_to_number strict mode rejects >53-bit")

  -- Int64 vs array distinction
  local int64_val = bit64_new(0, 42)
  local array_val = { 1, 2 }
  assert_eq(bit64_is_int64(int64_val), true, "bit64.is_int64 identifies Int64")
  assert_eq(bit64_is_int64(array_val), false, "bit64.is_int64 rejects plain array")

  -- ============================================================================
  -- MESSAGE ENCODE/DECODE: SCALAR TYPES
  -- ============================================================================

  -- Bool
  local boolSchema = make_schema("Bool", "flag", Schema.DataType.BOOL, Schema.WireType.VARINT)
  for _, v in ipairs({ true, false }) do
    local dec = pb.decode(Schema, boolSchema, pb.encode(Schema, boolSchema, { flag = v }))
    assert_eq(dec.flag, v, "encode/decode bool " .. tostring(v))
  end

  -- Int32
  local int32Schema = make_schema("Int32", "value", Schema.DataType.INT32, Schema.WireType.VARINT)
  for _, v in ipairs({ 0, 1, 127, 128, 65535, 2147483647 }) do
    local dec = pb.decode(Schema, int32Schema, pb.encode(Schema, int32Schema, { value = v }))
    assert_eq(dec.value, v, "encode/decode int32 " .. v)
  end

  -- Uint64
  local uint64Schema = make_schema("Uint64", "value", Schema.DataType.UINT64, Schema.WireType.VARINT)
  local u64 = bit64_new(0x00001800, 0x00001000)
  local decU64 = pb.decode(Schema, uint64Schema, pb.encode(Schema, uint64Schema, { value = u64 }))
  assert_int64(decU64.value, u64[1], u64[2], "encode/decode uint64")

  -- Sint32 (zigzag)
  local sint32Schema = make_schema("Sint32", "value", Schema.DataType.SINT32, Schema.WireType.VARINT)
  for _, v in ipairs({ 0, 1, -1, 100, -100, 2147483647, -2147483648 }) do
    local dec = pb.decode(Schema, sint32Schema, pb.encode(Schema, sint32Schema, { value = v }))
    assert_eq(dec.value, v, "encode/decode sint32 " .. v)
  end

  -- Sint64 (zigzag)
  local sint64Schema = make_schema("Sint64", "value", Schema.DataType.SINT64, Schema.WireType.VARINT)
  for _, t in ipairs({ { bit64_new(0, 0), "0" }, { bit64_new(0, 1), "1" }, { bit64_new(0xFFFFFFFF, 0xFFFFFFFF), "-1" } }) do
    local dec = pb.decode(Schema, sint64Schema, pb.encode(Schema, sint64Schema, { value = t[1] }))
    assert_int64(dec.value, t[1][1], t[1][2], "encode/decode sint64 " .. t[2])
  end

  -- String
  local stringSchema = make_schema("String", "text", Schema.DataType.STRING, Schema.WireType.LENGTH_DELIMITED)
  for _, v in ipairs({ "", "hello", "unicode: \xC3\xA9", string.rep("x", 1000) }) do
    local dec = pb.decode(Schema, stringSchema, pb.encode(Schema, stringSchema, { text = v }))
    assert_eq(dec.text, v, "encode/decode string len=" .. #v)
  end

  -- A non-string/table value on a length-delimited field must raise, not emit a
  -- bare tag with no payload (which would silently truncate the stream).
  assert_error(function()
    pb.encode(Schema, stringSchema, { text = 42 })
  end, "length%-delimited", "error: number on a length-delimited field")

  -- Float
  local floatSchema = make_schema("Float", "value", Schema.DataType.FLOAT, Schema.WireType.FIXED32)
  for _, v in ipairs({ 0.0, 1.0, -1.0, 3.14159, 1e10 }) do
    local dec = pb.decode(Schema, floatSchema, pb.encode(Schema, floatSchema, { value = v }))
    assert_close(dec.value, v, 1e-4, "encode/decode float " .. v)
  end

  -- Double
  local doubleSchema = make_schema("Double", "value", Schema.DataType.DOUBLE, Schema.WireType.FIXED64)
  for _, v in ipairs({ 0.0, 1.0, -1.0, 3.141592653589793, 1e100 }) do
    local dec = pb.decode(Schema, doubleSchema, pb.encode(Schema, doubleSchema, { value = v }))
    assert_close(dec.value, v, 1e-10, "encode/decode double " .. v)
  end

  -- The reported failure came through the FIXED32/FLOAT and FIXED64/DOUBLE
  -- branches of encode_scalar and decode_scalar, not through the codecs alone,
  -- so a wrong-width or wrong-branch regression there would leave the direct
  -- codec assertions above green.
  assert_nan(
    pb.decode(Schema, floatSchema, pb.encode(Schema, floatSchema, { value = NAN })).value,
    "encode/decode float NaN"
  )
  assert_nan(
    pb.decode(Schema, doubleSchema, pb.encode(Schema, doubleSchema, { value = NAN })).value,
    "encode/decode double NaN"
  )
  assert_eq(
    pb.decode(Schema, floatSchema, pb.encode(Schema, floatSchema, { value = INF })).value,
    INF,
    "encode/decode float +infinity"
  )
  assert_eq(
    pb.decode(Schema, doubleSchema, pb.encode(Schema, doubleSchema, { value = -INF })).value,
    -INF,
    "encode/decode double -infinity"
  )

  -- Fixed32
  local fixed32FieldSchema = make_schema("Fixed32", "value", Schema.DataType.FIXED32, Schema.WireType.FIXED32)
  for _, v in ipairs({ 0, 1, 255, 0xFFFFFFFF }) do
    local dec = pb.decode(Schema, fixed32FieldSchema, pb.encode(Schema, fixed32FieldSchema, { value = v }))
    assert_eq(dec.value, v, "encode/decode fixed32 " .. v)
  end

  -- Fixed64
  local fixed64FieldSchema = make_schema("Fixed64", "value", Schema.DataType.FIXED64, Schema.WireType.FIXED64)
  for _, v in ipairs({ bit64_new(0, 0), bit64_new(0, 1), bit64_new(0xFFFFFFFF, 0xFFFFFFFF) }) do
    local dec = pb.decode(Schema, fixed64FieldSchema, pb.encode(Schema, fixed64FieldSchema, { value = v }))
    assert_int64(dec.value, v[1], v[2], string.format("encode/decode fixed64 {0x%X, 0x%X}", v[1], v[2]))
  end

  -- Enum
  local enumSchema = make_schema("Enum", "status", Schema.DataType.ENUM, Schema.WireType.VARINT)
  for _, v in ipairs({ 0, 1, 2, 100 }) do
    local dec = pb.decode(Schema, enumSchema, pb.encode(Schema, enumSchema, { status = v }))
    assert_eq(dec.status, v, "encode/decode enum " .. v)
  end

  -- Large field numbers (tests raw lshift signed/unsigned handling in key encoding)
  for _, fn in ipairs({ 536870911, 268435455, 100000 }) do
    local largeFieldSchema = {
      name = "LargeField",
      options = {},
      fields = {
        [fn] = { name = "val", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
      },
    }
    local dec = pb.decode(Schema, largeFieldSchema, pb.encode(Schema, largeFieldSchema, { val = 42 }))
    assert_eq(dec.val, 42, "encode/decode field number " .. fn)
  end

  -- ============================================================================
  -- MESSAGE ENCODE/DECODE: REPEATED FIELDS
  -- ============================================================================

  local repeatedSchema =
    make_schema("Repeated", "values", Schema.DataType.INT32, Schema.WireType.VARINT, { repeated = true })
  local vals = { 1, 2, 3, 100, 200 }
  local decR = pb.decode(Schema, repeatedSchema, pb.encode(Schema, repeatedSchema, { values = vals }))
  assert_eq(#decR.values, #vals, "repeated field count")
  for i, v in ipairs(vals) do
    assert_eq(decR.values[i], v, "repeated field[" .. i .. "]")
  end

  -- ============================================================================
  -- MESSAGE DECODE: PACKED REPEATED FIELDS
  -- ============================================================================

  -- `pb.encode` only emits the unpacked spelling, so these assemble the wire bytes by
  -- hand: field 1 tagged LENGTH_DELIMITED over a block of untagged elements.
  local function packed_field(block)
    return pb.encode_varint(8 + Schema.WireType.LENGTH_DELIMITED) .. pb.encode_length_delimited(block)
  end

  local function concat_encoded(values, encode)
    local block = ""
    for _, v in ipairs(values) do
      block = block .. encode(v)
    end
    return block
  end

  local packedU32Schema =
    make_schema("PackedU32", "ids", Schema.DataType.UINT32, Schema.WireType.VARINT, { repeated = true })
  local u32vals = { 0, 1, 300, 65535 }
  local decPU = pb.decode(Schema, packedU32Schema, packed_field(concat_encoded(u32vals, pb.encode_varint)))
  assert_eq(#decPU.ids, #u32vals, "packed uint32 element count")
  for i, v in ipairs(u32vals) do
    assert_eq(decPU.ids[i], v, "packed uint32[" .. i .. "]")
  end

  local packedS32Schema =
    make_schema("PackedS32", "deltas", Schema.DataType.SINT32, Schema.WireType.VARINT, { repeated = true })
  local s32vals = { -1, 1, -500 }
  local decPS = pb.decode(
    Schema,
    packedS32Schema,
    packed_field(concat_encoded(s32vals, function(v)
      return pb.encode_varint(pb.zigzag_encode32(v))
    end))
  )
  for i, v in ipairs(s32vals) do
    assert_eq(decPS.deltas[i], v, "packed sint32[" .. i .. "] zigzag-decoded")
  end

  local packedBoolSchema =
    make_schema("PackedBool", "flags", Schema.DataType.BOOL, Schema.WireType.VARINT, { repeated = true })
  local decPBool = pb.decode(Schema, packedBoolSchema, packed_field("\1\0\1"))
  assert_eq(#decPBool.flags, 3, "packed bool element count")
  assert_eq(decPBool.flags[1], true, "packed bool[1]")
  assert_eq(decPBool.flags[2], false, "packed bool[2]")

  local packedU64Schema =
    make_schema("PackedU64", "counters", Schema.DataType.UINT64, Schema.WireType.VARINT, { repeated = true })
  local decPU64 = pb.decode(Schema, packedU64Schema, packed_field(concat_encoded({ 1, 300 }, pb.encode_varint)))
  assert_eq(#decPU64.counters, 2, "packed uint64 element count")
  assert_int64(decPU64.counters[1], 0, 1, "packed uint64[1] stays an Int64 pair")
  assert_int64(decPU64.counters[2], 0, 300, "packed uint64[2] stays an Int64 pair")

  local packedFloatSchema =
    make_schema("PackedFloat", "samples", Schema.DataType.FLOAT, Schema.WireType.FIXED32, { repeated = true })
  local decPFloat = pb.decode(Schema, packedFloatSchema, packed_field(concat_encoded({ 1.5, -2.25 }, pb.encode_float)))
  assert_eq(#decPFloat.samples, 2, "packed float element count")
  assert_close(decPFloat.samples[1], 1.5, 0.0001, "packed float[1]")
  assert_close(decPFloat.samples[2], -2.25, 0.0001, "packed float[2]")

  local packedDoubleSchema =
    make_schema("PackedDouble", "readings", Schema.DataType.DOUBLE, Schema.WireType.FIXED64, { repeated = true })
  local decPDouble =
    pb.decode(Schema, packedDoubleSchema, packed_field(concat_encoded({ 0.5, 3.75 }, pb.encode_double)))
  assert_eq(#decPDouble.readings, 2, "packed double element count")
  assert_close(decPDouble.readings[2], 3.75, 0.0001, "packed double[2]")

  local decMixed = pb.decode(
    Schema,
    packedU32Schema,
    packed_field(pb.encode_varint(7))
      .. pb.encode(Schema, packedU32Schema, { ids = { 1, 300 } })
      .. packed_field(pb.encode_varint(9))
  )
  assert_eq(#decMixed.ids, 4, "packed blocks and unpacked tags append to one list")
  assert_eq(decMixed.ids[1], 7, "mixed packed/unpacked[1]")
  assert_eq(decMixed.ids[3], 300, "mixed packed/unpacked[3]")
  assert_eq(decMixed.ids[4], 9, "mixed packed/unpacked[4]")

  assert_eq(#pb.decode(Schema, packedU32Schema, packed_field("")).ids, 0, "empty packed block decodes to no elements")

  local repeatedStrSchema =
    make_schema("RepeatedStr", "names", Schema.DataType.STRING, Schema.WireType.LENGTH_DELIMITED, { repeated = true })
  local decRS = pb.decode(Schema, repeatedStrSchema, pb.encode(Schema, repeatedStrSchema, { names = { "a", "bc" } }))
  assert_eq(#decRS.names, 2, "repeated string is not unpacked as a packed block")
  assert_eq(decRS.names[2], "bc", "repeated string[2]")

  -- ============================================================================
  -- MESSAGE ENCODE/DECODE: NESTED MESSAGES
  -- ============================================================================

  -- Single level nesting
  Schema.Message["Inner"] = {
    name = "Inner",
    options = {},
    fields = {
      [1] = { name = "id", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
    },
  }
  local outerSchema =
    make_schema("Outer", "inner", Schema.DataType.MESSAGE, Schema.WireType.LENGTH_DELIMITED, { subschema = "Inner" })
  local decO = pb.decode(Schema, outerSchema, pb.encode(Schema, outerSchema, { inner = { id = 42 } }))
  assert_eq(decO.inner.id, 42, "nested message single level")

  -- Deep nesting (3 levels)
  Schema.Message["L3"] = {
    name = "L3",
    options = {},
    fields = {
      [1] = { name = "val", type = Schema.DataType.STRING, wireType = Schema.WireType.LENGTH_DELIMITED },
    },
  }
  Schema.Message["L2"] = {
    name = "L2",
    options = {},
    fields = {
      [1] = { name = "name", type = Schema.DataType.STRING, wireType = Schema.WireType.LENGTH_DELIMITED },
      [2] = {
        name = "child",
        type = Schema.DataType.MESSAGE,
        wireType = Schema.WireType.LENGTH_DELIMITED,
        subschema = "L3",
      },
    },
  }
  local l1Schema = {
    name = "L1",
    options = {},
    fields = {
      [1] = { name = "id", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
      [2] = {
        name = "child",
        type = Schema.DataType.MESSAGE,
        wireType = Schema.WireType.LENGTH_DELIMITED,
        subschema = "L2",
      },
    },
  }
  local deepMsg = { id = 100, child = { name = "level2", child = { val = "deepest" } } }
  local decD = pb.decode(Schema, l1Schema, pb.encode(Schema, l1Schema, deepMsg))
  assert_eq(decD.id, 100, "deep nested level1.id")
  assert_eq(decD.child.name, "level2", "deep nested level2.name")
  assert_eq(decD.child.child.val, "deepest", "deep nested level3.val")

  -- Repeated nested messages
  Schema.Message["Item"] = {
    name = "Item",
    options = {},
    fields = {
      [1] = { name = "name", type = Schema.DataType.STRING, wireType = Schema.WireType.LENGTH_DELIMITED },
      [2] = { name = "qty", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
    },
  }
  local orderSchema = {
    name = "Order",
    options = {},
    fields = {
      [1] = { name = "id", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
      [2] = {
        name = "items",
        type = Schema.DataType.MESSAGE,
        wireType = Schema.WireType.LENGTH_DELIMITED,
        subschema = "Item",
        repeated = true,
      },
    },
  }
  local orderMsg = { id = 123, items = { { name = "A", qty = 5 }, { name = "B", qty = 3 } } }
  local decOrder = pb.decode(Schema, orderSchema, pb.encode(Schema, orderSchema, orderMsg))
  assert_eq(decOrder.id, 123, "repeated nested order.id")
  assert_eq(#decOrder.items, 2, "repeated nested item count")
  assert_eq(decOrder.items[1].name, "A", "repeated nested item[1].name")
  assert_eq(decOrder.items[2].qty, 3, "repeated nested item[2].qty")

  -- Multiple nested message fields
  Schema.Message["Addr"] = {
    name = "Addr",
    options = {},
    fields = {
      [1] = { name = "street", type = Schema.DataType.STRING, wireType = Schema.WireType.LENGTH_DELIMITED },
      [2] = { name = "city", type = Schema.DataType.STRING, wireType = Schema.WireType.LENGTH_DELIMITED },
    },
  }
  local personSchema = {
    name = "Person",
    options = {},
    fields = {
      [1] = { name = "name", type = Schema.DataType.STRING, wireType = Schema.WireType.LENGTH_DELIMITED },
      [2] = {
        name = "home",
        type = Schema.DataType.MESSAGE,
        wireType = Schema.WireType.LENGTH_DELIMITED,
        subschema = "Addr",
      },
      [3] = {
        name = "work",
        type = Schema.DataType.MESSAGE,
        wireType = Schema.WireType.LENGTH_DELIMITED,
        subschema = "Addr",
      },
    },
  }
  local personMsg =
    { name = "Alice", home = { street = "123 Home", city = "H" }, work = { street = "456 Work", city = "W" } }
  local decP = pb.decode(Schema, personSchema, pb.encode(Schema, personSchema, personMsg))
  assert_eq(decP.name, "Alice", "multi-nested person.name")
  assert_eq(decP.home.city, "H", "multi-nested home.city")
  assert_eq(decP.work.city, "W", "multi-nested work.city")

  -- ============================================================================
  -- MESSAGE ENCODE/DECODE: MAP FIELDS
  -- ============================================================================

  local function make_entry(name, key_type, key_wire, value_type, value_wire, value_subschema)
    Schema.Message[name] = {
      name = name,
      options = {},
      fields = {
        [1] = { name = "key", type = key_type, wireType = key_wire },
        [2] = { name = "value", type = value_type, wireType = value_wire, subschema = value_subschema },
      },
    }
  end

  local function map_size(t)
    local n = 0
    for _ in pairs(t) do
      n = n + 1
    end
    return n
  end

  -- map<string, int32>
  make_entry(
    "CountsEntry",
    Schema.DataType.STRING,
    Schema.WireType.LENGTH_DELIMITED,
    Schema.DataType.INT32,
    Schema.WireType.VARINT
  )
  local countsSchema = make_schema(
    "Counts",
    "counts",
    Schema.DataType.MESSAGE,
    Schema.WireType.LENGTH_DELIMITED,
    { map = true, subschema = "CountsEntry" }
  )

  -- `pb.encode` emits a message's fields in `pairs` order, which differs between Lua
  -- and LuaJIT, and protobuf accepts a submessage's fields in either order.
  local framing = "\10\5" -- field 1, LENGTH_DELIMITED, 5 bytes
  local entryKey = "\10\1" .. "a" -- key = "a"
  local entryValue = "\16\1" -- value = 1
  local oneEntry = pb.encode(Schema, countsSchema, { counts = { a = 1 } })
  assert_eq(
    oneEntry == framing .. entryKey .. entryValue or oneEntry == framing .. entryValue .. entryKey,
    true,
    "map encodes as one length-delimited key/value entry"
  )

  local counts = { alpha = 1, beta = 2, [""] = 0 }
  local decC = pb.decode(Schema, countsSchema, pb.encode(Schema, countsSchema, { counts = counts }))
  assert_eq(map_size(decC.counts), 3, "map<string,int32> entry count")
  for k, v in pairs(counts) do
    assert_eq(decC.counts[k], v, "map<string,int32> roundtrip key " .. string.format("%q", k))
  end

  -- map<int32, string>: a non-string key stays a number through the roundtrip
  make_entry(
    "NamesEntry",
    Schema.DataType.INT32,
    Schema.WireType.VARINT,
    Schema.DataType.STRING,
    Schema.WireType.LENGTH_DELIMITED
  )
  local namesSchema = make_schema(
    "Names",
    "names",
    Schema.DataType.MESSAGE,
    Schema.WireType.LENGTH_DELIMITED,
    { map = true, subschema = "NamesEntry" }
  )
  local decN = pb.decode(Schema, namesSchema, pb.encode(Schema, namesSchema, { names = { [7] = "seven" } }))
  assert_eq(decN.names[7], "seven", "map<int32,string> numeric key roundtrip")

  -- map<string, Item>: message values recurse through the same entry path
  make_entry(
    "ItemsEntry",
    Schema.DataType.STRING,
    Schema.WireType.LENGTH_DELIMITED,
    Schema.DataType.MESSAGE,
    Schema.WireType.LENGTH_DELIMITED,
    "Item"
  )
  local itemsSchema = make_schema(
    "Items",
    "items",
    Schema.DataType.MESSAGE,
    Schema.WireType.LENGTH_DELIMITED,
    { map = true, subschema = "ItemsEntry" }
  )
  local decI =
    pb.decode(Schema, itemsSchema, pb.encode(Schema, itemsSchema, { items = { widget = { name = "W", qty = 9 } } }))
  assert_eq(decI.items.widget.name, "W", "map<string,Item> message value name")
  assert_eq(decI.items.widget.qty, 9, "map<string,Item> message value qty")

  -- An empty map contributes no bytes, and so decodes back to no field at all
  assert_eq(pb.encode(Schema, countsSchema, { counts = {} }), "", "empty map encodes to nothing")
  assert_eq(pb.decode(Schema, countsSchema, "").counts, nil, "empty map decodes to nil field")

  -- A producer may drop either side of an entry when it holds the zero value, so a
  -- zero-length entry has to read back as the key and value defaults rather than raise.
  local decDefaults = pb.decode(Schema, countsSchema, "\10\0")
  assert_eq(decDefaults.counts[""], 0, "entry omitting key and value applies both defaults")
  local decDefaultValue = pb.decode(Schema, namesSchema, "\10\2\8\7")
  assert_eq(decDefaultValue.names[7], "", "entry omitting value applies the value default")

  -- ============================================================================
  -- EDGE CASES
  -- ============================================================================

  -- Empty message
  local emptyDec = pb.decode(Schema, int32Schema, pb.encode(Schema, int32Schema, {}))
  assert_eq(emptyDec.value, nil, "empty message has nil field")

  -- Unknown field skipping
  local twoField = {
    name = "Two",
    options = {},
    fields = {
      [1] = { name = "a", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
      [2] = { name = "b", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
    },
  }
  local oneField = {
    name = "One",
    options = {},
    fields = {
      [1] = { name = "a", type = Schema.DataType.INT32, wireType = Schema.WireType.VARINT },
    },
  }
  local enc2 = pb.encode(Schema, twoField, { a = 123, b = 456 })
  local dec1 = pb.decode(Schema, oneField, enc2)
  assert_eq(dec1.a, 123, "unknown field skipped, known preserved")

  -- ============================================================================
  -- ERROR HANDLING
  -- ============================================================================

  assert_error(function()
    pb.encode(Schema, repeatedSchema, { values = 123 })
  end, "non%-list", "error: non-list for repeated field")

  assert_error(function()
    pb.encode(Schema, int32Schema, { value = { 1, 2, 3 } })
  end, "not repeated", "error: list for non-repeated field")

  local noSubSchema = make_schema("NoSub", "nested", Schema.DataType.MESSAGE, Schema.WireType.LENGTH_DELIMITED)
  assert_error(function()
    pb.encode(Schema, noSubSchema, { nested = { foo = 1 } })
  end, "no subschema", "error: nested message without subschema")

  assert_error(function()
    pb.encode(Schema, countsSchema, { counts = 42 })
  end, "non%-table", "error: non-table for map field")

  local bytesSchema = make_schema("Bytes", "data", Schema.DataType.BYTES, Schema.WireType.LENGTH_DELIMITED)
  assert_error(function()
    pb.encode(Schema, bytesSchema, { data = true })
  end, "length%-delimited", "error: boolean for bytes field")

  assert_error(function()
    pb.encode(Schema, outerSchema, { inner = 42 })
  end, "length%-delimited", "error: number for message field")

  local repeatedStringSchema =
    make_schema("RepeatedString", "tags", Schema.DataType.STRING, Schema.WireType.LENGTH_DELIMITED, { repeated = true })
  assert_error(function()
    pb.encode(Schema, repeatedStringSchema, { tags = { "ok", 42 } })
  end, "length%-delimited", "error: number element in repeated string field")

  -- ============================================================================
  -- SUMMARY
  -- ============================================================================
  print(string.format("\nProtobuf operations: %d/%d tests passed\n", passed, passed + failed))
  return failed == 0
end

return selftest
