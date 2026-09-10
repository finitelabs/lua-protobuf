-- Generated Lua schema from protobuf descriptor set
-- Do not edit manually

--- @class ProtoSchema
local ProtoSchema = {}

--- Maps enum names to their definitions.
ProtoSchema.Enum = {}

--- Maps message names to their definitions.
--- @type table<string, ProtoMessageSchema>
ProtoSchema.Message = {}

--- Maps service names to their method definitions.
--- @type table<string, ProtoServiceSchema>
ProtoSchema.RPC = {}

--- ProtoWireType Maps protobuf wire types to their integer values.
--- @enum ProtoWireType
ProtoSchema.WireType = {
  VARINT = 0,
  FIXED64 = 1,
  LENGTH_DELIMITED = 2,
  FIXED32 = 5,
}

--- ProtoDataType Maps protobuf data types to their integer values.
--- @enum ProtoDataType
ProtoSchema.DataType = {
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
}

--- @class ProtoFieldSchema
--- @field name string The name of the field.
--- @field wireType ProtoWireType The protobuf wire type (see ProtoSchema.WireType).
--- @field type ProtoDataType The protobuf type (see ProtoSchema.DataType).
--- @field repeated boolean? Whether the field is repeated (optional).
--- @field map boolean? Whether the field is a map, encoded as repeated `subschema` entries (optional).
--- @field subschema string? The subschema name for nested messages (optional).

--- @class ProtoMessageSchema
--- @field name string The name of the message type.
--- @field options table<string, any> Message options.
--- @field fields table<integer,  ProtoFieldSchema> A map of field numbers to ProtoFieldSchema definitions.

--- @class ProtoServiceMethodSchema
--- @field service string The name of the service.
--- @field method string The method name.
--- @field inputType ProtoMessageSchema The protobuf message type for the request.
--- @field outputType ProtoMessageSchema The protobuf message type for the response.

--- @class ProtoServiceSchema
--- @field [string] ProtoServiceMethodSchema Maps method names to their method definitions.

--- @class ProtoTestAllTypesProto3
--- @field optional_int32 number?
--- @field optional_int64 (number|Int64HighLow)?
--- @field optional_uint32 number?
--- @field optional_uint64 (number|Int64HighLow)?
--- @field optional_sint32 number?
--- @field optional_sint64 (number|Int64HighLow)?
--- @field optional_fixed32 number?
--- @field optional_fixed64 (number|Int64HighLow)?
--- @field optional_sfixed32 number?
--- @field optional_sfixed64 (number|Int64HighLow)?
--- @field optional_float number?
--- @field optional_double number?
--- @field optional_bool boolean?
--- @field optional_string string?
--- @field optional_bytes string?
--- @field optional_nested_message ProtoNestedMessage?
--- @field optional_foreign_message ProtoForeignMessage?
--- @field optional_nested_enum ProtoNestedEnum?
--- @field optional_foreign_enum ProtoForeignEnum?
--- @field optional_aliased_enum ProtoAliasedEnum?
--- @field optional_string_piece string?
--- @field optional_cord string?
--- @field recursive_message ProtoTestAllTypesProto3?
--- @field repeated_int32 number[]?
--- @field repeated_int64 (number[]|Int64HighLow[])?
--- @field repeated_uint32 number[]?
--- @field repeated_uint64 (number[]|Int64HighLow[])?
--- @field repeated_sint32 number[]?
--- @field repeated_sint64 (number[]|Int64HighLow[])?
--- @field repeated_fixed32 number[]?
--- @field repeated_fixed64 (number[]|Int64HighLow[])?
--- @field repeated_sfixed32 number[]?
--- @field repeated_sfixed64 (number[]|Int64HighLow[])?
--- @field repeated_float number[]?
--- @field repeated_double number[]?
--- @field repeated_bool boolean[]?
--- @field repeated_string string[]?
--- @field repeated_bytes string[]?
--- @field repeated_nested_message ProtoNestedMessage[]?
--- @field repeated_foreign_message ProtoForeignMessage[]?
--- @field repeated_nested_enum ProtoNestedEnum[]?
--- @field repeated_foreign_enum ProtoForeignEnum[]?
--- @field repeated_string_piece string[]?
--- @field repeated_cord string[]?
--- @field packed_int32 number[]?
--- @field packed_int64 (number[]|Int64HighLow[])?
--- @field packed_uint32 number[]?
--- @field packed_uint64 (number[]|Int64HighLow[])?
--- @field packed_sint32 number[]?
--- @field packed_sint64 (number[]|Int64HighLow[])?
--- @field packed_fixed32 number[]?
--- @field packed_fixed64 (number[]|Int64HighLow[])?
--- @field packed_sfixed32 number[]?
--- @field packed_sfixed64 (number[]|Int64HighLow[])?
--- @field packed_float number[]?
--- @field packed_double number[]?
--- @field packed_bool boolean[]?
--- @field packed_nested_enum ProtoNestedEnum[]?
--- @field unpacked_int32 number[]?
--- @field unpacked_int64 (number[]|Int64HighLow[])?
--- @field unpacked_uint32 number[]?
--- @field unpacked_uint64 (number[]|Int64HighLow[])?
--- @field unpacked_sint32 number[]?
--- @field unpacked_sint64 (number[]|Int64HighLow[])?
--- @field unpacked_fixed32 number[]?
--- @field unpacked_fixed64 (number[]|Int64HighLow[])?
--- @field unpacked_sfixed32 number[]?
--- @field unpacked_sfixed64 (number[]|Int64HighLow[])?
--- @field unpacked_float number[]?
--- @field unpacked_double number[]?
--- @field unpacked_bool boolean[]?
--- @field unpacked_nested_enum ProtoNestedEnum[]?
--- @field map_int32_int32 table<number, number>?
--- @field map_int64_int64 (table<number|Int64HighLow, (number|Int64HighLow)>)?
--- @field map_uint32_uint32 table<number, number>?
--- @field map_uint64_uint64 (table<number|Int64HighLow, (number|Int64HighLow)>)?
--- @field map_sint32_sint32 table<number, number>?
--- @field map_sint64_sint64 (table<number|Int64HighLow, (number|Int64HighLow)>)?
--- @field map_fixed32_fixed32 table<number, number>?
--- @field map_fixed64_fixed64 (table<number|Int64HighLow, (number|Int64HighLow)>)?
--- @field map_sfixed32_sfixed32 table<number, number>?
--- @field map_sfixed64_sfixed64 (table<number|Int64HighLow, (number|Int64HighLow)>)?
--- @field map_int32_float table<number, number>?
--- @field map_int32_double table<number, number>?
--- @field map_bool_bool table<boolean, boolean>?
--- @field map_string_string table<string, string>?
--- @field map_string_bytes table<string, string>?
--- @field map_string_nested_message table<string, ProtoNestedMessage>?
--- @field map_string_foreign_message table<string, ProtoForeignMessage>?
--- @field map_string_nested_enum table<string, ProtoNestedEnum>?
--- @field map_string_foreign_enum table<string, ProtoForeignEnum>?
--- @field oneof_uint32 number?
--- @field oneof_nested_message ProtoNestedMessage?
--- @field oneof_string string?
--- @field oneof_bytes string?
--- @field oneof_bool boolean?
--- @field oneof_uint64 (number|Int64HighLow)?
--- @field oneof_float number?
--- @field oneof_double number?
--- @field oneof_enum ProtoNestedEnum?
--- @field fieldname1 number?
--- @field field_name2 number?
--- @field _field_name3 number?
--- @field field__name4_ number?
--- @field field0name5 number?
--- @field field_0_name6 number?
--- @field fieldName7 number?
--- @field FieldName8 number?
--- @field field_Name9 number?
--- @field Field_Name10 number?
--- @field FIELD_NAME11 number?
--- @field FIELD_name12 number?
--- @field __field_name13 number?
--- @field __Field_name14 number?
--- @field field__name15 number?
--- @field field__Name16 number?
--- @field field_name17__ number?
--- @field Field_name18__ number?

--- @class ProtoNestedMessage
--- @field a number?
--- @field corecursive ProtoTestAllTypesProto3?

--- @class ProtoMapInt32Int32Entry
--- @field key number?
--- @field value number?

--- @class ProtoMapInt64Int64Entry
--- @field key (number|Int64HighLow)?
--- @field value (number|Int64HighLow)?

--- @class ProtoMapUint32Uint32Entry
--- @field key number?
--- @field value number?

--- @class ProtoMapUint64Uint64Entry
--- @field key (number|Int64HighLow)?
--- @field value (number|Int64HighLow)?

--- @class ProtoMapSint32Sint32Entry
--- @field key number?
--- @field value number?

--- @class ProtoMapSint64Sint64Entry
--- @field key (number|Int64HighLow)?
--- @field value (number|Int64HighLow)?

--- @class ProtoMapFixed32Fixed32Entry
--- @field key number?
--- @field value number?

--- @class ProtoMapFixed64Fixed64Entry
--- @field key (number|Int64HighLow)?
--- @field value (number|Int64HighLow)?

--- @class ProtoMapSfixed32Sfixed32Entry
--- @field key number?
--- @field value number?

--- @class ProtoMapSfixed64Sfixed64Entry
--- @field key (number|Int64HighLow)?
--- @field value (number|Int64HighLow)?

--- @class ProtoMapInt32FloatEntry
--- @field key number?
--- @field value number?

--- @class ProtoMapInt32DoubleEntry
--- @field key number?
--- @field value number?

--- @class ProtoMapBoolBoolEntry
--- @field key boolean?
--- @field value boolean?

--- @class ProtoMapStringStringEntry
--- @field key string?
--- @field value string?

--- @class ProtoMapStringBytesEntry
--- @field key string?
--- @field value string?

--- @class ProtoMapStringNestedMessageEntry
--- @field key string?
--- @field value ProtoNestedMessage?

--- @class ProtoMapStringForeignMessageEntry
--- @field key string?
--- @field value ProtoForeignMessage?

--- @class ProtoMapStringNestedEnumEntry
--- @field key string?
--- @field value ProtoNestedEnum?

--- @class ProtoMapStringForeignEnumEntry
--- @field key string?
--- @field value ProtoForeignEnum?

--- @class ProtoForeignMessage
--- @field c number?

--- @class ProtoNullHypothesisProto3

--- @class ProtoEnumOnlyProto3

-- Package: protobuf_test_messages.proto3
--- @enum ProtoForeignEnum
ProtoSchema.Enum["protobuf_test_messages.proto3.ForeignEnum"] = {
  FOREIGN_FOO = 0,
  FOREIGN_BAR = 1,
  FOREIGN_BAZ = 2,
}

--- @enum ProtoNestedEnum
ProtoSchema.Enum["protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum"] = {
  FOO = 0,
  BAR = 1,
  BAZ = 2,
  NEG = -1,
}

--- @enum ProtoAliasedEnum
ProtoSchema.Enum["protobuf_test_messages.proto3.TestAllTypesProto3.AliasedEnum"] = {
  ALIAS_FOO = 0,
  ALIAS_BAR = 1,
  ALIAS_BAZ = 2,
  MOO = 2,
  moo = 2,
  bAz = 2,
}

--- @enum ProtoBool
ProtoSchema.Enum["protobuf_test_messages.proto3.EnumOnlyProto3.Bool"] = {
  kFalse = 0,
  kTrue = 1,
}


-- Package: protobuf_test_messages.proto3
--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3",
  options = {},
  fields = {
    [1] = {
      name = "optional_int32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [2] = {
      name = "optional_int64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT64,
    },
    [3] = {
      name = "optional_uint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
    },
    [4] = {
      name = "optional_uint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
    },
    [5] = {
      name = "optional_sint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT32,
    },
    [6] = {
      name = "optional_sint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT64,
    },
    [7] = {
      name = "optional_fixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FIXED32,
    },
    [8] = {
      name = "optional_fixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.FIXED64,
    },
    [9] = {
      name = "optional_sfixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.SFIXED32,
    },
    [10] = {
      name = "optional_sfixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.SFIXED64,
    },
    [11] = {
      name = "optional_float",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FLOAT,
    },
    [12] = {
      name = "optional_double",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.DOUBLE,
    },
    [13] = {
      name = "optional_bool",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
    },
    [14] = {
      name = "optional_string",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [15] = {
      name = "optional_bytes",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.BYTES,
    },
    [18] = {
      name = "optional_nested_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.NestedMessage",
    },
    [19] = {
      name = "optional_foreign_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.ForeignMessage",
    },
    [21] = {
      name = "optional_nested_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum
    },
    [22] = {
      name = "optional_foreign_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.ForeignEnum
    },
    [23] = {
      name = "optional_aliased_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.AliasedEnum
    },
    [24] = {
      name = "optional_string_piece",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [25] = {
      name = "optional_cord",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [27] = {
      name = "recursive_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3",
    },
    [31] = {
      name = "repeated_int32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
      repeated = true,
    },
    [32] = {
      name = "repeated_int64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT64,
      repeated = true,
    },
    [33] = {
      name = "repeated_uint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
      repeated = true,
    },
    [34] = {
      name = "repeated_uint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
      repeated = true,
    },
    [35] = {
      name = "repeated_sint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT32,
      repeated = true,
    },
    [36] = {
      name = "repeated_sint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT64,
      repeated = true,
    },
    [37] = {
      name = "repeated_fixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FIXED32,
      repeated = true,
    },
    [38] = {
      name = "repeated_fixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.FIXED64,
      repeated = true,
    },
    [39] = {
      name = "repeated_sfixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.SFIXED32,
      repeated = true,
    },
    [40] = {
      name = "repeated_sfixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.SFIXED64,
      repeated = true,
    },
    [41] = {
      name = "repeated_float",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FLOAT,
      repeated = true,
    },
    [42] = {
      name = "repeated_double",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.DOUBLE,
      repeated = true,
    },
    [43] = {
      name = "repeated_bool",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
      repeated = true,
    },
    [44] = {
      name = "repeated_string",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
      repeated = true,
    },
    [45] = {
      name = "repeated_bytes",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.BYTES,
      repeated = true,
    },
    [48] = {
      name = "repeated_nested_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      repeated = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.NestedMessage",
    },
    [49] = {
      name = "repeated_foreign_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      repeated = true,
      subschema = "protobuf_test_messages.proto3.ForeignMessage",
    },
    [51] = {
      name = "repeated_nested_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum
      repeated = true,
    },
    [52] = {
      name = "repeated_foreign_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.ForeignEnum
      repeated = true,
    },
    [54] = {
      name = "repeated_string_piece",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
      repeated = true,
    },
    [55] = {
      name = "repeated_cord",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
      repeated = true,
    },
    [75] = {
      name = "packed_int32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
      repeated = true,
    },
    [76] = {
      name = "packed_int64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT64,
      repeated = true,
    },
    [77] = {
      name = "packed_uint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
      repeated = true,
    },
    [78] = {
      name = "packed_uint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
      repeated = true,
    },
    [79] = {
      name = "packed_sint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT32,
      repeated = true,
    },
    [80] = {
      name = "packed_sint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT64,
      repeated = true,
    },
    [81] = {
      name = "packed_fixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FIXED32,
      repeated = true,
    },
    [82] = {
      name = "packed_fixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.FIXED64,
      repeated = true,
    },
    [83] = {
      name = "packed_sfixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.SFIXED32,
      repeated = true,
    },
    [84] = {
      name = "packed_sfixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.SFIXED64,
      repeated = true,
    },
    [85] = {
      name = "packed_float",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FLOAT,
      repeated = true,
    },
    [86] = {
      name = "packed_double",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.DOUBLE,
      repeated = true,
    },
    [87] = {
      name = "packed_bool",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
      repeated = true,
    },
    [88] = {
      name = "packed_nested_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum
      repeated = true,
    },
    [89] = {
      name = "unpacked_int32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
      repeated = true,
    },
    [90] = {
      name = "unpacked_int64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT64,
      repeated = true,
    },
    [91] = {
      name = "unpacked_uint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
      repeated = true,
    },
    [92] = {
      name = "unpacked_uint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
      repeated = true,
    },
    [93] = {
      name = "unpacked_sint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT32,
      repeated = true,
    },
    [94] = {
      name = "unpacked_sint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT64,
      repeated = true,
    },
    [95] = {
      name = "unpacked_fixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FIXED32,
      repeated = true,
    },
    [96] = {
      name = "unpacked_fixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.FIXED64,
      repeated = true,
    },
    [97] = {
      name = "unpacked_sfixed32",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.SFIXED32,
      repeated = true,
    },
    [98] = {
      name = "unpacked_sfixed64",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.SFIXED64,
      repeated = true,
    },
    [99] = {
      name = "unpacked_float",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FLOAT,
      repeated = true,
    },
    [100] = {
      name = "unpacked_double",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.DOUBLE,
      repeated = true,
    },
    [101] = {
      name = "unpacked_bool",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
      repeated = true,
    },
    [102] = {
      name = "unpacked_nested_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum
      repeated = true,
    },
    [56] = {
      name = "map_int32_int32",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32Int32Entry",
    },
    [57] = {
      name = "map_int64_int64",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt64Int64Entry",
    },
    [58] = {
      name = "map_uint32_uint32",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapUint32Uint32Entry",
    },
    [59] = {
      name = "map_uint64_uint64",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapUint64Uint64Entry",
    },
    [60] = {
      name = "map_sint32_sint32",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSint32Sint32Entry",
    },
    [61] = {
      name = "map_sint64_sint64",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSint64Sint64Entry",
    },
    [62] = {
      name = "map_fixed32_fixed32",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapFixed32Fixed32Entry",
    },
    [63] = {
      name = "map_fixed64_fixed64",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapFixed64Fixed64Entry",
    },
    [64] = {
      name = "map_sfixed32_sfixed32",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSfixed32Sfixed32Entry",
    },
    [65] = {
      name = "map_sfixed64_sfixed64",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSfixed64Sfixed64Entry",
    },
    [66] = {
      name = "map_int32_float",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32FloatEntry",
    },
    [67] = {
      name = "map_int32_double",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32DoubleEntry",
    },
    [68] = {
      name = "map_bool_bool",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapBoolBoolEntry",
    },
    [69] = {
      name = "map_string_string",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringStringEntry",
    },
    [70] = {
      name = "map_string_bytes",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringBytesEntry",
    },
    [71] = {
      name = "map_string_nested_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringNestedMessageEntry",
    },
    [72] = {
      name = "map_string_foreign_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringForeignMessageEntry",
    },
    [73] = {
      name = "map_string_nested_enum",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringNestedEnumEntry",
    },
    [74] = {
      name = "map_string_foreign_enum",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      map = true,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringForeignEnumEntry",
    },
    [111] = {
      name = "oneof_uint32",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
    },
    [112] = {
      name = "oneof_nested_message",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.NestedMessage",
    },
    [113] = {
      name = "oneof_string",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [114] = {
      name = "oneof_bytes",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.BYTES,
    },
    [115] = {
      name = "oneof_bool",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
    },
    [116] = {
      name = "oneof_uint64",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
    },
    [117] = {
      name = "oneof_float",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FLOAT,
    },
    [118] = {
      name = "oneof_double",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.DOUBLE,
    },
    [119] = {
      name = "oneof_enum",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum
    },
    [401] = {
      name = "fieldname1",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [402] = {
      name = "field_name2",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [403] = {
      name = "_field_name3",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [404] = {
      name = "field__name4_",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [405] = {
      name = "field0name5",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [406] = {
      name = "field_0_name6",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [407] = {
      name = "fieldName7",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [408] = {
      name = "FieldName8",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [409] = {
      name = "field_Name9",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [410] = {
      name = "Field_Name10",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [411] = {
      name = "FIELD_NAME11",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [412] = {
      name = "FIELD_name12",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [413] = {
      name = "__field_name13",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [414] = {
      name = "__Field_name14",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [415] = {
      name = "field__name15",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [416] = {
      name = "field__Name16",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [417] = {
      name = "field_name17__",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [418] = {
      name = "Field_name18__",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.NestedMessage"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.NestedMessage",
  options = {},
  fields = {
    [1] = {
      name = "a",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [2] = {
      name = "corecursive",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3",
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32Int32Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32Int32Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapInt64Int64Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt64Int64Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT64,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT64,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapUint32Uint32Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapUint32Uint32Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapUint64Uint64Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapUint64Uint64Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.UINT64,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapSint32Sint32Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSint32Sint32Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapSint64Sint64Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSint64Sint64Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT64,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.SINT64,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapFixed32Fixed32Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapFixed32Fixed32Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FIXED32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FIXED32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapFixed64Fixed64Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapFixed64Fixed64Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.FIXED64,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.FIXED64,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapSfixed32Sfixed32Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSfixed32Sfixed32Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.SFIXED32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.SFIXED32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapSfixed64Sfixed64Entry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapSfixed64Sfixed64Entry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.SFIXED64,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.SFIXED64,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32FloatEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32FloatEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.FIXED32,
      type = ProtoSchema.DataType.FLOAT,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32DoubleEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapInt32DoubleEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.FIXED64,
      type = ProtoSchema.DataType.DOUBLE,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapBoolBoolEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapBoolBoolEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.BOOL,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapStringStringEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringStringEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapStringBytesEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringBytesEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.BYTES,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapStringNestedMessageEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringNestedMessageEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.TestAllTypesProto3.NestedMessage",
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapStringForeignMessageEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringForeignMessageEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.MESSAGE,
      subschema = "protobuf_test_messages.proto3.ForeignMessage",
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapStringNestedEnumEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringNestedEnumEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.TestAllTypesProto3.NestedEnum
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.TestAllTypesProto3.MapStringForeignEnumEntry"] = {
  name = "protobuf_test_messages.proto3.TestAllTypesProto3.MapStringForeignEnumEntry",
  options = {},
  fields = {
    [1] = {
      name = "key",
      wireType = ProtoSchema.WireType.LENGTH_DELIMITED,
      type = ProtoSchema.DataType.STRING,
    },
    [2] = {
      name = "value",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.ENUM, -- protobuf_test_messages.proto3.ForeignEnum
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.ForeignMessage"] = {
  name = "protobuf_test_messages.proto3.ForeignMessage",
  options = {},
  fields = {
    [1] = {
      name = "c",
      wireType = ProtoSchema.WireType.VARINT,
      type = ProtoSchema.DataType.INT32,
    },
  },
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.NullHypothesisProto3"] = {
  name = "protobuf_test_messages.proto3.NullHypothesisProto3",
  options = {},
  fields = {},
}

--- @type ProtoMessageSchema
ProtoSchema.Message["protobuf_test_messages.proto3.EnumOnlyProto3"] = {
  name = "protobuf_test_messages.proto3.EnumOnlyProto3",
  options = {},
  fields = {},
}


return ProtoSchema
