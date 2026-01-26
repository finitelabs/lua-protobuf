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

return ProtoSchema
