# lua-protobuf

A pure Lua implementation of Protocol Buffers encoding and decoding with **zero
external dependencies**. This library provides a lightweight, portable
implementation that runs on Lua 5.1, 5.2, 5.3, 5.4, and LuaJIT.

## Features

- **Zero Dependencies**: Pure Lua implementation, no C extensions or external
  libraries required
- **Portable**: Runs on any Lua interpreter (5.1+)
- **Schema-based**: Uses Lua tables as proto schemas for type-safe encoding/decoding
- **Schema Generator**: Includes tool to generate Lua schemas from `.proto` files
- **Complete Types**: Supports all standard protobuf wire types (varint, fixed32,
  fixed64, length-delimited, floats, doubles, zigzag encoding)
- **Well-tested**: Comprehensive self-tests with embedded test vectors

## Installation

Clone this repository:

```bash
git clone https://github.com/finitelabs/lua-protobuf.git
cd lua-protobuf
```

Add the `src` and `vendor` directories to your Lua path, or copy the files to
your project.

## Usage

### Workflow Overview

1. Define your `.proto` file (or use existing proto definitions)
2. Generate a Lua schema using `gen_lua_proto_schema`
3. Use the schema with `encode()` and `decode()` functions

### Generating a Schema

Use the included `gen_lua_proto_schema` tool to convert `.proto` files into Lua schemas.

#### Setup

The schema generator requires Python 3.8+ and `protoc`. Install all dependencies with:

```bash
# macOS - installs protoc, Python venv, and dependencies
make install-deps

# Ubuntu/Debian - install protoc manually, then run make
sudo apt-get install protobuf-compiler
make setup-schema-generator
```

#### Usage

The tool accepts local file paths and/or URLs as input:

```bash
# Generate schema from a local proto file
make gen-schema PROTO=input.proto OUTPUT=output_schema.lua

# Generate from multiple proto files
make gen-schema PROTO="api.proto api_options.proto" OUTPUT=output_schema.lua

# Generate from a remote proto file (URLs supported)
make gen-schema PROTO="https://example.com/api.proto" OUTPUT=output_schema.lua

# Mix local files and URLs
make gen-schema PROTO="local.proto https://example.com/remote.proto" OUTPUT=output_schema.lua
```

### Basic Example

```lua
local protobuf = require("protobuf")
local schema = require("my_proto_schema")

-- Encode a message
local message = {
  id = 123,
  name = "example",
  active = true
}
local encoded = protobuf.encode(schema, schema.Message.MyMessage, message)

-- Decode a message
local decoded = protobuf.decode(schema, schema.Message.MyMessage, encoded)
print(decoded.name)  -- "example"
```

### Supported Types

| Proto Type | Wire Type | Notes |
|------------|-----------|-------|
| int32, int64, uint32, uint64 | varint | 64-bit uses `{high, low}` pairs on Lua 5.1 |
| sint32, sint64 | varint | ZigZag encoded |
| fixed32, sfixed32 | 32-bit | Little-endian |
| fixed64, sfixed64 | 64-bit | Little-endian |
| float | 32-bit | IEEE 754 |
| double | 64-bit | IEEE 754 |
| bool | varint | |
| string, bytes | length-delimited | |
| enum | varint | |
| message | length-delimited | Nested messages |

### 64-bit Value Representation

On Lua 5.1 and LuaJIT (without 64-bit integer support), 64-bit values are
represented as `{high, low}` pairs where:
- `high` is the upper 32 bits
- `low` is the lower 32 bits

Example: `0x123456789ABCDEF0` is represented as `{0x12345678, 0x9ABCDEF0}`

Helper functions are provided:
```lua
-- Convert number to {high, low}
local int64 = protobuf.int64_from_number(1234567890123)

-- Convert {high, low} to number (may lose precision)
local num = protobuf.int64_to_number({0x00000001, 0xFFFFFFFF})

-- Convert to hex string
local hex = protobuf.int64_to_hex({0x12345678, 0x9ABCDEF0})
```

## Testing

Run the test suite:

```bash
# Run all tests
make test

# Run specific test suite
make test-protobuf

# Run test matrix across all Lua versions
make test-matrix
```

## Current Limitations

- Pure Lua performance is slower than native protobuf libraries
- No constant-time guarantees
- Packed repeated fields not yet supported
- Unknown fields are discarded during decode

## License

GNU Affero General Public License v3.0 - see LICENSE file for details

## Contributing

Contributions are welcome! Please ensure all tests pass and add new tests for
any new functionality.

---

<a href="https://www.buymeacoffee.com/derek.miller" target="_blank"><img src="https://cdn.buymeacoffee.com/buttons/v2/default-yellow.png" alt="Buy Me A Coffee" style="height: 60px !important;width: 217px !important;" ></a>
