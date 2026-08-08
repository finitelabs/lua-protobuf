# lua-protobuf Development Guide

## Project Structure

```
lua-protobuf/
├── src/protobuf/
│   ├── init.lua      # Main module with encode/decode and embedded selftest
│   └── types.lua     # Generated base type definitions (from empty.proto)
├── vendor/
│   └── bitn.lua      # Vendored bitwise operations library
├── tools/
│   ├── gen_lua_proto_schema  # Python script to generate Lua schemas from .proto
│   └── requirements.txt      # Python dependencies for schema generator
├── .github/workflows/
│   └── build.yml     # CI: check, test matrix, build
├── empty.proto       # Empty proto for generating base types
├── run_tests.sh      # Main test runner
├── run_tests_matrix.sh   # Multi-version test runner
└── Makefile          # Build automation
```

## Key Commands

```bash
# Run tests
make test

# Run across Lua versions
make test-matrix

# Format code
make format

# Lint code
make lint

# Run all quality checks (format, lint, check-types, typecheck)
make check

# Build single-file distributions
make build

# Generate schema from proto files
make gen-schema PROTO=input.proto OUTPUT=output.lua

# Regenerate base types from empty.proto
make gen-types

# Verify types.lua is up to date
make check-types
```

### typecheck

`make typecheck` runs lua-language-server against the committed
`.luarc-typecheck.json`. It catches what luacheck does not: undefined or duplicate
`@alias`, returns that disagree with `@return`, fields missing from a `@class`.

`--configpath` displaces each individual setting the committed config declares,
not each table, so a suppression knob is only closed if it is named. `diagnostics`
therefore declares four: `enable`, `disable`, `severity` and `globals`. Each was
measured as a live bypass with a planted probe, `enable: false` silencing the check
entirely and the rest suppressing individual codes, and each is a no-op on a clean
tree. Anything under `diagnostics` not in that list is still reachable from a local
`.luarc.json`, so add it here rather than assume the list is complete.

The server version is not pinned locally, though. `install-deps` takes whatever
Homebrew has while CI pins 3.19.0, so compare the version the target prints if a
local result disagrees with CI.

`vendor/` is both a `library` and an `ignoreDir`, which is load-bearing: with only
`ignoreDir` the vendored definitions are lost and their uses become
`undefined-doc-name`, and with only `library` the vendored code is diagnosed here.

`runtime.version` is pinned to LuaJIT because that is what Control4 runs, and
here it is also load-bearing for the check itself: unset, the server assumes Lua
5.4 and reports the `math.frexp` and `math.ldexp` polyfill reads in `init.lua` as
deprecated, which fails the gate. So the pin keeps this repo's own cross-version
shims from tripping it.

Part of `check`, so CI enforces it. CI pins the server version so the count cannot
move under an upstream release; 3.18.2 and 3.19.0 agree here.

## Architecture

### Module Design

The library provides Protocol Buffers encoding/decoding with these main functions:

- `protobuf.encode(schema, message_schema, data)` - Encode Lua table to protobuf binary
- `protobuf.decode(schema, message_schema, data)` - Decode protobuf binary to Lua table
- `protobuf.selftest()` - Run embedded test suite

### Schema Structure

Schemas are generated from `.proto` files and contain:

```lua
local schema = {
  Enum = {},      -- Enum definitions
  Message = {},   -- Message definitions with field schemas
  RPC = {},       -- Service/method definitions
  WireType = {},  -- Wire type constants
  DataType = {},  -- Data type constants
}
```

### Wire Types and Data Types

Defined in `src/protobuf/types.lua`:
- **WireType**: VARINT (0), FIXED64 (1), LENGTH_DELIMITED (2), FIXED32 (5)
- **DataType**: All standard protobuf types (DOUBLE, FLOAT, INT32, INT64, STRING, MESSAGE, etc.)

### 64-bit Representation

64-bit values use `{high, low}` pairs for Lua 5.1/LuaJIT compatibility:
```lua
-- 0x123456789ABCDEF0 represented as:
local value = {0x12345678, 0x9ABCDEF0}
```

Uses `bitn.bit64` for 64-bit operations with `bit64.new(high, low)` constructor.

### Vendor Dependencies

The `vendor/bitn.lua` file is a vendored copy of the [lua-bitn](https://github.com/finitelabs/lua-bitn) library providing portable bitwise operations. Import as `require("bitn")` (not `vendor.bitn`).

## Testing

Tests are embedded in `src/protobuf/init.lua` as a `selftest()` function. The test runner invokes this function and reports results.

Run with: `./run_tests.sh` or `make test`

## Schema Generation

The `tools/gen_lua_proto_schema` Python script converts `.proto` files to Lua schemas:

```bash
# Setup (one-time)
make setup-schema-generator

# Generate schema
make gen-schema PROTO=api.proto OUTPUT=src/schema.lua

# Supports URLs and multiple files
make gen-schema PROTO="local.proto https://example.com/remote.proto" OUTPUT=schema.lua
```

## Building

The build process uses `amalg` to create single-file distributions:

```bash
make build
# Output:
#   build/protobuf.lua          - Core (canonical); requires external bitn on the path
#   build/protobuf-portable.lua - Portable; bitn bundled, zero external deps
```

Version is automatically injected from git tags during release.

## CI/CD

- **build.yml**: Runs on push/PR to main
  - Format check with stylua
  - Lint with luacheck
  - Verify types.lua is up to date
  - Test matrix (Lua 5.1-5.4, LuaJIT 2.0/2.1)
  - Build single-file distributions

## Code Style

- 2-space indentation
- 120 column width
- Double quotes preferred
- LuaDoc annotations for public functions