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

# Full gate: format-check, lint, check-types, typecheck.
# Never rewrites files -- format-check fails instead.
make check

# Build single-file distributions
make build

# Generate schema from proto files
make gen-schema PROTO=input.proto OUTPUT=src/output.lua

# Regenerate base types from empty.proto
make gen-types

# Verify types.lua is up to date
make check-types
```

`make check` is the gate CI runs. `make all` is `format lint test build`, which
rewrites `src/` in place and runs none of `format-check`, `check-types` or
`typecheck` — it is not a substitute for `check`.

### typecheck

`make typecheck` runs lua-language-server against the committed
`.luarc-typecheck.json`. It catches what luacheck does not: undefined or duplicate
`@alias`, returns that disagree with `@return`, fields missing from a `@class`.

`--configpath` displaces each individual setting the committed config declares,
not each table, so a knob is only closed if it is named. Suppression keys can be
enumerated from the diagnostics read sites:

    grep -rhoE "config\.get\([^,]*, *'Lua\.[A-Za-z.]+'" \
      script/core/diagnostics/*.lua script/provider/diagnostic.lua

Treat that as a floor, not a ceiling: its file scope is the shape of its blind
spot. Anything that gates file loading or rewrites source before analysis is read
elsewhere, and has to be enumerated separately from `script/plugin.lua` and
`script/workspace.lua`. `runtime.plugin` is the case that matters, and the grep
cannot surface it by construction. `check_worker.lua` does `require 'plugin'`, so
an `OnSetText` returning an empty edit blanks every file in the repo and the check
passes having analysed nothing.

Two traps decide how a key gets declared, and neither is answered by the key's
type:

Empty is not always inert, so read the read site. `neededFileStatus` and
`groupFileStatus` are per-key lookups that fall back to the built-in default, so
`{}` leaves behaviour untouched. `enableScheme` defaults to `["file"]`, which makes
`[]` silence the whole check exactly as a local `["git"]` would. It is declared as
`["file"]` for that reason.

Immunity is per-code, so one planted probe does not measure a key.
`check_worker.lua`'s `downgrade_checks_to_opened` force-overwrites only codes whose
default status is `Any`, leaving everything defaulting to `Opened` under local
control, which is precisely the type-check group this gate exists for. An
`undefined-global` probe therefore reports `neededFileStatus` as inert while a
`return-type-mismatch` probe shows it silencing the check. Probe with a type-check
code.

Declared here as measured live bypasses: `enable`, `disable`, `severity`,
`globals`, `globalsRegex`, `enableScheme`, `neededFileStatus` and `groupFileStatus`
under `diagnostics`, plus `special` and `plugin` under `runtime`. `pluginArgs`,
`groupSeverity`, `maxPreload` and `preloadFileSize` are declared as belt and
braces rather than measured bypasses: `groupSeverity` relabels a finding that is
still counted and still exits non-zero, and `preloadFileSize: 0` fails loud rather
than hiding anything. Declaring them costs nothing and saves re-deriving that.

Any setting this file does not name, under any table, is still reachable from a
local `.luarc.json`. Re-run both enumerations when upgrading the server rather than
assuming this list stayed complete.

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
- `protobuf.decode(schema, message_schema, data)` - Decode protobuf binary to Lua
  table. Returns **two** values, `message, pos`.
- `protobuf.version()` - Build-injected version string
- `protobuf.selftest()` - Run embedded test suite

**Every failure raises.** There is no `nil, err` path anywhere in encode or
decode; callers must `pcall`.

### What is not implemented

This is the section to read before assuming a `.proto` will round-trip:

- **Packed repeated fields are unsupported in both directions.** Encode emits a
  tag per element; decode has no packed branch. proto3 packs scalar repeated
  fields *by default*, so a message produced by `protoc` decodes to a single raw
  byte-string in the list rather than the values. This is the most likely source
  of a silent wrong answer.
- **`oneof`, `map`, field defaults and `required` are not implemented.** Absent
  fields decode to `nil` with no default applied; nothing enforces `required`.
- **Unknown fields are skipped, not preserved.** Re-encoding a decoded message
  drops them.
- **Groups are unsupported.** `DataType` has no `GROUP` (10) and `WireType` has no
  SGROUP (3) / EGROUP (4); both raise `"Unknown wire type"`.
- **The schema generator only walks top-level messages.** `nested_type` is never
  emitted, and messages are registered under their bare name while fields
  reference `<package>.<Message>`. Any `.proto` with a `package` declaration or a
  nested message produces a schema whose subschema lookup misses. `empty.proto`
  avoids this only by being empty.

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
- **WireType**: VARINT (0), FIXED64 (1), LENGTH_DELIMITED (2), FIXED32 (5).
  SGROUP (3) and EGROUP (4) are absent.
- **DataType**: 17 of the 18 standard types. `GROUP` (10) is absent.

### 64-bit Representation

64-bit values use `{high, low}` pairs for Lua 5.1/LuaJIT compatibility, but the
pair is **not a plain table** — `bit64` attaches a private metatable and
`is_int64` tests for it:

```lua
-- Correct:
local value = bit64.new(0x12345678, 0x9ABCDEF0)
local also  = pb.int64_from_number(n)

-- Wrong: a bare literal is classified as a list, and encoding fails with
-- "Field '...' is not repeated but received a list."
local bad = {0x12345678, 0x9ABCDEF0}
```

This is easy to miss because `int64_to_hex`, `equals` and `is_zero` *do* accept
plain pairs; only the encode path rejects them.

Decode is asymmetric and this is the thing most often got wrong: INT64, UINT64,
SINT64, FIXED64 and SFIXED64 come back as Int64 **tables**, while INT32, UINT32,
ENUM, BOOL and FIXED32 come back as plain numbers. `pb.decode_varint` silently
truncates beyond 53 bits.

### Vendor Dependencies

The `vendor/bitn.lua` file is a vendored copy of the [lua-bitn](https://github.com/finitelabs/lua-bitn) library providing portable bitwise operations. Import as `require("bitn")` (not `vendor.bitn`).

## Testing

Tests are embedded in `src/protobuf/init.lua` as a `selftest()` function. The test runner invokes this function and reports results.

Run with: `./run_tests.sh` or `make test`

## Schema Generation

The `tools/gen_lua_proto_schema` Python script converts `.proto` files to Lua schemas:

```bash
# Setup (one-time; needs python3 and protoc). Creates .venv/
make setup-schema-generator

# Generate schema. OUTPUT must contain a directory component --
# a bare "schema.lua" raises FileNotFoundError.
make gen-schema PROTO=api.proto OUTPUT=src/schema.lua

# Regenerate the base types, then COMMIT the result
make gen-types
```

`src/protobuf/types.lua` is **tracked, not generated at build time** — only
`build/` and `.venv/` are gitignored. `make check-types` regenerates and diffs,
failing on stale output, so editing `empty.proto` without committing the
regenerated `types.lua` breaks CI.

Two traps follow from `check-types` being part of `check`:

- **`make check` fails on a fresh clone** until `make setup-schema-generator` has
  run, because `check-types` hard-requires `.venv/bin/python3`.
- **`make clean` removes `.venv/`**, so `make clean && make check` breaks the same
  way.

Only the *first* `PROTO` file is passed to `protoc`; additional ones are fetched
and then ignored unless the first imports them. The generator also wraps
generation in a bare `except` that prints the error and still exits 0, so check
the output file rather than the exit status.

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

- **build.yml**: Runs on push/PR to `main` or `master`
  - `check` job — `make check`: format-check, luacheck, check-types, and typecheck
    against lua-language-server 3.19.0
  - `test` job — `make test-all` across Lua 5.1-5.4, LuaJIT 2.0/2.1
  - `build` job — single-file distributions
  - The `luajit-2.1` matrix entry is silently built as **`luajit-openresty`**:
    rolling LuaJIT HEAD miscompiled the signed arithmetic-shift edge cases in
    zigzag encoding (arshift of INT_MIN). The job name is kept as `Lua luajit-2.1`
    so the required status check still matches, so the matrix does not test what
    its label says.
- **release.yml**: on version tags (`v*`) — publishes both `build/protobuf.lua`
  and `build/protobuf-portable.lua`.

`make test-matrix` locally pins `5.1.5 5.2.4 5.3.6 5.4.8 luajit-2.1-dev` and needs
`luaenv` plus the `luaenv-luarocks` plugin. It does **not** cover LuaJIT 2.0,
which CI does.

## Code Style

- 2-space indentation
- 120 column width
- Double quotes preferred
- LuaCATS annotations on public functions

There is no `.stylua.toml`; these live only as CLI flags in the Makefile and cover
`src/` only.
- LuaDoc annotations for public functions