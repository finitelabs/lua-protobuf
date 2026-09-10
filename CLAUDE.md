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
│   ├── check_schema_refs.lua # Asserts a generated schema resolves its subschemas
│   └── requirements.txt      # Python dependencies for schema generator
├── test/
│   ├── nested.proto  # Fixture: nested types, packages, services (check-schema)
│   ├── maps.proto    # Fixture: map fields and their synthesized entries (check-schema)
│   ├── test_messages_proto3.proto  # Trimmed upstream conformance message
│   ├── wire_vectors_test.lua       # Differential wire-format suite
│   ├── known_gaps.lua              # Vectors that fail today, with tickets
│   └── generated/    # Checked-in generated schema and goldens (not typechecked)
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

# Full gate: format-check, lint, check-types, check-schema, typecheck.
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

# Verify generated schemas resolve every subschema they reference
make check-schema
```

`make check` is the gate CI runs. `make all` is `format lint test build`, which
rewrites `src/` in place and runs none of `format-check`, `check-types`,
`check-schema` or `typecheck` — it is not a substitute for `check`.

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

- **The encoder never packs.** It emits one tag per element even for a proto3
  scalar `repeated` field, which `protoc` would pack. The bytes are valid and any
  conformant parser reads them, but they will not match a reference capture.
  Decode accepts both spellings; see Packed Repeated Fields below.
- **`oneof`, field defaults and `required` are not implemented.** Absent fields
  decode to `nil` with no default applied; nothing enforces `required`. Map
  entries are the one exception, covered under Map Fields below.
- **Unknown fields are skipped, not preserved.** Re-encoding a decoded message
  drops them.
- **Groups are unsupported.** `DataType` has no `GROUP` (10) and `WireType` has no
  SGROUP (3) / EGROUP (4); both raise `"Unknown wire type"`.
- **A negative `int32` or `enum` decodes differently per interpreter.** The wire
  form is a ten-byte sign-extended varint, and `bit64.to_number` is unsigned
  (`value[1] * 0x100000000 + value[2]`). On 5.3+ that multiply overflows signed
  64-bit integers and wraps to the correct answer; on 5.1, 5.2 and LuaJIT the
  operands are doubles, nothing wraps, and `-1` reads as `1.8446744073709552e19`.
  Four of the six CI matrix entries are the second kind. Note that a current
  Homebrew `lua` is 5.5, which is **not** in the matrix and does wrap, so this is
  invisible locally. Tracked as FL-19.
- **`sfixed32` is unsigned in both directions.** Decode returns `4294967295` for
  the wire bytes `FFFFFFFF` where the reference returns `-1`, and encoding a
  negative one raises `bad argument #4 to 'char'`. `sfixed64` is unaffected: its
  `{high, low}` pair carries the two's-complement bits, so signedness is the
  caller's interpretation rather than the codec's choice. Tracked as FL-18.
- **Subnormal floats and doubles are wrong in both directions.** Decode applies
  the implicit leading 1 unconditionally, so `01000000` reads as `5.88e-39`
  instead of `1.40e-45`; encode clamps the exponent to 0 with a zero mantissa, so
  any subnormal flushes to zero. NaN, the infinities and negative zero are
  handled. Tracked as FL-16.
- **The `frexp` fallback is not exact.** `math_frexp` falls back to a `math.log`
  computation when `math.frexp` is absent, and that fallback can return a
  mantissa of exactly 1.0, which encodes some normal doubles a factor of two too
  small. Every CI target has a native `math.frexp`, so nothing exercises it.

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

`Message` and `Enum` are keyed by **fully-qualified** protobuf name, meaning package
and enclosing messages included, dot separated. That is the form `type_name` already
gives a field's `subschema`, and `decode` resolves a subschema by indexing `Message`
with it directly. So `test/nested.proto`'s doubly-nested `Deep` registers as
`Message["fixture.nested.Outer.Inner.Deep"]`, not `Message.Deep`.

For a `.proto` with no `package` and no nested types the qualified name equals the
bare name, which is why `api.proto`-derived schemas are unaffected by the rule.

The LuaDoc `@class` names stay short (`ProtoBindingRecord`): a class name cannot
contain dots, and `@field` annotations are emitted from the same short form.

### Map Fields

A `map<K, V>` field carries `map = true` **instead of** `repeated = true`, and its
`subschema` names the `<Field>Entry` message protoc synthesizes. Those entries are
registered in `Message` like any other type, because that is genuinely what a map
is on the wire: repeated length-delimited entries with the key at field 1 and the
value at field 2.

In Lua the field is a table keyed by the protobuf key, **not** a list of entries:

```lua
{ counts = { alpha = 1, beta = 2 } }   -- not { { key = "alpha", value = 1 }, ... }
```

Two consequences worth knowing:

- **`repeated` is absent on a map field**, so anything branching on `repeated` to
  decide list-ness reads a map as a singular field. Branch on `map` first.
- **A map entry applies key and value defaults**, unlike every other field, which
  decodes absent as `nil`. A producer may drop either side of an entry when it
  holds the zero value, and a missing key would otherwise index the destination
  table with `nil` and raise. `map<int64, …>` keys arrive as Int64 **tables**, per
  the decode asymmetry below, which makes them useless for lookup by value.

### Packed Repeated Fields

proto3 packs scalar `repeated` fields *by default*: a single length-delimited
block holds the elements concatenated, with no per-element tags. Packing applies
to scalars only, so a `repeated` message field is length-delimited per element
and is never packed.

Decode accepts every spelling a producer may emit, and appends rather than
replaces, because one field may legally arrive as several blocks or mix the two
forms:

- packed, as one block
- unpacked, as one tag per element
- split across several blocks, or packed and unpacked in the same message

A packed element and an unpacked one read through the same type dispatch in
`decode_scalar`, so zigzag, bool, Int64, float and double cannot drift apart
between the two paths.

Encode emits only the unpacked spelling. Re-encoding a decoded message therefore
does not reproduce the original bytes when the producer packed them, which is
worth knowing before diffing encoder output against a capture.

Before v0.6.7 decode had no packed branch and read the whole block as one
LENGTH_DELIMITED value, so a `protoc`-produced message decoded to a single raw
byte-string in the list rather than the values.

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

Three modules, all driven by `./run_tests.sh` or `make test`:

- **protobuf** — the embedded `selftest()` in `src/protobuf/init.lua`.
- **math-fallback** — `test/math_fallback_test.lua`, differential against the
  interpreter's native `frexp`/`ldexp`.
- **wire-vectors** — `test/wire_vectors_test.lua`, differential against the
  reference protobuf implementation.

Each runs twice, once with native `math.frexp`/`math.ldexp` and once with them
cleared so the module's own fallbacks are bound.

### Wire vectors

`test/generated/wire_vectors.lua` holds golden bytes produced by the reference
implementation from `test/test_messages_proto3.proto`, paired with the table the
decoder should produce. Both sides are derived by walking the reference message,
so neither is hand-computed — which matters, because three hand-written
expectations in the FL-16 Part 1 work were themselves wrong.

The goldens are **checked in**, so `make test` needs no Python and runs on a bare
clone. `make gen-wire-vectors` regenerates them and `make check-wire-vectors`
fails on drift. Neither is part of `make check`: Python is genuinely required
here, and gating `check` on it would re-create the fresh-clone and `make clean`
trap that `check-types` already has.

The two directions are asserted differently, and the asymmetry is deliberate:

- **reference → Lua** is strict. The input is byte-exact.
- **Lua → reference** is semantic, never bytewise. This encoder never packs, so
  its output will not match `protoc`'s for any repeated scalar even when it is
  correct. The Lua suite re-decodes the encoder's own output; `make
  check-wire-vectors` additionally parses those bytes with the reference
  implementation and compares messages, which normalises packing, field order
  and map order away.

`test/known_gaps.lua` lists vectors that do not agree with the reference today,
each with its ticket. It is a machine-checked version of "What is not
implemented" above, and it should only ever shrink. Two categories:

- **`strict`** must fail. A listed case that starts passing **fails the run**, so
  an entry can only be removed in the change that fixes the defect.
- **`version_dependent`** may do either, because the outcome depends on the
  interpreter's number model. FL-19 is correct on 5.3 and 5.4 and wrong on 5.1,
  5.2 and LuaJIT, so a strict entry would just move which half of the matrix is
  red. These are printed on every run — `agrees here` or `differs here` — so the
  split stays visible instead of becoming a silent exclusion.

Run the suite under more than one interpreter before trusting it. `make test`
uses whatever `lua` resolves to, which on a current Homebrew is 5.5 and is not a
matrix entry; `LUA_BINARY=luajit ./run_tests.sh` is the cheapest second opinion
and is what surfaced FL-19.

`test/generated/` is in `typecheck`'s `ignoreDir`. The schema generator emits the
shared `@class ProtoSchema` and `ProtoFieldSchema` blocks into every file it
produces, so a second generated schema in the workspace collides with
`src/protobuf/types.lua` and reports 32 `duplicate-doc-field` warnings.

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

**Well-known types do not survive.** `--include_imports` is passed and protoc does
deliver them, but `parse_descriptor_set` skips any file whose package is
`google.protobuf`, in all four of its loops. So a `.proto` importing
`google/protobuf/timestamp.proto` generates a field whose `subschema` names
`google.protobuf.Timestamp` and no message to resolve it against, and
`check-schema` fails. `test/test_messages_proto3.proto` is vendored with those
fields trimmed for that reason. Tracked as FL-17.

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
  - `check` job — `make check`: format-check, luacheck, check-types, check-schema,
    and typecheck against lua-language-server 3.19.0
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