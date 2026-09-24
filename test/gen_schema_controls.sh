#!/usr/bin/env bash
#
# Controls for tools/gen_lua_proto_schema: message options round-trip into the generated Lua by
# their declared type, and a failed generation exits non-zero without touching the output file.

set -u

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
gen="$root/tools/gen_lua_proto_schema"
python="$root/.venv/bin/python3"
lua=${LUA_BINARY:-lua}

work=$(mktemp -d) || exit 1
trap 'rm -rf "$work"' EXIT

failed=0

ok() { printf '  ok    %s\n' "$1"; }

bad() {
  failed=$((failed + 1))
  printf '  FAIL  %s\n' "$1"
  printf '        %s\n' "$2"
}

# Generates schema from a proto, then runs the Lua on stdin against it as arg[1].
expect_round_trip() {
  local label=$1 proto=$2 msg
  out="$work/roundtrip.lua"
  if ! "$python" "$gen" "$out" "$proto" > /dev/null 2> "$work/err"; then
    bad "$label" "generation failed: $(tr '\n' ' ' < "$work/err")"
  elif msg=$("$lua" - "$out" 2>&1); then
    ok "$label"
  else
    bad "$label" "$msg"
  fi
}

expect_failure() {
  local label=$1 proto=$2 needle=$3
  out="$work/stale.lua"
  echo "-- stale schema" > "$out"
  cp "$out" "$work/before"
  if "$python" "$gen" "$out" "$proto" > /dev/null 2> "$work/err"; then
    bad "$label" "expected a non-zero exit, got 0"
  elif ! cmp -s "$out" "$work/before"; then
    bad "$label" "the output file changed"
  elif ! grep -qF -- "$needle" "$work/err"; then
    bad "$label" "stderr lacks '$needle': $(tr '\n' ' ' < "$work/err")"
  else
    ok "$label"
  fi
}

# Writes stdin, after a proto2 header importing descriptor.proto, to $work/<name>.proto.
write_proto() {
  { printf 'syntax = "proto2";\nimport "google/protobuf/descriptor.proto";\n'; cat; } > "$work/$1.proto"
}

echo "gen_lua_proto_schema controls"

expect_round_trip "fixed-width options round-trip" "$root/test/fixed_options.proto" << 'LUA'
local options = dofile(arg[1]).Message["Fixed"].options
local expected = {
  opt_double = 1.5,
  opt_float = 0.25,
  opt_fixed32 = 4294967295,
  opt_sfixed32 = -2,
  opt_fixed64 = 4294967296,
  opt_sfixed64 = -4294967296,
  opt_varint = 7,
}
for name, want in pairs(expected) do
  if options[name] ~= want then
    error(string.format("%s = %s, expected %s", name, tostring(options[name]), tostring(want)), 0)
  end
end
LUA

expect_round_trip "varint, string and bytes options round-trip by declared type" \
  "$root/test/typed_options.proto" << 'LUA'
local file = assert(io.open(arg[1], "rb"))
local text = file:read("*a")
file:close()
for i = 1, #text do
  local byte = text:byte(i)
  if (byte < 32 and byte ~= 10) or byte == 127 then
    error(string.format("unescaped control byte %d at offset %d", byte, i), 0)
  end
end
local options = dofile(arg[1]).Message["Typed"].options
local expected = {
  opt_true = true,
  opt_false = false,
  opt_int32 = -2,
  opt_int64 = -3,
  opt_sint32 = -4,
  opt_sint64 = -5,
  opt_uint64 = 6,
  opt_enum = 2,
  opt_string = "q\"b\\s\r\n\t\0012\127\195\169",
  opt_bytes = "\255\000z",
  opt_int64_limit = 9007199254740992,
  opt_sfixed64_limit = -9007199254740992,
}
for name, want in pairs(expected) do
  if options[name] ~= want then
    error(string.format("%s = %q, expected %q", name, tostring(options[name]), tostring(want)), 0)
  end
end
LUA

expect_round_trip "infinite options round-trip as math.huge" "$root/test/nonfinite_option.proto" << 'LUA'
local options = dofile(arg[1]).Message["NonFinite"].options
local expected = {
  opt_inf = math.huge,
  opt_neg_inf = -math.huge,
  opt_float_inf = math.huge,
  opt_float_neg_inf = -math.huge,
}
for name, want in pairs(expected) do
  if options[name] ~= want then
    error(string.format("%s = %s, expected %s", name, tostring(options[name]), tostring(want)), 0)
  end
end
LUA

# protoc 3.x cannot express a NaN option, so the NaN literal is rendered from the generator directly.
nan="$work/nan.lua"
if ! "$python" - "$gen" > "$nan" 2> "$work/err" << 'PY'
import importlib.machinery, importlib.util, struct, sys
from google.protobuf.descriptor_pb2 import FieldDescriptorProto as F

loader = importlib.machinery.SourceFileLoader("gen", sys.argv[1])
gen = importlib.util.module_from_spec(importlib.util.spec_from_loader("gen", loader))
loader.exec_module(gen)
nan = float("nan")
double = gen.option_literal("d", F.TYPE_DOUBLE, "fixed", struct.pack("<d", nan))
single = gen.option_literal("f", F.TYPE_FLOAT, "fixed", struct.pack("<f", nan))
print(f"return {{ {double}, {single} }}")
PY
then
  bad "NaN options round-trip as (0/0)" "$(tr '\n' ' ' < "$work/err")"
elif msg=$("$lua" - "$nan" 2>&1 << 'LUA'
local values = dofile(arg[1])
for i = 1, 2 do
  if type(values[i]) ~= "number" or values[i] == values[i] then
    error(string.format("value %d = %s, expected NaN", i, tostring(values[i])), 0)
  end
end
LUA
); then
  ok "NaN options round-trip as (0/0)"
else
  bad "NaN options round-trip as (0/0)" "$msg"
fi

expect_failure "group option fails and leaves the output" "$root/test/group_option.proto" \
  "Option optgroup is TYPE_GROUP"

write_proto message_option << 'PROTO'
message Inner { optional int32 a = 1; }
extend google.protobuf.MessageOptions { optional Inner opt_message = 50201; }
message WithMessageOption { option (opt_message) = { a: 1 }; }
PROTO
expect_failure "message option fails and leaves the output" "$work/message_option.proto" \
  "Option opt_message is TYPE_MESSAGE"

write_proto group_field << 'PROTO'
message WithGroup {
  optional group Item = 1 { optional int32 a = 1; }
}
PROTO
expect_failure "group field fails and leaves the output" "$work/group_field.proto" \
  "Field WithGroup.item is a group"

for case in "int64 INT64 9007199254740993" "uint64 UINT64 9007199254740993" \
  "sint64 SINT64 -9007199254740993" "fixed64 FIXED64 9007199254740993" \
  "sfixed64 SFIXED64 -9007199254740993"; do
  set -- $case
  write_proto "wide_$1" << PROTO
extend google.protobuf.MessageOptions { optional $1 opt_wide = 50301; }
message Wide { option (opt_wide) = $3; }
PROTO
  expect_failure "$1 option beyond 2^53 fails and leaves the output" "$work/wide_$1.proto" \
    "Option opt_wide = $3 (TYPE_$2) exceeds 2^53"
done

[ "$failed" -eq 0 ] || { echo "$failed control(s) failed" >&2; exit 1; }
