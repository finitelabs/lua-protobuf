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

passed=0
failed=0

ok() {
  passed=$((passed + 1))
  printf '  ok    %s\n' "$1"
}

bad() {
  failed=$((failed + 1))
  printf '  FAIL  %s\n' "$1"
  printf '        %s\n' "$2"
}

# Generates schema from a proto, then runs the Lua on stdin against it as arg[1].
expect_round_trip() {
  local label=$1 proto=$2 out="$work/roundtrip.lua" msg
  if ! "$python" "$gen" "$out" "$proto" > /dev/null 2> "$work/err"; then
    bad "$label" "generation failed: $(tr '\n' ' ' < "$work/err")"
  elif msg=$("$lua" - "$out" 2>&1); then
    ok "$label"
  else
    bad "$label" "$msg"
  fi
}

expect_failure() {
  local label=$1 proto=$2 needle=$3 out="$work/stale.lua"
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

write_proto fixed << 'PROTO'
extend google.protobuf.MessageOptions {
  optional double opt_double = 50001;
  optional float opt_float = 50002;
  optional fixed32 opt_fixed32 = 50003;
  optional sfixed32 opt_sfixed32 = 50004;
  optional fixed64 opt_fixed64 = 50005;
  optional sfixed64 opt_sfixed64 = 50006;
  optional uint32 opt_varint = 50007;
}
message Fixed {
  option (opt_double) = 1.5;
  option (opt_float) = 0.25;
  option (opt_fixed32) = 4294967295;
  option (opt_sfixed32) = -2;
  option (opt_fixed64) = 4294967296;
  option (opt_sfixed64) = -4294967296;
  option (opt_varint) = 7;
}
PROTO
expect_round_trip "fixed-width options round-trip" "$work/fixed.proto" << 'LUA'
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

write_proto typed << 'PROTO'
enum Color {
  RED = 0;
  GREEN = 2;
}
enum Sign {
  NEG = -1;
  ZERO = 0;
}
extend google.protobuf.MessageOptions {
  optional bool opt_true = 50101;
  optional bool opt_false = 50102;
  optional int32 opt_int32 = 50103;
  optional int64 opt_int64 = 50104;
  optional sint32 opt_sint32 = 50105;
  optional sint64 opt_sint64 = 50106;
  optional uint64 opt_uint64 = 50107;
  optional Color opt_enum = 50108;
  optional string opt_string = 50109;
  optional bytes opt_bytes = 50110;
  optional int64 opt_int64_limit = 50111;
  optional sfixed64 opt_sfixed64_limit = 50112;
  optional Sign opt_neg_enum = 50113;
}
message Typed {
  option (opt_true) = true;
  option (opt_false) = false;
  option (opt_int32) = -2;
  option (opt_int64) = -3;
  option (opt_sint32) = -4;
  option (opt_sint64) = -5;
  option (opt_uint64) = 6;
  option (opt_enum) = GREEN;
  option (opt_string) = "q\"b\\s\r\n\t\0012\177\303\251";
  option (opt_bytes) = "\377\000\200z";
  option (opt_int64_limit) = 9007199254740992;
  option (opt_sfixed64_limit) = -9007199254740992;
  option (opt_neg_enum) = NEG;
}
PROTO
expect_round_trip "varint, string and bytes options round-trip by declared type" \
  "$work/typed.proto" << 'LUA'
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
  opt_bytes = "\255\000\128z",
  opt_int64_limit = 9007199254740992,
  opt_sfixed64_limit = -9007199254740992,
  opt_neg_enum = -1,
}
for name, want in pairs(expected) do
  if options[name] ~= want then
    error(string.format("%s = %q, expected %q", name, tostring(options[name]), tostring(want)), 0)
  end
end
LUA

# protoc 3.x rejects `inf` and `nan` identifiers; an overflowing literal still parses to infinity.
write_proto nonfinite << 'PROTO'
extend google.protobuf.MessageOptions {
  optional double opt_inf = 50001;
  optional double opt_neg_inf = 50002;
  optional float opt_float_inf = 50003;
  optional float opt_float_neg_inf = 50004;
  optional double opt_neg_zero = 50005;
  optional float opt_float_neg_zero = 50006;
}
message NonFinite {
  option (opt_inf) = 1e999;
  option (opt_neg_inf) = -1e999;
  option (opt_float_inf) = 1e999;
  option (opt_float_neg_inf) = -1e999;
  option (opt_neg_zero) = -0.0;
  option (opt_float_neg_zero) = -0.0;
}
PROTO
expect_round_trip "infinite and negative-zero options round-trip" "$work/nonfinite.proto" << 'LUA'
local file = assert(io.open(arg[1], "rb"))
local text = file:read("*a")
file:close()
-- Lua 5.1 loads a -0.0 literal as +0 when an earlier 0 constant is in the chunk; CI runs 5.4.
if text:find("= %-0%.0,") then
  error("negative zero emitted as a -0.0 literal", 0)
end
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
for _, name in ipairs({ "opt_neg_zero", "opt_float_neg_zero" }) do
  if options[name] ~= 0 or 1 / options[name] ~= -math.huge then
    error(string.format("%s = %s, expected -0", name, tostring(options[name])), 0)
  end
end
LUA

# protoc 3.x cannot express a NaN option, so the NaN literal is rendered from the generator directly.
nan="$work/nan.lua"
if ! "$python" - "$gen" > "$nan" 2> "$work/err" << 'PY'
import struct, sys
from google.protobuf.descriptor_pb2 import FieldDescriptorProto as F

gen = type(sys)("gen_lua_proto_schema")
exec(compile(open(sys.argv[1]).read(), sys.argv[1], "exec"), gen.__dict__)
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

write_proto packaged << 'PROTO'
package acme;
extend google.protobuf.MessageOptions { optional uint32 opt_id = 50401; }
message Packaged { option (opt_id) = 7; }
PROTO
expect_round_trip "a packaged option name round-trips as a bracketed key" "$work/packaged.proto" << 'LUA'
local value = dofile(arg[1]).Message["acme.Packaged"].options["acme.opt_id"]
if value ~= 7 then
  error(string.format("acme.opt_id = %s, expected 7", tostring(value)), 0)
end
LUA

write_proto keyword << 'PROTO'
extend google.protobuf.MessageOptions { optional bool end = 50402; }
message Keyword { option (end) = true; }
PROTO
expect_round_trip "a keyword option name round-trips as a bracketed key" "$work/keyword.proto" << 'LUA'
local value = dofile(arg[1]).Message["Keyword"].options["end"]
if value ~= true then
  error(string.format("end = %s, expected true", tostring(value)), 0)
end
LUA

write_proto group_option << 'PROTO'
extend google.protobuf.MessageOptions {
  optional group OptGroup = 50010 { optional int32 a = 1; }
}
message Grouped { option (optgroup) = { a: 1 }; }
PROTO
expect_failure "group option fails and leaves the output" "$work/group_option.proto" \
  "Option optgroup is TYPE_GROUP"

write_proto message_option << 'PROTO'
message Inner { optional int32 a = 1; }
extend google.protobuf.MessageOptions { optional Inner opt_message = 50201; }
message WithMessageOption { option (opt_message) = { a: 1 }; }
PROTO
expect_failure "message option fails and leaves the output" "$work/message_option.proto" \
  "Option opt_message is TYPE_MESSAGE"

# protoc before 23 serializes a packed option unpacked, so the packed path is reached through the
# generator directly.
if "$python" - "$gen" > /dev/null 2> "$work/err" << 'PY'
import sys
from google.protobuf.descriptor_pb2 import FieldDescriptorProto as F

gen = type(sys)("gen_lua_proto_schema")
exec(compile(open(sys.argv[1]).read(), sys.argv[1], "exec"), gen.__dict__)
gen.option_literal("opt_packed", F.TYPE_INT32, "bytes", b"\x01\x02")
PY
then
  bad "packed repeated option fails as packed" "expected a ValueError, got none"
elif ! grep -qF "Option opt_packed is a packed repeated TYPE_INT32 field" "$work/err"; then
  bad "packed repeated option fails as packed" "$(tr '\n' ' ' < "$work/err")"
else
  ok "packed repeated option fails as packed"
fi

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

total=$((passed + failed))
echo
if [ "$failed" -eq 0 ]; then
  echo "gen_lua_proto_schema: $passed/$total controls passed"
  exit 0
fi
echo "gen_lua_proto_schema: $failed/$total controls FAILED"
exit 1
