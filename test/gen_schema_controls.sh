#!/usr/bin/env bash
#
# Controls for tools/gen_lua_proto_schema: fixed-width message options round-trip into the
# generated Lua, and a failed generation exits non-zero without touching the output file.

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

echo "gen_lua_proto_schema controls"

out="$work/fixed.lua"
if ! "$python" "$gen" "$out" "$root/test/fixed_options.proto" > /dev/null 2> "$work/err"; then
  bad "fixed-width options generate" "$(tr '\n' ' ' < "$work/err")"
elif msg=$("$lua" - "$out" 2>&1 << 'LUA'
local schema = dofile(arg[1])
local options = schema.Message["Fixed"].options
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
); then
  ok "fixed-width options round-trip"
else
  bad "fixed-width options round-trip" "$msg"
fi

expect_failure() {
  local label=$1 proto=$2 needle=$3
  out="$work/stale.lua"
  echo "-- stale schema" > "$out"
  cp "$out" "$work/before"
  if "$python" "$gen" "$out" "$root/test/$proto" > /dev/null 2> "$work/err"; then
    bad "$label" "expected a non-zero exit, got 0"
  elif ! cmp -s "$out" "$work/before"; then
    bad "$label" "the output file changed"
  elif ! grep -qF -- "$needle" "$work/err"; then
    bad "$label" "stderr lacks '$needle': $(tr '\n' ' ' < "$work/err")"
  else
    ok "$label"
  fi
}

expect_failure "unsupported wire type fails and leaves the output" group_option.proto \
  "Unsupported wire type: 3"
expect_failure "non-finite option fails and leaves the output" nonfinite_option.proto \
  "has no Lua literal"

[ "$failed" -eq 0 ] || { echo "$failed control(s) failed" >&2; exit 1; }
