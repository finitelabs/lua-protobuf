#!/usr/bin/env bash
#
# Positive controls for tools/proto-provenance. Each perturbs one thing and restores before the
# next, so every failure is attributable.

set -u

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tool="$root/tools/proto-provenance"
[ -x "$tool" ] || { echo "not executable: $tool" >&2; exit 1; }

work=$(mktemp -d) || exit 1
trap 'rm -rf "$work"' EXIT

schema="$work/schema.lua"
runtime="$work/protobuf.lua"
boundary='-- provenance-boundary: every line below is covered by body-sha256'

passed=0
failed=0
status=0

ok() {
  passed=$((passed + 1))
  printf '  ok    %s\n' "$1"
}

bad() {
  failed=$((failed + 1))
  printf '  FAIL  %s\n' "$1"
  [ -n "${2:-}" ] && printf '        %s\n' "$2"
  return 0
}

run() {
  "$tool" "$@" > "$work/out" 2> "$work/err"
  status=$?
}

expect_green() {
  local label=$1
  shift
  run "$@"
  if [ "$status" -eq 0 ]; then
    ok "$label"
  else
    bad "$label" "expected exit 0, got $status: $(tr '\n' ' ' < "$work/err")"
  fi
}

expect_fail() {
  local label=$1 needle=$2
  shift 2
  run "$@"
  if [ "$status" -eq 0 ]; then
    bad "$label" "expected a non-zero exit, got 0"
  elif grep -qF -- "$needle" "$work/err"; then
    ok "$label"
  else
    bad "$label" "message lacks '$needle': $(tr '\n' ' ' < "$work/err")"
  fi
}

write_runtime() {
  printf 'local VERSION = "%s"\n' "$1" > "$runtime"
}

write_schema() {
  cat > "$schema" << 'EOF'
-- Generated Lua schema from protobuf descriptor set
-- Do not edit manually

--- @class ProtoSchema
local ProtoSchema = {}
ProtoSchema.Message = {}
ProtoSchema.Message["Example"] = { name = "Example", fields = {} }
return ProtoSchema
EOF
}

# Aborting rather than reporting a failure keeps a broken fixture from certifying the tool off an
# argument error.
fresh() {
  write_runtime v0.6.9
  write_schema
  "$tool" stamp "$schema" "$runtime" esphome=2026.8.2 > /dev/null || {
    echo "fixture stamp failed; controls cannot run" >&2
    exit 1
  }
}

apply() {
  sed "$1" "$schema" > "$work/s.tmp" && mv "$work/s.tmp" "$schema"
}

drop_line() {
  grep -v -F -x -- "$1" "$schema" > "$work/s.tmp" && mv "$work/s.tmp" "$schema"
}

body_of() {
  awk -v b="$boundary" 'seen { print } $0 == b { seen = 1 }' "$1"
}

boundary_line() {
  grep -n -F -x -- "$boundary" "$schema" | head -1 | cut -d: -f1
}

echo "proto-provenance controls"

# --- the recorded pair, as a consumer uses it ---------------------------------

fresh
expect_green "clean tree" check "$schema" "$runtime" esphome=2026.8.2

fresh
apply 's/^ProtoSchema.Message = {}$/ProtoSchema.Message = { }/'
expect_fail "body edited mid-file" "body does not match its header" \
  check "$schema" "$runtime" esphome=2026.8.2
expect_fail "... and the message names reformatting as a cause" "reformatted" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
apply 's/^-- generator: v0.6.9$/-- generator: v0.6.8/'
expect_fail "header generator moved back a release" "comes from lua-protobuf v0.6.8" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
write_runtime v0.7.0
expect_fail "runtime moved forward, the real-world direction" "the vendored decoder is v0.7.0" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
expect_fail "consumer field bumped without restamping" "esphome is now 2026.9.0" \
  check "$schema" "$runtime" esphome=2026.9.0

# --- the loud-failure paths ---------------------------------------------------

fresh
drop_line "$boundary"
expect_fail "boundary line removed" "has 0 provenance boundary lines" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
apply '/^-- body-sha256: /d'
expect_fail "body-sha256 line removed" "expected exactly one 'body-sha256' line" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
apply "/^${boundary}\$/p"
expect_fail "duplicate boundary line" "has 2 provenance boundary lines" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
write_runtime dev
expect_fail "runtime built from an untagged tree" "VERSION=dev" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
write_runtime 0.6.9
expect_fail "runtime VERSION is not a release tag" "not a release tag" \
  check "$schema" "$runtime" esphome=2026.8.2

# --- the hash boundary sits exactly where the header claims -------------------

fresh
apply '3a\
--'
expect_green "header-only edit above the boundary" check "$schema" "$runtime" esphome=2026.8.2

fresh
apply "$(($(boundary_line) + 1))s/\$/ -- x/"
expect_fail "first line below the boundary edited" "body does not match its header" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
apply '$s/^return ProtoSchema$/return ProtoSchema --/'
expect_fail "last line of the file edited" "body does not match its header" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
expect_green "everything restored" check "$schema" "$runtime" esphome=2026.8.2

# --- caller-supplied fields are opaque, but not optional ----------------------

fresh
expect_fail "field the header records left unsupplied" "different set of provenance fields" \
  check "$schema" "$runtime"

fresh
expect_fail "field supplied that the header does not record" "different set of provenance fields" \
  check "$schema" "$runtime" esphome=2026.8.2 protoc=29.3

fresh
apply "/^${boundary}\$/i\\
-- protoc: 29.3"
expect_fail "field line injected into the header" "different set of provenance fields" \
  check "$schema" "$runtime" esphome=2026.8.2

fresh
expect_fail "reserved key supplied by the caller" "is reserved" \
  check "$schema" "$runtime" esphome=2026.8.2 generator=v0.6.9

fresh
expect_fail "key supplied twice" "was supplied 2 times" \
  check "$schema" "$runtime" esphome=2026.8.2 esphome=2026.9.0

fresh
expect_fail "field with an empty value" "has an empty value" \
  check "$schema" "$runtime" esphome=

fresh
expect_fail "field that is not key=value" "not in key=value form" \
  check "$schema" "$runtime" esphome

write_runtime v0.6.9
write_schema
expect_green "stamp with no consumer fields" stamp "$schema" "$runtime"
expect_green "... and check with none" check "$schema" "$runtime"

write_runtime v0.6.9
write_schema
expect_green "stamp with two fields" stamp "$schema" "$runtime" esphome=2026.8.2 protoc=29.3
expect_green "... and check supplying them in the other order" \
  check "$schema" "$runtime" protoc=29.3 esphome=2026.8.2

# --- re-stamping records, it does not rewrite ---------------------------------

fresh
body_of "$schema" > "$work/body-before"
if "$tool" stamp "$schema" "$runtime" esphome=2026.8.2 > /dev/null 2>&1; then
  body_of "$schema" > "$work/body-after"
  if diff -q "$work/body-before" "$work/body-after" > /dev/null; then
    ok "re-stamping leaves the body byte-identical"
  else
    bad "re-stamping leaves the body byte-identical" "the body changed"
  fi
else
  bad "re-stamping leaves the body byte-identical" "re-stamp exited non-zero"
fi

total=$((passed + failed))
echo
if [ "$failed" -eq 0 ]; then
  echo "proto-provenance: $passed/$total controls passed"
  exit 0
fi
echo "proto-provenance: $failed/$total controls FAILED"
exit 1
