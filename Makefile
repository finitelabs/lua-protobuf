# Luarocks path for amalg and other tools
LUAROCKS_PATH := $(shell luarocks path --lr-path 2>/dev/null)

# Lua path for local modules (src, vendor)
LUA_PATH_LOCAL := ./?.lua;./?/init.lua;./src/?.lua;./src/?/init.lua;./vendor/?.lua;$(LUAROCKS_PATH)

# Interpreter for standalone tool scripts; run_tests.sh honours the same variable
LUA_BINARY ?= lua

# Default target
.PHONY: all
all: format lint test build

# Run tests
.PHONY: test
test:
	./run_tests.sh

# Run test matrix
.PHONY: test-matrix
test-matrix:
	./run_tests_matrix.sh

# Run specific test suite for test matrix
.PHONY: test-matrix-%
test-matrix-%:
	./run_tests_matrix.sh $*

# Run specific test suite
.PHONY: test-%
test-%:
	./run_tests.sh $*

build/amalg.cache: src/protobuf/init.lua
	@echo "Generating amalgamation cache..."
	@mkdir -p build
	@if command -v amalg.lua >/dev/null 2>&1; then \
		LUA_PATH="$(LUA_PATH_LOCAL)" lua -lamalg src/protobuf/init.lua && mv amalg.cache build || exit 1; \
		echo "Generated amalg.cache"; \
	else \
		echo "Error: amalg not found."; \
		echo "Please install amalg: luarocks install amalg"; \
		echo "Or run: make install-deps"; \
		exit 1; \
	fi

# Build single-file distributions
.PHONY: build
build: build/amalg.cache
	@echo "Building single-file distribution..."
	@if command -v amalg.lua >/dev/null 2>&1; then \
		LUA_PATH="$(LUA_PATH_LOCAL)" amalg.lua -o build/protobuf.lua -C ./build/amalg.cache -i "bitn" || exit 1;\
		echo "Built build/protobuf.lua (core; bitn excluded, expected on the path)"; \
		LUA_PATH="$(LUA_PATH_LOCAL)" amalg.lua -o build/protobuf-portable.lua -C ./build/amalg.cache || exit 1;\
		echo "Built build/protobuf-portable.lua (portable; all dependencies bundled)"; \
		VERSION=$$(git describe --exact-match --tags 2>/dev/null || echo "dev"); \
		if [ "$$VERSION" != "dev" ]; then \
			echo "Injecting version $$VERSION..."; \
			sed -i.bak 's/VERSION = "dev"/VERSION = "'$$VERSION'"/' build/protobuf.lua && rm build/protobuf.lua.bak; \
			sed -i.bak 's/VERSION = "dev"/VERSION = "'$$VERSION'"/' build/protobuf-portable.lua && rm build/protobuf-portable.lua.bak; \
		fi; \
		echo "Testing version function..."; \
		CORE_VERSION=$$(LUA_PATH="$(LUA_PATH_LOCAL)" lua -e 'local p = require("build.protobuf"); print(p.version())' 2>/dev/null || echo "test failed"); \
		PORTABLE_VERSION=$$(LUA_PATH="$(LUA_PATH_LOCAL)" lua -e 'local p = require("build.protobuf-portable"); print(p.version())' 2>/dev/null || echo "test failed"); \
		if [ "$$CORE_VERSION" = "$$VERSION" ] && [ "$$PORTABLE_VERSION" = "$$VERSION" ]; then \
			echo "Version correctly set to: $$VERSION (core + portable)"; \
		else \
			echo "Version test failed. Expected: $$VERSION, core: $$CORE_VERSION, portable: $$PORTABLE_VERSION"; \
		fi; \
	else \
		echo "Error: amalg not found."; \
		echo "Please install amalg: luarocks install amalg"; \
		echo "Or run: make install-deps"; \
		exit 1; \
	fi

# Install all development dependencies
.PHONY: install-deps
install-deps:
	@echo "Installing development dependencies..."
	@echo ""
	@echo "=== Installing system tools ==="
	@if command -v brew >/dev/null 2>&1; then \
		echo "Using Homebrew to install tools..."; \
		brew install lua-language-server stylua protobuf || true; \
	else \
		echo "Please install the following manually:"; \
		echo "  - lua-language-server: https://github.com/LuaLS/lua-language-server/releases"; \
		echo "  - stylua: https://github.com/JohnnyMorganz/StyLua/releases"; \
		echo "  - luarocks: https://github.com/luarocks/luarocks/wiki/Download"; \
		echo "  - protobuf: https://github.com/protocolbuffers/protobuf/releases"; \
	fi
	@echo ""
	@echo "=== Installing Lua tools ==="
	@if command -v luarocks >/dev/null 2>&1; then \
		echo "Using LuaRocks to install tools..."; \
		luarocks install luacheck || exit 1; \
		luarocks install amalg || exit 1; \
	else \
		echo "luarocks not found. Please install it first."; \
		echo "  macOS: brew install luarocks"; \
		echo "  Linux: apt-get install luarocks"; \
		exit 1; \
	fi
	@echo ""
	@echo "=== Setting up Python environment for schema generator ==="
	@$(MAKE) setup-schema-generator

# Setup Python virtual environment for schema generator
.PHONY: setup-schema-generator
setup-schema-generator:
	@echo "Setting up Python virtual environment..."
	@python3 -m venv .venv
	@echo "Installing Python dependencies..."
	@.venv/bin/pip install -r tools/requirements.txt
	@echo "Schema generator ready. Use 'make gen-schema PROTO=<file> OUTPUT=<file>' to generate schemas."

# Generate Lua schema from proto files
# Usage: make gen-schema PROTO=input.proto OUTPUT=output.lua
# For multiple protos: make gen-schema PROTO="file1.proto file2.proto" OUTPUT=output.lua
.PHONY: gen-schema
gen-schema:
	@if [ -z "$(PROTO)" ] || [ -z "$(OUTPUT)" ]; then \
		echo "Usage: make gen-schema PROTO=<proto-file(s)> OUTPUT=<output.lua>"; \
		echo "Example: make gen-schema PROTO=api.proto OUTPUT=src/schema.lua"; \
		exit 1; \
	fi
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@.venv/bin/python3 tools/gen_lua_proto_schema $(OUTPUT) $(PROTO)

# Generate the base types schema from empty.proto
.PHONY: gen-types
gen-types:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@echo "Generating src/protobuf/types.lua from empty.proto..."
	@.venv/bin/python3 tools/gen_lua_proto_schema src/protobuf/types.lua empty.proto
	@echo "Generated src/protobuf/types.lua"

# Check that types.lua matches what would be generated (for CI)
.PHONY: check-types
check-types:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@echo "Checking src/protobuf/types.lua is up to date..."
	@mkdir -p build
	@.venv/bin/python3 tools/gen_lua_proto_schema build/types.lua.tmp empty.proto
	@if diff -q src/protobuf/types.lua build/types.lua.tmp >/dev/null 2>&1; then \
		echo "src/protobuf/types.lua is up to date"; \
		rm -f build/types.lua.tmp; \
	else \
		echo "ERROR: src/protobuf/types.lua is out of date!"; \
		echo "Run 'make gen-types' to regenerate it."; \
		echo ""; \
		echo "Diff:"; \
		diff src/protobuf/types.lua build/types.lua.tmp || true; \
		rm -f build/types.lua.tmp; \
		exit 1; \
	fi

# Generate each fixture schema and assert its subschema references resolve, then run the
# generator's option round-trip and failure controls
.PHONY: check-schema
check-schema:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@echo "Checking generated schemas resolve their subschema references..."
	@mkdir -p build
	@.venv/bin/python3 tools/gen_lua_proto_schema build/nested.schema.lua test/nested.proto
	@$(LUA_BINARY) tools/check_schema_refs.lua build/nested.schema.lua
	@.venv/bin/python3 tools/gen_lua_proto_schema build/maps.schema.lua test/maps.proto
	@$(LUA_BINARY) tools/check_schema_refs.lua build/maps.schema.lua
	@.venv/bin/python3 tools/gen_lua_proto_schema build/test_messages_proto3.schema.lua test/test_messages_proto3.proto
	@$(LUA_BINARY) tools/check_schema_refs.lua build/test_messages_proto3.schema.lua
	@.venv/bin/python3 tools/gen_lua_proto_schema build/empty.schema.lua empty.proto
	@$(LUA_BINARY) tools/check_schema_refs.lua build/empty.schema.lua
	@LUA_BINARY=$(LUA_BINARY) ./test/gen_schema_controls.sh

# Exercise tools/proto-provenance against its positive controls. Needs no venv,
# no protoc and no network, the same footing a consumer's check runs on.
.PHONY: check-provenance
check-provenance:
	@./test/proto_provenance_controls.sh

# Regenerate the checked-in wire-format schema and golden vectors
.PHONY: gen-wire-vectors
gen-wire-vectors:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@.venv/bin/python3 tools/gen_lua_proto_schema \
		test/generated/test_messages_proto3_schema.lua test/test_messages_proto3.proto
	@.venv/bin/python3 tools/gen_wire_vectors \
		test/generated/wire_vectors.lua test/test_messages_proto3.proto

# Check wire vectors for drift and against the reference implementation. Needs the venv, so not in `check`.
.PHONY: check-wire-vectors
check-wire-vectors:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@mkdir -p build
	@.venv/bin/python3 tools/gen_lua_proto_schema \
		build/wire.schema.lua.tmp test/test_messages_proto3.proto
	@.venv/bin/python3 tools/gen_wire_vectors \
		build/wire_vectors.lua.tmp test/test_messages_proto3.proto
	@if ! diff -q test/generated/test_messages_proto3_schema.lua build/wire.schema.lua.tmp >/dev/null 2>&1; then \
		echo "ERROR: test/generated/test_messages_proto3_schema.lua is out of date!"; \
		echo "Run 'make gen-wire-vectors' to regenerate it."; \
		diff test/generated/test_messages_proto3_schema.lua build/wire.schema.lua.tmp || true; \
		exit 1; \
	fi
	@if ! diff -q test/generated/wire_vectors.lua build/wire_vectors.lua.tmp >/dev/null 2>&1; then \
		echo "ERROR: test/generated/wire_vectors.lua is out of date!"; \
		echo "Run 'make gen-wire-vectors' to regenerate it."; \
		diff test/generated/wire_vectors.lua build/wire_vectors.lua.tmp || true; \
		exit 1; \
	fi
	@echo "Checked-in wire vectors match the generator."
	@LUA_BINARY=$(LUA_BINARY) .venv/bin/python3 tools/check_wire_vectors

# Regenerate the checked-in IEEE 754 codec vectors
.PHONY: gen-float-vectors
gen-float-vectors:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@.venv/bin/python3 tools/gen_float_vectors test/generated/float_vectors.lua

# Check float vectors for drift and against the oracle. Needs the venv, so not in `check`.
.PHONY: check-float-vectors
check-float-vectors:
	@if [ ! -f .venv/bin/python3 ]; then \
		echo "Python virtual environment not found. Run 'make setup-schema-generator' first."; \
		exit 1; \
	fi
	@mkdir -p build
	@.venv/bin/python3 tools/gen_float_vectors build/float_vectors.lua.tmp
	@if ! diff -q test/generated/float_vectors.lua build/float_vectors.lua.tmp >/dev/null 2>&1; then \
		echo "ERROR: test/generated/float_vectors.lua is out of date!"; \
		echo "Run 'make gen-float-vectors' to regenerate it."; \
		diff test/generated/float_vectors.lua build/float_vectors.lua.tmp || true; \
		exit 1; \
	fi
	@echo "Checked-in float vectors match the generator."
	@LUA_BINARY=$(LUA_BINARY) .venv/bin/python3 tools/check_float_vectors

# Format Lua code with stylua
.PHONY: format
format:
	@if command -v stylua >/dev/null 2>&1; then \
		echo "Running stylua..."; \
		stylua --indent-type Spaces --column-width 120 --line-endings Unix \
			--indent-width 2 --quote-style AutoPreferDouble \
			src/ 2>/dev/null; \
	else \
		echo "stylua not found. Install with: make install-deps"; \
		exit 1; \
	fi

# Check Lua formatting
.PHONY: format-check
format-check:
	@if command -v stylua >/dev/null 2>&1; then \
		echo "Running stylua check..."; \
		stylua --check --indent-type Spaces --column-width 120 --line-endings Unix \
			--indent-width 2 --quote-style AutoPreferDouble \
			src/; \
	else \
		echo "stylua not found. Install with: make install-deps"; \
		exit 1; \
	fi

# Lint the code with luacheck
.PHONY: lint
lint:
	@if command -v luacheck >/dev/null 2>&1; then \
		echo "Running luacheck..."; \
		luacheck src/; \
	else \
		echo "luacheck not found. Install with: make install-deps"; \
		exit 1; \
	fi

# Type-check annotations with the Lua language server. Catches what luacheck does
# not: undefined or duplicate `@alias`, returns that disagree with `@return`.
#
# `vendor/` is both a `library` and an `ignoreDir`: with only the first its code is
# diagnosed here, with only the second its definitions are lost. `runtime.version`
# is pinned to LuaJIT because that is what Control4 runs, not to change a count.
.PHONY: typecheck
typecheck:
	@if command -v lua-language-server >/dev/null 2>&1; then \
		echo "Running lua-language-server $$(lua-language-server --version)..."; \
		lua-language-server --check "$(CURDIR)" --checklevel=Warning \
			--configpath="$(CURDIR)/.luarc-typecheck.json" --logpath="$(CURDIR)/build/luals"; \
	else \
		echo "lua-language-server not found. Install with: make install-deps"; \
		exit 1; \
	fi

.PHONY: check
check: format-check lint check-provenance check-types check-schema typecheck
	@echo "Code quality checks complete."

# Clean generated files
.PHONY: clean
clean:
	rm -rf build/ .venv/

# Help
.PHONY: help
help:
	@echo "Lua Protobuf Library - Makefile targets"
	@echo ""
	@echo "Testing:"
	@echo "  make test               - Run all tests"
	@echo "  make test-<name>        - Run specific test (e.g., make test-protobuf)"
	@echo "  make test-matrix        - Run tests across all Lua versions"
	@echo "  make test-matrix-<name> - Run specific test across all Lua versions"
	@echo "  make test-wire-vectors  - Run only the wire-format vectors"
	@echo "  make test-float-vectors - Run only the IEEE 754 codec vectors"
	@echo ""
	@echo "Building:"
	@echo "  make build              - Build single-file distributions"
	@echo ""
	@echo "Schema Generation:"
	@echo "  make setup-schema-generator                - Setup Python venv for schema generator"
	@echo "  make gen-schema PROTO=<file> OUTPUT=<file> - Generate Lua schema from proto file(s)"
	@echo "  make gen-types                             - Regenerate src/protobuf/types.lua"
	@echo "  make check-types                           - Verify types.lua matches empty.proto"
	@echo "  make check-schema                          - Verify generated schemas resolve subschemas"
	@echo "  make check-provenance                      - Run the proto-provenance positive controls"
	@echo ""
	@echo "Wire Vectors (need Python; deliberately not part of check):"
	@echo "  make gen-wire-vectors   - Regenerate the checked-in schema and goldens"
	@echo "  make check-wire-vectors - Check goldens for drift and verify both directions"
	@echo "  make gen-float-vectors  - Regenerate the checked-in IEEE 754 vectors"
	@echo "  make check-float-vectors - Check those vectors for drift and against the oracle"
	@echo ""
	@echo "Code Quality:"
	@echo "  make check              - Run format-check, lint, check-provenance, check-types, check-schema, and typecheck"
	@echo "  make format             - Format code with stylua"
	@echo "  make format-check       - Check code formatting"
	@echo "  make lint               - Lint code with luacheck"
	@echo "  make typecheck          - Check annotations with lua-language-server"
	@echo ""
	@echo "Setup:"
	@echo "  make install-deps       - Install development dependencies"
	@echo "  make clean              - Remove generated files"
	@echo ""
	@echo "  make help               - Show this help"
