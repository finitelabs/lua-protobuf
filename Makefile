# Luarocks path for amalg and other tools
LUAROCKS_PATH := $(shell luarocks path --lr-path 2>/dev/null)

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
		LUA_PATH="./?.lua;./?/init.lua;./src/?.lua;./src/?/init.lua;$(LUAROCKS_PATH)" lua -lamalg src/protobuf/init.lua && mv amalg.cache build || exit 1; \
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
		LUA_PATH="./?.lua;./?/init.lua;./src/?.lua;./src/?/init.lua;$(LUAROCKS_PATH)" amalg.lua -o build/protobuf.lua -C ./build/amalg.cache || exit 1;\
		echo "Built build/protobuf.lua"; \
		VERSION=$$(git describe --exact-match --tags 2>/dev/null || echo "dev"); \
		if [ "$$VERSION" != "dev" ]; then \
			echo "Injecting version $$VERSION..."; \
			sed -i.bak 's/VERSION = "dev"/VERSION = "'$$VERSION'"/' build/protobuf.lua && rm build/protobuf.lua.bak; \
		fi; \
		echo "Testing version function..."; \
		LUA_VERSION=$$(lua -e 'local p = require("build.protobuf"); print(p.version())' 2>/dev/null || echo "test failed"); \
		if [ "$$LUA_VERSION" = "$$VERSION" ]; then \
			echo "Version correctly set to: $$VERSION"; \
		else \
			echo "Version test failed. Expected: $$VERSION, Got: $$LUA_VERSION"; \
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

.PHONY: check
check: format-check lint
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
	@echo "  make test              - Run all tests"
	@echo "  make test-<name>       - Run specific test (e.g., make test-protobuf)"
	@echo "  make test-matrix       - Run test matrix across Lua versions"
	@echo ""
	@echo "Building:"
	@echo "  make build             - Build single-file distribution"
	@echo ""
	@echo "Schema Generation:"
	@echo "  make setup-schema-generator                     - Setup Python venv for schema generator"
	@echo "  make gen-schema PROTO=<file> OUTPUT=<file>      - Generate Lua schema from proto file(s)"
	@echo ""
	@echo "Code Quality:"
	@echo "  make format            - Format all code (Lua)"
	@echo "  make format-check      - Check code formatting"
	@echo "  make lint              - Lint code with luacheck"
	@echo ""
	@echo "Setup:"
	@echo "  make install-deps      - Install all development dependencies"
	@echo "  make clean             - Remove generated files"
	@echo ""
	@echo "  make help              - Show this help"
