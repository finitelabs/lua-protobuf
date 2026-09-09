#!/bin/bash

# lua-protobuf Test Runner
#
# Usage: ./run_tests.sh [module_names...]
#
# Examples:
#   ./run_tests.sh                    # Run all modules
#   ./run_tests.sh protobuf           # Run only protobuf
#
# Every module runs twice, once with the interpreter's native math.frexp and
# math.ldexp and once with them cleared so the module's own fallbacks are bound.
#
# Available modules: protobuf, float-vectors

set -e  # Exit on any error

echo "============================================="
echo "Lua Protobuf Library - Test Suite Runner"
echo "============================================="
echo

# Colors for output
green='\033[0;32m'
red='\033[0;31m'
blue='\033[0;34m'
nc='\033[0m' # No Color

# Track overall results
passed_modules=()
failed_modules=()

# Lua binary to use for running tests
lua_binary="${LUA_BINARY:-lua}"

# Check if the lua binary is available
if ! command -v "$lua_binary" &> /dev/null; then
    echo -e "${red}Error: $lua_binary command not found.${nc}"
    exit 1
fi
echo "$($lua_binary -v)"
echo

# Get script directory
script_dir=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)

# Add repository root to Lua's package path
# This allows require() to find modules in the src/vendor directories
lua_path="$script_dir/?.lua;$script_dir/?/init.lua;$script_dir/src/?.lua;$script_dir/src/?/init.lua;$script_dir/vendor/?.lua;$LUA_PATH"

# Parse command line arguments to determine which modules to run
default_modules=("protobuf" "float-vectors")
all_modules=("protobuf" "float-vectors")
modules_to_run=("$@")

# Validate modules if specified
if [ ${#modules_to_run[@]} -gt 0 ] && [ "${modules_to_run[0]}" != "all" ]; then
    for module in "${modules_to_run[@]}"; do
        valid=0
        for valid_module in "${all_modules[@]}"; do
            if [ "$module" = "$valid_module" ]; then
                valid=1
                break
            fi
        done
        if [ $valid -eq 0 ]; then
            echo -e "${red}Error: Unknown module '$module'${nc}"
            echo "Available modules: ${all_modules[*]}"
            exit 1
        fi
    done
fi

if [ ${#modules_to_run[@]} -eq 0 ]; then
    modules_to_run=("${default_modules[@]}")
    echo "Running default modules: ${modules_to_run[*]}"
elif [ "${modules_to_run[0]}" = "all" ]; then
    modules_to_run=("${all_modules[@]}")
    echo "Running all modules: ${modules_to_run[*]}"
else
    echo "Running specified modules: ${modules_to_run[*]}"
fi
echo

# Function to check if a module should be run
should_run_module() {
    local module_key="$1"
    for module in "${modules_to_run[@]}"; do
        if [ "$module" = "$module_key" ]; then
            return 0
        fi
    done
    return 1
}

# math.frexp and math.ldexp are absent on a 5.3 or 5.4 built without
# LUA_COMPAT_5_3, so the module falls back to its own implementations. Every
# interpreter in the matrix supplies them natively, which left the fallback
# unreached by any test. Clearing the globals before require() binds the
# fallbacks instead, so each module runs once down each path.
math_modes=("native" "fallback")

math_preamble() {
    if [ "$1" = "fallback" ]; then
        echo "math.frexp = nil; math.ldexp = nil;"
    fi
}

# Function to run a test and capture result
run_test() {
    local module_name="$1"
    local module_key="$2"
    local lua_command="$3"

    if ! should_run_module "$module_key"; then
        return
    fi

    for math_mode in "${math_modes[@]}"; do
        local labelled="$module_name (math $math_mode)"

        echo "---------------------------------------------"
        echo -e "${blue}Testing $labelled...${nc}"
        echo "---------------------------------------------"

        if LUA_PATH="$lua_path" "$lua_binary" -e "$(math_preamble "$math_mode") $lua_command" 2>&1; then
            echo -e "${green}✅ $labelled: ALL TESTS PASSED${nc}"
            passed_modules+=("$labelled")
        else
            echo -e "${red}❌ $labelled: TESTS FAILED${nc}"
            failed_modules+=("$labelled")
        fi

        echo
    done
}

run_selftest() {
  local module_name="$1"
  local module_key="$2"
  local lua_module="$3"
  run_test "$module_name" "$module_key" "
    local result = require('$lua_module').selftest()
    if not result then
        os.exit(1)
    end
  "
}

run_selftest "Protobuf operations" "protobuf" "protobuf"

export PB_TEST_DIR="$script_dir/test"
run_test "Float codec vectors" "float-vectors" "
    dofile('$script_dir/test/float_vectors_test.lua')
"

passed_count=${#passed_modules[@]}
failed_count=${#failed_modules[@]}
total_count=$((passed_count + failed_count))

# If only one module is run, no need to summarize
if [ $total_count -eq 1 ]; then
    if [ $failed_count -gt 0 ]; then
        exit 1
    fi
    exit 0
fi

# Summary
echo "============================================="
echo "📊 TEST SUMMARY"
echo "============================================="

if [ $passed_count -eq $total_count ]; then
    echo -e "${green}🎉 ALL MODULES PASSED: $passed_count/$total_count${nc}"
    echo
    echo "Passed modules:"
    for module in "${passed_modules[@]}"; do
        echo "• $module: ✅ PASS"
    done
    exit 0
else
    echo -e "${red}💥 SOME MODULES FAILED: $passed_count/$total_count passed${nc}"
    echo
    echo "Failed modules:"
    for module in "${failed_modules[@]}"; do
        echo "• $module: ❌ FAIL"
    done
    exit 1
fi