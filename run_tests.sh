#!/bin/bash

# lua-protobuf Test Runner
#
# Usage: ./run_tests.sh [module_names...]
#
# Examples:
#   ./run_tests.sh                    # Run all modules
#   ./run_tests.sh protobuf           # Run only protobuf
#
# test/<name>_test.lua is module <name>, underscores as dashes. Each runs with native
# math.frexp/ldexp and again with them cleared, unless `-- @test-modes` restricts it.
# `-- @test-name` sets its label.

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

module_keys=()
declare -a module_files=()

for test_file in "$script_dir"/test/*_test.lua; do
    [ -e "$test_file" ] || continue
    base=${test_file##*/}
    base=${base%_test.lua}
    key=${base//_/-}
    module_keys+=("$key")
    module_files+=("$test_file")
done

if [ ${#module_keys[@]} -eq 0 ]; then
    echo -e "${red}Error: no test/*_test.lua modules found${nc}"
    exit 1
fi

module_directive() {
    sed -n "s/^-- @$2[[:space:]]\{1,\}//p" "$1" | head -1
}

module_file_for() {
    local i
    for i in "${!module_keys[@]}"; do
        if [ "${module_keys[$i]}" = "$1" ]; then
            echo "${module_files[$i]}"
            return 0
        fi
    done
    return 1
}

default_modules=("${module_keys[@]}")
all_modules=("${module_keys[@]}")
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

# No matrix interpreter lacks the natives, so without this nothing runs the codecs through the fallbacks.
math_modes=("native" "fallback")

math_preamble() {
    if [ "$1" = "fallback" ]; then
        echo "math.frexp = nil; math.ldexp = nil;"
    fi
}

run_module() {
    local module_key="$1"
    local test_file
    test_file=$(module_file_for "$module_key")

    if ! should_run_module "$module_key"; then
        return
    fi

    local module_name
    module_name=$(module_directive "$test_file" "test-name")
    [ -n "$module_name" ] || module_name="$module_key"

    local wanted_modes
    wanted_modes=$(module_directive "$test_file" "test-modes")
    [ -n "$wanted_modes" ] || wanted_modes="${math_modes[*]}"

    local lua_command="dofile('$test_file')"

    for math_mode in "${math_modes[@]}"; do
        if [[ " $wanted_modes " != *" $math_mode "* ]]; then
            continue
        fi
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

for module_key in "${module_keys[@]}"; do
    run_module "$module_key"
done

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