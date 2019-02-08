# Tests workflow
#
# All tests go to the "test directory" `rsc/tests`. The test directory must contain only
# "context directories" which themselves contain at least a directory `contracts`. It stores
# contract files, which are the contracts on which the tests of the context directory (explained
# below) will use.
#
# A test directory can also contain two optional directories `okay` and `error`. Both store
# testcases in the techelson sense. A testcase in `okay` (if it exists) succeeds if its exit
# code is `0`. Testcases in `error` (if it exists) succed when their exit code is not `0`.
#
# Additionally, "test generation directory" `rsc/testgen` contains contract files in a `okay` or
# `error` subdirectory. This script will run test generation on each contract file separately.

root_dir="rsc/tests"
binary="./bin/techelson"
zero_if_no_error="0"

testgen_root="rsc/testgen"
testgen_count="15"

# Prints an error message and exits with exit code `2`.
#
# Arguments
# - error message
function bail {
    err_msg="$1"
    echo "Error: $err_msg"
    exit 2
}

colored_okay="\033[32mokay\033[0m"
colored_error="\033[31merror\033[0m"

# Prints the info for a test about to run.
#
# Arguments
# - "okay" or "error" depending on the outcome expected
# - file name
# - command to run
function fmt_run {
    fmt_running_okay="$1"
    fmt_running_name="$2"
    fmt_running_cmd="$3"

    if [[ "$fmt_running_cmd" == "" ]] ; then
        echo "empty command"
        exit 2
    fi

    if [[ "$fmt_running_okay" == "okay" ]] ; then
        pref=" $colored_okay"
        expected="0"
    else
        pref="$colored_error"
        expected="not 0"
    fi

    printf "    $pref/%-37s..." "$fmt_running_name"

    $fmt_running_cmd > /dev/null
    fmt_running_exit_code="$?"

    if [[ \
           "$fmt_running_exit_code" == "0" && "$fmt_running_okay" == "okay"  \
        || "$fmt_running_exit_code" != "0" && "$fmt_running_okay" == "error" \
    ]] ; then
        printf "$colored_okay  \\(^_^)/\n"
        # echo "    exit code: $fmt_running_exit_code, expected $expected"
    else
        zero_if_no_error="2"
        printf "$colored_error /(T_T)\\"
        echo
        echo "        exit code: $fmt_running_exit_code (expected $expected)"
        echo "        command: \"$fmt_running_cmd\""
        exit 2
    fi
}

# Runs a test and prints a success or failure message.
#
# Arguments
# - contract options: e.g. `--contract <contract_1> --contract <contract_2>`
# - path the to testcase
# - "okay" if the test is expected to succeed, "error" otherwise
function run_test {
    contract_options="$1"
    testcase="$2"
    okay="$3"

    short_testcase=`echo "$testcase" | sed -e 's:.*/\(okay/\)::' -e 's:.*/\(error/\)::'`
    command="$binary $contract_options -- $testcase"

    fmt_run "$okay" "$short_testcase" "$command"
}

# Runs all the tests in a test directory.
#
# Arguments
# - the path to the test directory
function run_tests {
    test_dir="$1"

    # Build contract options.
    contract_options=""
    sep=""
    if [ -d "$test_dir/contracts" ] ; then
        for contract_file in `find "$test_dir/contracts" -type f`; do
            contract_options="$contract_options$sep--contract $contract_file"
            sep=" "
        done
    else
        bail "found no \"contracts\" directory in test directory \"$test_dir\""
    fi

    echo "running tests for \"$test_dir\""

    # Run on "okay" tests.
    if [ -d "$test_dir/okay" ] ; then
        for okay_test in `find "$test_dir/okay" -type f`; do
            run_test "$contract_options" "$okay_test" "okay"
        done
    fi
    # Run on "error" tests.
    if [ -d "$test_dir/error" ] ; then
        for error_test in `find "$test_dir/error" -type f`; do
            run_test "$contract_options" "$error_test" "error"
        done
    fi
}

# Runs test generation on a subdirectory.
#
# Arguments:
# - the subdirectory, "okay" or "error"
function run_testgen_on {
    testgen_subdir="$1"
    current_testgen_dir="$testgen_root/$testgen_subdir"

    if [[ \
        -d "$current_testgen_dir" && \
        `ls "$current_testgen_dir" | wc -l | tr -d ' '` != "0" \
    ]] ; then
        echo "running test generation for contracts in $current_testgen_dir"
        for contract_file in `find "$testgen_root/$testgen_subdir" -type f`; do
            contract_options="--contract $contract_file"
            short_contract=`echo "$contract_file" | sed -e 's:.*/\(okay/\)::' -e 's:.*/\(error/\)::'`
            command="./bin/techelson --contract $contract_file testgen --count $testgen_count"
            fmt_run "$testgen_subdir" "$short_contract" "$command"
        done
    fi
}

# Runs test generation. No argument.
function run_testgen {
    run_testgen_on "okay"
    run_testgen_on "error"
}

# Run on all test directories.
for directory in `find "$root_dir" -type d -maxdepth 1` ; do
    # Skip top test directory.
    if [ "$root_dir" == "$directory" ] ; then
        continue
    fi

    run_tests "$directory"
done

run_testgen

exit $zero_if_no_error