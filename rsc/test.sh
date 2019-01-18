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

root_dir="rsc/tests"
binary="./bin/techelson"
zero_if_no_error="0"

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

    if [[ "$okay" == "okay" ]] ; then
        pref=" $colored_okay"
        expected="0"
    else
        pref="$colored_error"
        expected="not 0"
    fi

    short_testcase=`echo "$testcase" | sed -e 's:.*/\(okay/\)::' -e 's:.*/\(error/\)::'`
    printf "    $pref/%-37s" "$short_testcase..."

    command="$binary $contract_options -- $testcase"

    $command &> /dev/null
    exit_code="$?"

    if [[ \
           "$exit_code" == "0" && "$okay" == "okay"  \
        || "$exit_code" != "0" && "$okay" == "error" \
    ]] ; then
        printf "$colored_okay  \\(^_^)/\n"
        # echo "    exit code: $exit_code, expected $expected"
    else
        zero_if_no_error="2"
        printf "$colored_error /(T_T)\\"
        echo
        echo "        exit code: $exit_code (expected $expected)"
        echo "        command: \"$command\""
    fi
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

# Run on all test directories.
for directory in `find "$root_dir" -type d -maxdepth 1` ; do
    # Skip top test directory.
    if [ "$root_dir" == "$directory" ] ; then
        continue
    fi

    run_tests "$directory"
done

exit $zero_if_no_error