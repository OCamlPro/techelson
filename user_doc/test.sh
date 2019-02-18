#! /bin/bash

timeout="5"
command_pref="./bin/techelson --skip on"
root_dir="user_doc/"

# Prints an error, runs a command (if provided), and exits.
#
# Arguments
# - error message,
# - optional command to run (None if "").
function bail {
    err_msg="$1"
    err_cmd="$2"
    printf "|===| \033[31mError:\033[0m\n"
    echo "| $err_msg"
    if [ "$err_cmd" != "" ] ; then
        echo "|===> $err_cmd"
        timeout $timeout $err_cmd
    fi
    echo "|===|"
}

for test_dir in `ls "${root_dir}rsc"` ; do
    test_dir="${root_dir}rsc/$test_dir"

    all_contracts=""
    for contract in `find $test_dir/contracts -type f -iname "*.tz"` ; do
        echo "checking contract $contract"
        all_contracts="$all_contracts --contract $contract"
        command="$command_pref --contract $contract --"
        timeout $timeout $command &> /dev/null
        exit_code="$?"
        if [ "$exit_code" != "0" ] ; then
            bail "failed to load contract $contract" "$command"
        fi
    done

    for techel in `find $test_dir/okay -type f -iname "*.techel"` ; do
        echo "checking testcase $techel"
        command="$command_pref $all_contracts -- $techel"
        command_output=`$command`
        techel_output="$techel.output"
        if [ -f "$techel_output" ] ; then
            diff <(cat "$techel_output") <(echo "$command_output")
            exit_code="$?"
            if [ "$exit_code" != "0" ] ; then
                bail \
                    "testcase output on testcase $techel is different for expectation"
            fi
        fi
    done
done
