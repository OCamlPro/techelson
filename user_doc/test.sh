#! /bin/bash

timeout="5"
command_pref="../bin/techelson --skip on"
root_dir="./"

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
    exit 2
}

for test_dir in `ls "${root_dir}rsc"` ; do
    test_dir="${root_dir}rsc/$test_dir"

    echo "working on directory $test_dir..."

    if [ -d $test_dir/contracts ] ; then
        echo "  checking contracts"
        for contract in `find $test_dir/contracts -type f -iname "*.tz"` ; do
            echo "  - $contract"
            all_contracts="$all_contracts --contract $contract"
            command="$command_pref --contract $contract --"
            timeout $timeout $command &> /dev/null
            exit_code="$?"
            if [ "$exit_code" != "0" ] ; then
                bail "failed to load contract $contract" "$command"
            fi
        done
    fi


    if [ -d $test_dir/okay ] ; then
        echo "  verifying output files (okay)"

        for output_file in `find $test_dir/okay -type f -iname "*.techel.output"` ; do
            echo "  - $output_file"
            command=`head -n 1 "$output_file"`
            if [[ $command != \$* ]] ; then
                bail "output file $output_file does not start with a '\$ <command>' line"
            fi
            command=`echo "$command" | sed -e "s:\$ techelson:$command_pref:"`
            command_output=`timeout $timeout $command`
            exit_code="$?"
            if [ "$exit_code" != "0" ] ; then
                bail "testcase was expected to succeed, ran into an error" "$command"
            fi
            diff <(tail -n +2 "$output_file") <(echo "$command_output")
            exit_code="$?"
            if [ "$exit_code" != "0" ] ; then
                bail "run output on $output_file is different from expectation"
            fi
        done
    fi

    if [ -d $test_dir/error ] ; then
        echo "  verifying output files (error)"

        for output_file in `find $test_dir/error -type f -iname "*.techel.output"` ; do
            echo "  - $output_file"
            command=`head -n 1 "$output_file"`
            if [[ $command != \$* ]] ; then
                bail "output file $output_file does not start with a '\$ <command>' line"
            fi
            command=`echo "$command" | sed -e "s:\$ techelson:$command_pref:"`
            command_output=`timeout $timeout $command`
            exit_code="$?"
            if [ "$exit_code" == "0" ] ; then
                bail "testcase was expected to fail, but it succeeded" "$command"
            fi
            diff <(tail -n +2 "$output_file") <(echo "$command_output")
            exit_code="$?"
            if [ "$exit_code" != "0" ] ; then
                bail "run output on $output_file is different from expectation"
            fi
        done
    fi
done
