#!/bin/bash
failed_tests=""
failed=0
for i in $(echo `ls | grep testcase`) ; do
    if [ -e "$i/$i.ende" ]; then
	echo "Testing $i"
	../backend/target/debug/ende "$i/$i.ende" -o "$i/$i"
	if [ $? -ne 0 ]; then
	    failed=1
	else
	    chmod +x $i/$i
	    $i/$i
	    if [ $? -ne 0 ]; then
		failed=1
	    else
		failed=0
	    fi
	fi
	if [ $failed -eq 0 ]; then
	    echo "test $i succeed"
	else
	    echo "testcase $i failed"
	    failed_tests="$i $failed_tests"
	fi
    fi
done
if [ "$failed_tests" != "" ]; then
    echo "failed tests: $failed_tests"
    exit 1
fi
