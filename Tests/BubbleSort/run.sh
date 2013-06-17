#!/bin/bash
source ../common.sh

runCacheCow bubblesort
createResult

sort -n result.txt -o correct.txt
cmp correct.txt result.txt &> /dev/null
result=$?
rm -f correct.txt result.txt log.*
if [ $result -eq 0 ]; then
	echo "test ok" 1>&2;
else
	echo "test failed" 1>&2;
fi
exit $result;
