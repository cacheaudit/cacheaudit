#!/bin/bash
source ../common.sh

runCacheCow aes_inlined
createResult

cmp correct.txt result.txt
result=$?
#rm -f result.txt log.*
if [ $result -eq 0 ]; then
	echo "test ok" 1>&2;
else
	echo "test failed" 1>&2;
fi
exit $result;
