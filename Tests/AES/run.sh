#!/bin/bash
if [ ! -f ../../cachecow ]; then
    echo "Cachetool not compiled!";
    exit 1;
fi
../../cachecow --analyze --noInstructionCache aes_inlined
if [ ! -f log.txt ]; then
	echo "Log.txt not found"
    exit 1;
fi
cat log.txt | sed 's/^...........//' > result.txt
cmp correct.txt result.txt &> /dev/null
result=$?
rm -f result.txt log.txt
if [ $result -eq 0 ]; then
	echo "test ok" 1>&2;
else
	echo "test failed" 1>&2;
fi
exit $result;