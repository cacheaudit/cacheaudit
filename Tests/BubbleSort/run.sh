#!/bin/bash
#../../cachecow --analyze --noInstructionCache bubbleSort
cat log.txt | sed "s/.*\\\{\([^\\\}]\+\)\\\}.*/\1/" > result.txt
sort result.txt -o correct.txt
cmp correct.txt result.txt
echo $?
