#!/bin/bash

EXECUTABLE=cacheaudit

#Calls cachecow and exits when it isnt compiled or has failed with an exception
function runCacheCow {
	if [ ! -f ../../$EXECUTABLE ]; then
	    echo "Cachetool not compiled!";
	    exit 1;
	fi
	../../$EXECUTABLE $1
	if [ ! $? -eq 0 ]; then
		echo "Cachetool exited with error."
		exit 1;
	fi
	if [ ! -f log.txt ]; then
		echo "log.txt not found."
	    exit 1;
	fi
}

# Splits the file in two, and parses the second half into result.txt
function createResult {
	#total_lines=$(cat log.txt | wc -l)
	#((lines_per_file = total_lines / 2))
	#split -l ${lines_per_file} log.txt log.
	cat log.txt | sed 's/^...........//' > result.txt
}
