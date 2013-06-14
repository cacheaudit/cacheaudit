#!/bin/bash
echo "" > output.txt
for file in *
do
    if [ -d "$file" ]; then
    	result='ok';
        cd "$file";
        echo -en "Test $file : running... (tail -f outuput.txt)"
        echo -e "\n\n\n--- $file ---\n\n" >> ../output.txt
        ./run.sh >> ../output.txt 2>&1;
        if [ $? -eq 0 ]; then
        	echo -en "\033[32m"
        else
        	echo -en "\033[31m"
        	result='err'
        fi
        echo -e "\r\033[KTest $file : $result\n\n\033[39m"
        cd ..;
    fi
done

exit 0