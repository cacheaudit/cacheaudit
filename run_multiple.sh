#!/bin/bash

for strategy in lru # fifo plru
do
  if [ $strategy == lru ]
  then
    str_command=""
  else
    str_command="--$strategy"
  fi
  for domain in Set # IV
  do
    if [ $domain == Set ]
    then
      dom_command=""
    else
      dom_command="--interval-cache"
    fi
    for line_s in 64 # 32 128
    do
      for assoc in 1 2 # 8 4 
      do
        for csize in 3145728 #4096 8192 16384 32768 65536 131072 262144 # 3145728
        do
          for f in aes_nosched aes_nosched_preloading 
          do
            command="./cachecow ../../trunk/case_studies/Crypto/AES/$f --analyze $str_command $dom_command --cache-size $csize --assoc $assoc --line-size $line_s" 
            where="results/$f-$strategy-$domain-$csize-$assoc-way"
            echo $command ">" $where
            $command > $where
          done
        done
      done
    done
  done
done
