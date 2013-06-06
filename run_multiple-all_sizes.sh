#!/bin/bash

for strategy in lru fifo plru
do
  if [ $strategy == lru ]
  then
    str_command=""
  else
    str_command="--$strategy"
  fi
  for domain in Set IV
  do
    if [ $domain == Set ]
    then
      dom_command=""
    else
      dom_command="--interval-cache"
    fi
    for csize in 131072 #4096 8192 16384 32768 65536 262144 3145728
    do
      for f in aes_nosched aes_nosched_preloading
      do
        echo "./cachecow ../../trunk/CaseStudies/Crypto/AES/$f --analyze $str_command $dom_command --cache-size $csize > results/$f-$strategy-$domain-$csize"
        ./cachecow ../../trunk/CaseStudies/Crypto/AES/$f --analyze $str_command $dom_command --cache-size $csize > results/$f-$strategy-$domain-$csize
      done
    done
  done
done