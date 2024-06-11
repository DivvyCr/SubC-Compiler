#!/bin/bash

MAKE=1
while getopts ":n" arg; do
	case $arg in
		n) MAKE=0
	esac
done

cd ../src/

if [ $MAKE -eq 1 ]
then
	make clean && make
fi

RUN="$(pwd)/subc"

cd ../tests/
for testfile in ./modules/*.c; do
	$RUN $testfile > /dev/null
done

clang++ driver.cpp ./modules/*.ll -o run -Wno-override-module
./run

rm ./modules/*.ll
rm ./run
