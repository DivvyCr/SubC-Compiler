#!/bin/bash

cd ../src/
# make clean && make
RUN="$(pwd)/subc"

cd ../tests/
for testfile in ./*.c; do
	$RUN ${testfile:2} > /dev/null
done

clang++ driver.cpp *.ll -o run
./run

rm ./*.ll
rm ./run
