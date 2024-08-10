#!/bin/sh

DIR=$1
shift

OPT=$@

SRC=$(ls files/normal/src/*.c)

run_once() {
  mode=$1
  test=$2

  owi c \
    $OPT \
    $mode \
    -I ./files/normal/include \
    $SRC \
    $test

  return_code=$?
  [ $return_code -ne 0 ] && printf "\n[$return_code]\n"
}

for test in $(ls files/normal/testsuite/$DIR/*.c); do
  printf "Testing \"$test\":\n"

  printf "Using owi sym:\n"
  run_once "" "$test"

  printf "Using owi conc:\n"
  run_once "--concolic" "$test"
done

exit 0
