#!/bin/sh

################################################################################
PROF_ROW_SIZE=512
PROF_ROW_COUNT=10000 # Actual rows will be this number * 100

################################################################################
OUT_FILE=$1
CSV_HEADER=$2

################################################################################
cp $CSV_HEADER $OUT_FILE

################################################################################
for i in `seq $PROF_ROW_COUNT`; do
  title=`openssl rand -base64 $PROF_ROW_SIZE | paste -sd/`
  time=`echo|awk 'srand() {print rand()}'`

  if [ `expr $i % 2` -eq 0 ]; then
    row="TODO,,$title"
  else
    row="TODO,$time,$title"
  fi

  for j in `seq 100`; do echo "$row" >> $OUT_FILE; done
  printf .
done

################################################################################
echo # Print blank line to terminate all those dots.
