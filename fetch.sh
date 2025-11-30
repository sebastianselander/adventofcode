#!/usr/bin/env bash

YEAR=$1
DAY=$2
if [ -z $YEAR ] || [ -z $DAY ]; then
    YEAR=$(date | awk '{print $4}')
    MONTH=$(date | awk '{print $3}')
    DAY=$(date | awk '{print $2}')
    if [ "dec" != $MONTH ]; then
        echo "Bad month:" $MONTH
        exit
    fi
fi
if [ 1 -eq ${#DAY} ]; then
    DAY_OUTPUT="0$DAY"
else
    DAY_OUTPUT=$DAY
fi

OUTPUT_PATH="./inputs/$YEAR/$DAY_OUTPUT.txt"

curl --cookie cookie https://adventofcode.com/$YEAR/day/$DAY/input > $OUTPUT_PATH
echo $OUTPUT_PATH
bat $OUTPUT_PATH | tail
