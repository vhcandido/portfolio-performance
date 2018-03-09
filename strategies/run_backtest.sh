#!/bin/bash

if [ $# -eq 0 ]; then
    echo "No arguments supplied"
    exit
fi

for i in $(ls -1 ~/Documents/usp/research/currency-strength/data | grep .csv); do
	pair=${i/.csv/}
	Rscript $1 $pair '2014-07::' 'days'
done
