#!/bin/bash

cat $SXHKD_FIFO | while read -r line; do
    echo $line
    if [[ $line == *"BBegin chain"* ]]; then
        polybar-msg action sxhkd hook 1
    elif [[ $line == *"EEnd chain"* ]]; then
        polybar-msg action sxhkd hook 0
    fi
done




