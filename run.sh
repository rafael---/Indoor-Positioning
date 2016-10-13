#!/bin/sh

if [ $(which R) != "" ]; then
    R --no-save < main.r
else
    echo "R is not installed in your system\n"
fi

exit
