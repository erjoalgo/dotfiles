#!/bin/bash
if [ ${#} -eq 2 ]; then
    PRE=${1}
    shift
fi

# based on
# http://stackoverflow.com/questions/20988769/generate-uri-for-local-file-in-unix
# URI=$(readlink -f "$1") # get full path
URI=${@}
URI=${URI//%/%25}     # Substitute for percent signs
URI=${URI// /%20}     # Substitute for spaces
URI=${URI//+/%2B}     # Substitute for plus signs
# echo "file:$fname"
echo ${PRE}${URI}
