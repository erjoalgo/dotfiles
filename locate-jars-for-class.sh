#!/bin/bash

CLASS=${1} && shift
REPO=${1:-${HOME}/.m2/repository} && shift
#  https://stackoverflow.com/questions/1500141
for JAR in $(find ${REPO} -name '*.jar'); do
    jar tf "${JAR}" | grep "/${CLASS}[.]class" 1>&2  && echo "${JAR}"
done
