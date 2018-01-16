#!/bin/bash -x

if ! command -v gprof2dot || ! command -v dot; then
    echo "gprof2dot and/or dot not installed"
    exit ${LINENO}
fi

STATS_FN=${1:-"prof"}
if -e gmon.out; then
    gprof > prof
    STATS_FN="prof"
fi
PROF_TOP=prof-results
test -d "${PROF_TOP}" || mkdir "${PROF_TOP}"
CALLGRAPH_FN="${PROF_TOP}/callgraph.svg"

gprof2dot -f prof "${STATS_FN}" | dot -Tsvg -o "${CALLGRAPH_FN}"

firefox --new-tab "${CALLGRAPH_FN}"

# command = ["python", "-OO", "-u", "-m", "cProfile", "-o", stats_fn, program_abs]+rem_args
