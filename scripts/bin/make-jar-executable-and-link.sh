#!/bin/bash -x

set -euo pipefail

JAR=$(python3 -c "import os; print(os.path.realpath('$1'));")
grep "[.]jar$" <<< "${JAR}" || exit ${LINENO}
EXE="${JAR}.sh"
LINKNAME=${LINKNAME:-$(basename ${EXE})}

cat /dev/stdin "${JAR}" <<'EOF' > "${EXE}"
#!/bin/sh
#https://coderwall.com/p/ssuaxa/how-to-make-a-jar-file-linux-executable
MYSELF=$(which "$0" 2>/dev/null)
[ $? -gt 0 -a -f "$0" ] && MYSELF="./$0"
test -n "$JAVA_HOME" && JAVA="$JAVA_HOME/bin/java"
JAVA=${JAVA:-java}
exec "$JAVA" $java_args -jar $MYSELF "$@"
exit 1
EOF

chmod +x "${EXE}"
BINDIR=${2:-/usr/local/bin}
DEST="${BINDIR}/${LINKNAME}"
sudo ln -sf "${EXE}" "${DEST}"
echo "${DEST}"
