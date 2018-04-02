# get important bootstrap scripts
TMPPATH=${HOME}/.local/bin/tmppath
mkdir -p ${TMPPATH}
for SCRIPT in insert-text-block \
		  git-fetch-ff; do
    if ! which ${SCRIPT}; then
	DEST=${TMPPATH}/${SCRIPT}
	
	curl https://raw.githubusercontent.com/erjoalgo/erjoalgo-gnu-scripts/master/${SCRIPT} \
	     -o ${DEST}
	chmod +x ${DEST}
	which ${SCRIPT}
    fi
done
PATH=$PATH:$TMPPATH