#!/bin/bash -x

test -e setup.py || exit ${LINENO}
sudo rm -rf build  dist *.egg-info
python setup.py sdist
VERSION=$(python setup.py --version) || exit ${LINENO}
PROJECT_NAME=$(basename $(pwd))
TARGZ=dist/${PROJECT_NAME}-${VERSION}.tar.gz
test -e ${TARGZ} || exit ${LINENO}
gpg --detach-sign -a ${TARGZ} || exit ${LINENO}
gpg --verify -a ${TARGZ}{.asc,} || exit ${LINENO}

if ! command -v twine; then
    echo "twine not available, installing it..."
    pip install --user twine
fi

CMD=$(echo twine upload ${TARGZ}{,.asc})
read -p "${CMD} :"
${CMD}
