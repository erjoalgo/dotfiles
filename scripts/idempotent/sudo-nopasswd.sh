#!/bin/bash -x

SUDO_ASKPASS=$(which false) sudo -A echo hola
if test 0 -ne $?; then
	SUDOCMD="su"
else
	SUDOCMD="sudo bash"
fi

#ealfonso ALL=(ALL:ALL) NOPASSWD:ALL
LINE="${USER} ALL=(ALL:ALL) NOPASSWD:ALL"
SUDOERS="/etc/sudoers"
if ! ${SUDOCMD} -c "grep -F '${LINE}' ${SUDOERS}"; then
	${SUDOCMD} -c "echo '${LINE}' >> ${SUDOERS}"
fi
