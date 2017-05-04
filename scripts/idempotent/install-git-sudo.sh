#!/bin/bash
APT_GET="apt-get"
if command -v yum; then
	APT_GET="yum"
fi


sudo ${APT_GET} install -y git sudo || exit ${LINENO}
