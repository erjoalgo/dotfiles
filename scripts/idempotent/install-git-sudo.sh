#!/bin/bash
APT_GET="apt-get"
if command -v yum; then
	APT_GET="yum"
fi


if ! command -v git || ! command -v sudo; then
    sudo ${APT_GET} install -y git sudo || exit ${LINENO}
fi
