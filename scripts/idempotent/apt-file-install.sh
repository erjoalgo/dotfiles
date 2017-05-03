#!/bin/bash -x

if ! command -v apt-file; then
	sudo apt-get install -y apt-file
	sudo apt-file update
fi
