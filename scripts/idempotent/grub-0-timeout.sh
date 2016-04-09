#!/bin/bash -x

if grep -P '^GRUB_TIMEOUT=[0-9]+$' /etc/default/grub \
	| grep -v '=0'; then
    echo "updating grub"
    sudo sed -i 's/^GRUB_TIMEOUT=[0-9]*$/GRUB_TIMEOUT=0/' \
	 /etc/default/grub
    sudo update-grub
fi
