#!/bin/bash -x

set -euo pipefail

# https://www.sindastra.de/p/807/quickly-kill-ipv6-leaks-on-your-openvpn-server/
# Note: this script should be run on the server.

sudo insert-text-block  \
    '# YCwGCBeukesO79tWUWy6lCGib8Tua3J4-force-client-ipv6-through-vpn'  \
    /etc/openvpn/server/server.conf <<EOF
# https://www.sindastra.de/p/807/quickly-kill-ipv6-leaks-on-your-openvpn-server/
# This will assign the network address of 2001:db8:0:123::/64 to your OpenVPN
# server, and then push a route so that all IPv6-internet addresses on the
# client side will be connected to through the VPN server.

server-ipv6 2001:db8:0:123::/64
push "route-ipv6 2000::/3"
EOF

sudo insert-text-block '# Qy8E1jodiG2Xb6Eb5mQLe7dYT4sgQpHI-disable-ipv6-forwardings'  \
     /etc/sysctl.conf<<EOF
# https://www.sindastra.de/p/807/quickly-kill-ipv6-leaks-on-your-openvpn-server/
# You might have IPv6 internet on the OpenVPN server but don’t want to support
# it. Now comes the part where we kill it off!
# First, make sure IPv6 forwarding is disabled. Edit
# /etc/sysctl.conf and uncomment net.ipv6.conf.all.forwarding=1 and change
# the 1 to a 0 to make sure IPv6 forwarding is not allowed.
net.ipv6.conf.all.forwarding=0
EOF
sudo sysctl --system

# Now, while we could leave it at that, you might notice that some sites will
# load slowly and some things will time out. This is because your client is
# trying to connect to an IPv6 address. To prevent this, we have to specifically
# reject all IPv6 connections from the OpenVPN clients.
# On a firewall manager like ufw, you will want to execute:
sudo ufw reject from 2001:db8:0:123::/64 to any

sudo service openvpn restart

# Now test on the client for ipv6 leak by navigating to http://ipv6leak.com/
