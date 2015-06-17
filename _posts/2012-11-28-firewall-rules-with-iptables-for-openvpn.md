---
layout: post
title: Firewall rules with iptables for OpenVPN
created: 1354120607
tags: linux vpn
---
This post is a follow-up of
[installing OpenVPN on Debian GNU/Linux](/node/97) post and provides
information on setting up your firewall rules with *iptables(8)* for
OpenVPN.

It assumes you have installed your OpenVPN server already as described
in [this post here](/node/97). Just as a reminder this is how our
hosts and networks looked like.

```text
[ Internet ] <---> [ Gateway ] <---> [ OpenVPN server]
```

And the hosts and networks we have:

* 10.1.16.0/20 - internal network
* 10.1.16.1 - Gateway and DNS server in the internal network
* 10.1.16.2 - OpenVPN server in the internal network
* 10.1.32.0/20 - Subnet for our OpenVPN clients

First we need to enable IP forwarding on both the OpenVPN server and
the firewall servers. On the OpenVPN server:

```bash
openvpn# echo 'net.ipv4.ip_forward=1' >> /etc/sysctl.conf
openvpn# sysctl -p
```

Do the same with the firewall server now:
	
```bash
firewall# echo 'net.ipv4.ip_forward=1' >> /etc/sysctl.conf
firewall# sysctl -p
```

Below is a simple firewall script that you can use on your firewall
server. You can copy-paste it and do just a few minor changes at the
very beginning for things like external interface, public address of
OpenVPN, etc.

```bash
#!/bin/sh

### BEGIN INIT INFO
# Provides:          firewall
# Required-Start:    
# Required-Stop:
# X-Start-Before:    
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6 
# Short-Description: Enables and disables firewall rules
# Description:       Enables and disables the firewall rules
#                    using iptables(8)
### END INIT INFO

LOCKFILE=/var/lock/firewall.lock
IPTABLES=/sbin/iptables

# External interface
EXT_IF=eth0

# OpenVPN public address
VPN_EXT_ADDR="a.b.c.d"

# OpenVPN private address
VPN_INT_ADDR="10.1.16.2"

set -e

. /lib/lsb/init-functions

# Load firewall rules
start_firewall() {

    if [ -f "${LOCKFILE}" ]; then
		log_failure_msg "Lock file exists, firewall is already enabled?"
		exit 1
	fi

	if ! lockfile ${LOCKFILE} ; then
		log_failure_msg "Cannot create a lock file!"
		exit 1
	fi

	log_success_msg "Enabling firewall rules using iptables(8)."

	# Remove any existing rules from all chains
	${IPTABLES} -F
	${IPTABLES} -F -t nat
	${IPTABLES} -F -t mangle

	# Remove any pre-existing user-defined rules
	${IPTABLES} -X
	${IPTABLES} -X -t nat 
	${IPTABLES} -X -t mangle
		
   	# Zero the counters
	${IPTABLES} -Z

	# Default policy
	${IPTABLES} -P INPUT DROP
	${IPTABLES} -P OUTPUT ACCEPT
	${IPTABLES} -P FORWARD ACCEPT

   	# Trust the local host
	${IPTABLES} -A INPUT -i lo -j ACCEPT

   	# Trust the internal networks
	${IPTABLES} -A INPUT -p all -m state --state NEW -s 10.0.0.0/8 -j ACCEPT

   	# Accept established sessions
	${IPTABLES} -A INPUT -m state --state RELATED,ESTABLISHED -j ACCEPT
    
        # NAT rules
	${IPTABLES} -t nat -A POSTROUTING -s 10.0.0.0/8 ! -d 10.0.0.0/8 -j MASQUERADE

        # One-To-One NAT for vpn.example.org
	${IPTABLES} -t nat -I PREROUTING -i ${EXT_IF} -d ${VPN_EXT_ADDR} -j DNAT --to-destination ${VPN_INT_ADDR} 
   	${IPTABLES} -t nat -I POSTROUTING -o ${EXT_IF} -s ${VPN_INT_ADDR} -j SNAT --to-source ${VPN_EXT_ADDR}

	log_success_msg "Firewall rules loaded successfully."
}

reset_firewall() {
    
	log_success_msg "Disabling iptables(8) firewall rules."

   	# Remove any existing rules from all chains
	${IPTABLES} -F
	${IPTABLES} -F -t nat
	${IPTABLES} -F -t mangle

   	# Remove any pre-existing user-defined rules
	${IPTABLES} -X
	${IPTABLES} -X -t nat 
	${IPTABLES} -X -t mangle
   
	# Zero the counters
	${IPTABLES} -Z
			
	${IPTABLES} -P INPUT ACCEPT
	${IPTABLES} -P OUTPUT ACCEPT
	${IPTABLES} -P FORWARD ACCEPT

	log_success_msg "Firewall shutdown successful."
}

status_firewall() {
		
	if [ -f "${LOCKFILE}" ]; then
		log_success_msg "Firewall is enabled."
	else
		log_success_msg "Firewall is disabled."
	fi
}
	
case "${1}" in
	start)
		start_firewall
		;;
	reset)
		reset_firewall
		;;
	stop)
		reset_firewall
		rm -f "${LOCKFILE}"
		;;
	status)
		status_firewall
		;;
	reload|restart|force-reload)
		reset_firewall
		rm -f "${LOCKFILE}"
		start_firewall
		;;
	*)
		echo "usage: ${0} {start|stop|reload|restart|force-reload|reset" >&2
		;;
esac
```

You can drop this script in `/etc/init.d/firewall` and then enable it
during boot-time by executing the command below:

```bash
firewall# update-rc.d firewall defaults
```
	
Enable the firewall rules by executing this command:

```bash
firewall# service firewall start
```

And that was for the server part. What's left is to apply a few
*iptables(8)* rules on the OpenVPN server as well:

```bash
openvpn# iptables -A FORWARD -i eth0 -o tun0 -m state --state ESTABLISHED,RELATED -j ACCEPT
openvpn# iptables -A FORWARD -s 10.0.0.0/8 -o eth0 -j ACCEPT
openvpn# iptables -t nat -A POSTROUTING -s 10.0.0.0/8 ! -d 10.0.0.0/8 -o eth0 -j MASQUERADE
```
	
In order to get these *iptables(8)* rules applied after a reboot of
the OpenVPN server you could place a file in
`/etc/network/if-pre-up.d` directory, which will get executed during
network initialization.

Below is an example script that I've used on one of my OpenVPN servers
to make sure the *iptables(8)* rules are applied after a reboot.

This script has been placed in
`/etc/network/if-pre-up.d/iptables-openvpn-rules` and it's content are
listed below:

```bash
#!/bin/sh

case "$IFACE" in
	eth0)
		iptables -A FORWARD -i eth0 -o tun0 -m state --state ESTABLISHED,RELATED -j ACCEPT
		iptables -A FORWARD -s 10.0.0.0/8 -o eth0 -j ACCEPT
		iptables -t nat -A POSTROUTING -s 10.0.0.0/8 ! -d 10.0.0.0/8 -o eth0 -j MASQUERADE
		;;
	*)
		exit 0
	;;
esac
```

Now after a reboot your OpenVPN `iptables(8)` rules should be applied
properly. And that should be all with the `iptables(8)` part of your
OpenVPN setup!
