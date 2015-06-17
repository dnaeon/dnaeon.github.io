---
layout: post
title: Installing OpenVPN on Debian GNU/Linux
created: 1354120138
tags: linux vpn
---
Some time ago in a previous handbook I've posted about
[how to install and configure OpenVPN on FreeBSD](/node/46). 

In this post we are going to see how to install a routed OpenVPN
server on a GNU/Linux system. The target system we install OpenVPN
will be Debian GNU/Linux.

Here's a very simple illustration of our setup:

```text
[ Internet ] <---> [ Gateway ] <---> [ OpenVPN server]
```

And some information about our hosts and networks:

* 10.1.16.0/20 - internal network
* 10.1.16.1 - Gateway and DNS server in the internal network
* 10.1.16.2 - OpenVPN server in the internal network
* 10.1.32.0/20 - Subnet for our OpenVPN clients

Now that we have all the needed information lets go ahead and install
OpenVPN.

To better distinguish our firewall and OpenVPN servers I use a prefix
**openvpn#** and **firewall#** for all commands used in this post for
the firewall and OpenVPN servers respectively.

At the end of this post we will see what `iptables(8)` rules we need
to apply on our firewall and OpenVPN server for passing traffic
through.

## Installing OpenVPN

Now, lets install OpenVPN:

```bash
openvpn# apt-get install openvpn
```

## Configuring OpenVPN
	
Installation is done, now we need to configure our OpenVPN. Below you
will find a fully working configuration file for a routed OpenVPN
server.

```
# MS Fix for Apps like Remote Desktop
mssfix 1400 
 
# Local ip to listen on
local 10.1.16.2

# Listen on UDP port 1194
port 1194
proto udp
 
# Use routed VPN
dev tun

# SSL/TLS root certificate (ca), certificate
# (cert), and private key (key).
ca /etc/openvpn/easy-rsa/keys/ca.crt
cert /etc/openvpn/easy-rsa/keys/vpn.example.org.crt
key /etc/openvpn/easy-rsa/keys/vpn.example.org.key # This file should be kept secret

# Diffie hellman parameters.
dh /etc/openvpn/easy-rsa/keys/dh1024.pem
 
# OpenVPN IP Pool for clients
# Server will use 10.1.32.1 address
# Clients will be assigned addresses from the
# 10.1.32.0/20 subnet
server 10.1.32.0 255.255.240.0
 
# Maintain a record of client <-> virtual IP address
ifconfig-pool-persist ipp.txt
 
# Push routes to the clients
push "route 10.1.16.0 255.255.240.0"
push "route 10.1.32.0 255.255.240.0"

# Configure all clients to redirect their
# default network gateway through the VPN 
#push "redirect-gateway def1 bypass-dhcp"
 
# Certain Windows-specific network settings
# can be pushed to clients, such as DNS
# or WINS server addresses.  CAVEAT:
# http://openvpn.net/faq.html#dhcpcaveats
push "dhcp-option DNS 10.1.16.1"
push "dhcp-option DOMAIN example.org"

keepalive 10 120
 
# Blowfish encryption (default)
cipher BF-CBC 
 
# Enable compression on the VPN link.
comp-lzo
 
# The maximum number of concurrently connected clients
max-clients 100
 
# User and group the OpenVPN daemon runs as
user nobody
group nogroup
	
persist-key
persist-tun
 
# Output a short status file showing
# current connections, truncated
# and rewritten every minute.
status /var/log/openvpn-status.log
 
# Append log file to /var/log/openvpn.log
log-append /var/log/openvpn.log

# Verbosity level 
verb 3
mute 20
```

You can copy-paste the above configuration file, adjust your network
settings and save it under `/etc/openvpn/server.conf`.

## Certificate creation for server and clients

Next we will create the server's and client's certificates using the
*easy-rsa* scripts that come with the OpenVPN package. First lets get
the *easy-rsa* scripts.

```bash
openvpn# cd /etc/openvpn
openvpn# mkdir easy-rsa
openvpn# cp -a /usr/share/doc/openvpn/examples/easy-rsa/2.0/*  easy-rsa/
```
	
The `/etc/openvpn/easy-rsa/vars` file contains some default values
that will be used by the `easy-rsa` scripts during certificate
creation, so you might want to do some changes there first. Once ready
source the file and lets create the root certificate.

```bash
openvpn# cd /etc/openvpn/easy-rsa
openvpn# source vars
```

Create the root certificate (CA) by executing the command below:

```bash
openvpn# ./build-ca
```

Generate the server's key:

```bash
openvpn# ./build-key-server vpn.example.org
```

Create the Diffie-Hellman parameters:

```bash
openvpn# ./build-dh
```
	
That was for the server part. Now lets add a client's certificate as
well:

```bash
openvpn# ./build-key client.example.org
```
	
The above command will create the clients certificate files. Once that
is done you should securely send these files to the client in order to
connect to the OpenVPN server:

* ca.crt
* client.example.crt
* client.example.key

## Configuring routes

We also need to configure a route between our OpenVPN clients and the
firewall we have. The below command adds a route on the firewall
server (10.1.16.1) to the OpenVPN client's subnet (10.1.32.0/20) to go
through the OpenVPN server (10.1.16.2).

```bash
firewall# route add -net 10.1.32.0/20 gw 10.1.16.2
```

It would be good to have this route added automatically after a
reboot, and a good place for it would be to add a *post-up* clause in
your interfaces file, similar to this one:

```bash
auto eth0
iface eth0 inet static
  address 10.1.16.1
  network 10.1.16.0
  netmask 255.255.240.0
  gateway <my-external-gw>
  post-up route add -net 10.1.32.0/20 gw 10.1.16.2
  pre-down route del -net 10.1.32.0 gw 10.1.16.2
```
 
## Starting up the OpenVPN server

Now that we are ready, we can start up our OpenVPN server, by
executing the command below:

```bash
openvpn# service openvpn start
```
	
If you have any issues starting up OpenVPN, please check the log file
at `/var/log/openvpn.log`.
	
## Configuring your OpenVPN client

You can find a working client configuration file at the link below,
which you can use for your clients

* [OpenVPN client configuration](/node/49)

## Firewall rules with iptables(8) for OpenVPN

Please check the post below for more information on setting up
firewall rules with `iptables(8)` for your OpenVPN server.

* [Firewall rules with iptables for OpenVPN](/node/98)
