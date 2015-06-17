---
layout: post
title: Installing and configuring OpenVPN server on FreeBSD
tags: freebsd vpn
---
In this handbook we will have a look at OpenVPN - a full-featured
open-source SSL VPN solution and see how to install and configure it
under FreeBSD.

Please refer to the [official web site of OpenVPN](http://openvpn.net)
for more information about it.

In the following handbook we are going to install and configure a
routed OpenVPN server.

For more information about routed and bridged setup of OpenVPN, please
have a look at the following page:

* [http://openvpn.net/index.php/open-source/documentation/howto.html#vpntype](http://openvpn.net/index.php/open-source/documentation/howto.html#vpntype)

## Installation

In order to install OpenVPN we are going to use the [FreeBSD Ports
Collection](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports.html)

To start the installation execute the command below:

```bash
$ cd /usr/ports/security/openvpn && sudo make install clean
```

![_config.yml]({{ site.baseurl }}/images/openvpn-config.jpg)

Once the installation is over you should see something similar:

```bash
### ------------------------------------------------------------------------
###  Edit /etc/rc.conf[.local] to start OpenVPN automatically at system
###  startup. See /usr/local/etc/rc.d/openvpn for details.
### ------------------------------------------------------------------------
###  For compatibility notes when interoperating with older OpenVPN
###  versions, please, see <http://openvpn.net/relnotes.html>
### ------------------------------------------------------------------------
===> Installing rc.d startup script(s)
===>   Compressing manual pages for openvpn-2.1.4
===>   Running ldconfig
/sbin/ldconfig -m /usr/local/lib
===>   Registering installation for openvpn-2.1.4
===> SECURITY REPORT: 
      This port has installed the following files which may act as network
      servers and may therefore pose a remote security risk to the system.
/usr/local/sbin/openvpn

      This port has installed the following startup scripts which may cause
      these network services to be started at boot time.
/usr/local/etc/rc.d/openvpn

      If there are vulnerabilities in these programs there may be a security
      risk to the system. FreeBSD makes no guarantee about the security of
      ports included in the Ports Collection. Please type 'make deinstall'
      to deinstall the port if this is a concern.

      For more information, and contact details about the security
      status of this software, see the following webpage: 
http://openvpn.net/index.php/open-source.html
===>  Cleaning for openvpn-2.1.4
```

Now that we have OpenVPN installed, let's continue with it's
configuration on the next steps.

## Generating certificates for server

OpenVPN's default install directory and configuration resides in
*/usr/local/share/doc/openvpn*

First, let's create a configuration directory for OpenVPN under
*/usr/local/etc/openvpn*

```bash
$ sudo mkdir /usr/local/etc/openvpn
```

Now let's copy the sample configuration for the OpenVPN server, and
the scripts we are going to need, in order to generate the
certificates and keys for our server and clients.

```bash
$ sudo cp /usr/local/share/doc/openvpn/sample-config-files/server.conf /usr/local/etc/openvpn
$ sudo cp -a /usr/local/share/doc/openvpn/easy-rsa /usr/local/etc/openvpn
```

In order to create the certificate files and keys we are going to use
the *easy-rsa* scripts which come with OpenVPN.

We need to make these scripts executable first, so to do that execute
the command below:

```bash
$ sudo chmod 0755 /usr/local/etc/openvpn/easy-rsa/2.0/*
```

The */usr/local/etc/openvpn/easy-rsa/2.0/vars* file contains some
default values that will be used by the *easy-rsa* scripts during
certificate creation.

The variables defined in the *vars* file will be used as defaults
during certificate creation, but you will be prompted to change them
if needed.

Before creating the certificates we need to source the
*/usr/local/etc/openvpn/easy-rsa/2.0/vars* file first.

**NOTE:** FreeBSD's default shell is /bin/tcsh. The
  */usr/local/etc/openvpn/easy-rsa/2.0/vars* file uses **export** for
  *setting variable values, so you will have to switch to a shell that
  *understands this. So if you are using /bin/tcsh, first change your
  *shell to /bin/sh and then source the file. Next commands assume you
  *are already using /bin/sh as your shell.

```bash
cd /usr/local/etc/openvpn/easy-rsa/2.0 && . vars
```

Please note that the dot '.' before the *vars* file is needed for
sourcing the file.

Now let's create the CA:

```bash
# ./build-ca
Generating a 1024 bit RSA private key
...++++++
....................................++++++
writing new private key to 'ca.key'
-----
You are about to be asked to enter information that will be
incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or
a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [BG]:
State or Province Name (full name) [Sofia]:
Locality Name (eg, city) [Sofia]:
Organization Name (eg, company) [example.org]:
Organizational Unit Name (eg, section) []:
Common Name (eg, your name or your server's hostname)
[openvpn.example.org CA]:openvpn.example.org
Name []:daemon
Email Address [admin@example.org]:
#
```

Now let's generate the server key file:

```bash
# ./build-key-server openvpn.example.org
Generating a 1024 bit RSA private key
.++++++
...........................++++++
writing new private key to 'openvpn.example.org.key'
-----
You are about to be asked to enter information that will be
incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or
a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [BG]:
State or Province Name (full name) [Sofia]:
Locality Name (eg, city) [Sofia]:
Organization Name (eg, company) [example.org]:
Organizational Unit Name (eg, section) []:
Common Name (eg, your name or your server's hostname)
[openvpn.example.org]:
Name []:daemon
Email Address [admin@example.org]:

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:my-password
An optional company name []:
Using configuration from
/usr/local/etc/openvpn/easy-rsa/2.0/openssl.cnf
Check that the request matches the signature
Signature ok
The Subject's Distinguished Name is as follows
countryName           :PRINTABLE:'BG'
stateOrProvinceName   :PRINTABLE:'Sofia'
localityName          :PRINTABLE:'Sofia'
organizationName      :PRINTABLE:'example.org'
commonName            :PRINTABLE:'openvpn.example.org'
name                  :PRINTABLE:'daemon'
emailAddress          :IA5STRING:'admin@example.org'
Certificate is to be certified until Mar  3 15:14:23 2021 GMT (3650
days)
Sign the certificate? [y/n]:y


1 out of 1 certificate requests certified, commit? [y/n]y
Write out database with 1 new entries
Data Base Updated
```

Now let's create the Diffie-Hellman parameters:

```bash
# ./build-dh
Generating DH parameters, 1024 bit long safe prime, generator 2
This is going to take a long time
..............+..............................................................................+...........................+...........................+..........................................................+....................+....................................................+.............+...........................................................................................................+.............+...................+.+........+............+.......+..............................................................................................+....+...+......................+...+......+...............................................................+.............+..................+...........................................+......................................+...............+.................................................................+.........................................................................................................+...........................................................+.........................................................+..............................................+...+.+...........+..............................................................+..............................................................................+.......+...........................+...................................................+........................+.................+...........................+.......................................................................+.................+........+...................+...................................................+.......................+.......................................................+...........................+..+..........................................+..................................................................+............+.....+..........+.............................+...........+..............+........................................................+.....+.....+............................................+..............+....................+.....................................................................................................................................................................................+..........................................................................................................+....................................................................................................+...........+.................+........................................+...+.....................+...............................................................................................................+......................................+................................+.............+.............+................................................+..............+.................+..........................................+...+.....................................................+...........................................................+......................+..........................................................+..............................+.....................+...............+.............+........................................................................................+...........................+........................+.+.........................................................................+....+......................................................................................................................................................+.........................................................................................................................................................................................................................................................................................................................................................................................+.............................................+.......+.......................+................+......................+.................................................................+....+....................................+................+...............++*++*++*
```

Now we need to copy the files we've generated to the
*/usr/local/etc/openvpn/keys* directory:

```bash
$ sudo mkdir /usr/local/etc/openvpn/keys
$ sudo cp /usr/local/etc/openvpn/easy-rsa/2.0/keys/* /usr/local/etc/openvpn/keys
```

Once ready with that, execute the clean-all script, so that you clean
up everything from the keys directory.

```bash
# ./clean-all
```

## Generating certificates for clients

In this step we will create certificates for our clients.

In order to create a certificate for a client execute the command
below (note that you need the *vars* files source already):

```bash
# ./build-key client.example.org
Generating a 1024 bit RSA private key
..........................++++++
.........++++++
writing new private key to 'client.example.org.key'
-----
You are about to be asked to enter information that will be
incorporated
into your certificate request.
What you are about to enter is what is called a Distinguished Name or
a DN.
There are quite a few fields but you can leave some blank
For some fields there will be a default value,
If you enter '.', the field will be left blank.
-----
Country Name (2 letter code) [BG]:
State or Province Name (full name) [Sofia]:
Locality Name (eg, city) [Sofia]:
Organization Name (eg, company) [example.org]:
Organizational Unit Name (eg, section) []:
Common Name (eg, your name or your server's hostname)
[client.example.org]:
Name []:client.example.org
Email Address [admin@example.org]:client@example.org

Please enter the following 'extra' attributes
to be sent with your certificate request
A challenge password []:my-password
An optional company name []:
Using configuration from
/usr/local/etc/openvpn/easy-rsa/2.0/openssl.cnf
Check that the request matches the signature
Signature ok
The Subject's Distinguished Name is as follows
countryName           :PRINTABLE:'BG'
stateOrProvinceName   :PRINTABLE:'Sofia'
localityName          :PRINTABLE:'Sofia'
organizationName      :PRINTABLE:'example.org'
commonName            :PRINTABLE:'client.example.org'
name                  :PRINTABLE:'client.example.org'
emailAddress          :IA5STRING:'client@example.org'
Certificate is to be certified until Mar  3 15:22:45 2021 GMT (3650
days)
Sign the certificate? [y/n]:y


1 out of 1 certificate requests certified, commit? [y/n]y
Write out database with 1 new entries
Data Base Updated
```

In order to create certificates for all clients that you will have,
just execute the above command for each client that will be connecting
to your OpenVPN server.

Keep in mind that each client certificate you create, needs to have a
unique *Common Name (CN)*.

Once you've created certificates for all your clients, you need to
copy the client certificates to the */usr/local/etc/openvpn/keys*
directory.

```bash
$ sudo cp /usr/local/etc/openvpn/easy-rsa/2.0/keys/* /usr/local/etc/openvpn/keys
```

The next thing to do is to provide the generated certificates to the
clients. Please use a secure connection to do that, and do not just
send them over the Internet.

* ca.crt
* client.crt
* client.key

When you are ready with all that you may execute the *clean-all*
script to clean-up the files in the
*/usr/local/etc/openvpn/easy-rsa/2.0/keys* directory.

## OpenVPN server configuration

Now we are going to configure the OpenVPN server itself.

The OpenVPN server configuration will reside in the
*/usr/local/etc/openvpn/server.conf* file.

Below you will find a working configuration file for routed OpenVPN
setup.

Please refer to the manual page of openvpn(8) for more information on
the different options.

The example configuration file is pretty well commented as well, and
can be found in
*/usr/local/share/doc/openvpn/sample-config-files/server.conf* file.

The network topology we have for this setup is as shown below:

```bash
[ Internet ] <---> [ Gateway ] <---> [ OpenVPN server]
```

The OpenVPN server that we have will be located in a private network
behind a firewall.

Where in our example private network we have:

* 10.1.16.0/20 - internal network
* 10.1.16.1 - Gateway and DNS server in the internal network
* 10.1.16.3 - IP address of the OpenVPN server in the internal network
* 10.1.32.0/20 - Subnet for our OpenVPN clients

We will add a route for each client from the 10.1.32.0/20 subnet
(OpenVPN clients), so that they can access the internal network in
10.1.16.0/20

```text
# MS Fix for Apps like Remote Desktop
mssfix 1400

# Local ip to listen on
local 10.1.16.3

# Listen on UDP port 1194
port 1194
proto udp

# Use routed VPN
dev tun

# SSL/TLS root certificate (ca), certificate
# (cert), and private key (key).
ca /usr/local/etc/openvpn/keys/ca.crt
cert /usr/local/etc/openvpn/keys/openvpn.example.org.crt
key /usr/local/etc/openvpn/keys/openvpn.example.org.key # This file
# should be kept secret

# Diffie hellman parameters.
dh /usr/local/etc/openvpn/keys/dh1024.pem

# OpenVPN IP Pool for clients
# Server will use 10.1.32.1 address
# Clients will be assigned addresses from the
# 10.1.32.0/20 subnet
server 10.1.32.0 255.255.240.0

# Maintain a record of client <-> virtual IP address
ifconfig-pool-persist ipp.txt

# Push routes to the clients
# Add route for accessing the 10.1.16.0/20 subnet
push "route 10.1.16.0 255.255.240.0"

# Configure all clients to redirect their
# default network gateway through the VPN
push "redirect-gateway def1 bypass-dhcp"

# Certain Windows-specific network settings
# can be pushed to clients, such as DNS
# or WINS server addresses.  CAVEAT:
# http://openvpn.net/faq.html#dhcpcaveats
push "dhcp-option DNS 10.1.16.1"

keepalive 10 120

# Blowfish encryption (default)
cipher BF-CBC

# Enable compression on the VPN link.
comp-lzo

# The maximum number of concurrently connected clients
max-clients 10

# User and group the OpenVPN daemon runs as
user nobody
group nobody

persist-key
persist-tun

# Output a short status file showing
# current connections, truncated
# and rewritten every minute.
status /var/log/openvpn-status.log

# Append log file to /var/log/openvpn.log
log-append   /var/log/openvpn.log

# Verbosity level
verb 3
mute 20
```

## Creating routes to the OpenVPN server

If your OpenVPN server runs in an internal network where all your
clients will be assigned addresses from as well, you can skip this
step safely.

If your setup is similar to the shown above, then we will need to add
some additional routes to the machines from the internal 10.1.16.0/20
network, so that they can reach the OpenVPN server from the
10.1.32.0/20 network.

Since our OpenVPN server is part of the 10.1.16.0/20 internal network
and our clients will be assigned addresses from the 10.1.32.0/20
network, we need to tell the machines from our private network
(10.1.16.0/20) how they can reach the OpenVPN server in the
10.1.32.0/20 network.

Doing so would also allow the OpenVPN clients to reach our hosts from
the 10.1.16.0/20 network.

To add the routes manually on the machines from the 10.1.16.0/20
network, execute the command below:

```bash
$ sudo route add -net 10.1.32.0/20 <openvpn-server-ip> 255.255.240.0
```

You need to do this at least on one machine, and that's your network
gateway.

To make this route permanent during boot-time, add the following lines
to your */etc/rc.conf* file:

```bash
static_routes="openvpn_clients"
route_openvpn_clients="-net 10.1.32.0/20 10.1.16.3 255.255.240.0"
```

## Logging via syslog-ng3

If you are using
[sysutils/syslog-ng3](http://freshports.org/sysutils/syslog-ng3), and
want to be able to do logging for OpenVPN, you will need to add the
following lines to your */usr/local/etc/syslog-ng.conf*:

```text
#
# destinations
#

destination d_openvpn      { file("/var/log/openvpn.log"); };

#
# program filters
#

filter f_openvpn           { program("openvpn"); };

#
# !openvpn
# *.*
#
log { source(s_local); filter(f_openvpn); destination(d_openvpn); };
```

And then reload syslog-ng configuration:

```bash
$ sudo /usr/local/etc/rc.d/syslog-ng reload
```

## Starting the OpenVPN server

In order to enable the OpenVPN server during boot-time, you need to
add the following lines to your */etc/rc.conf* file:

```bash
# Enable OpenVPN server
openvpn_enable="YES"
openvpn_configfile="/usr/local/etc/openvpn/server.conf"
openvpn_if="tun"
```

In order to start manually the OpenVPN server, execute the command
below:

```bash
$ sudo /usr/local/etc/rc.d/openvpn start
```

## OpenVPN Client Configuration

Below is a sample OpenVPN client configuration file that you can use
for your OpenVPN clients.

Please refer to the openvpn(8) manual page for explanation of the
different options.

```text
client
remote-cert-tls server
dev tun
proto udp
remote openvpn.example.org 1194
nobind
user nobody
group nobody
persist-key
persist-tun
ca /usr/local/etc/openvpn/keys/ca.crt
cert /usr/local/etc/openvpn/keys/client.crt
key /usr/local/etc/openvpn/keys/client.key
cipher BF-CBC
comp-lzo
verb 3
mute 20
```

Please note that Linux users will need to change the **group** option
from **nobody** to **nogroup**
