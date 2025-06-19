---
layout: post
title: Installing and configuring a PPTP server with mpd5 on FreeBSD
created: 1295108372
tags: freebsd vpn pptp mpd5
---
In this document we will see how to install and configure
`Mpd5` - a `netgraph(4)` based impelementation of the multi-link
PPP protocol for FreeBSD.

We will show you how to install and configure `net/mpd5` port as a
PPTP server, allowing remote VPN connections.
	
Please refer to the
[official web site of Mpd](http://mpd.sourceforge.net/)
for more information about Mpd.

This setup has been tested and works well on FreeBSD 8.1-RELEASE
system and Mpd version 5.5

## Requirements

* root access or sudo rights

## Installation

In this document we will be installing Mpd using the
[FreeBSD Ports Collection](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports-using.html).

So first make sure that your ports tree is updated and then proceed with the installation.

Now let's install `net/mpd5`:

```bash
$ cd /usr/ports/net/mpd5 && sudo make install clean
```

After executing the above command you will see the following screen,
allowing you to select different options.

[![]({{ site.baseurl }}/images/mpd5-config.jpg)]({{ site.baseurl }}/images/mpd5-config.jpg){:.glightbox}

fYou can safely leave the options as they are and continue with the
installation.

Once the installation is over, you should see something similar:

```text
===> Installing rc.d startup script(s)
===>   Registering installation for mpd-5.5
===> SECURITY REPORT: 
      This port has installed the following files which may act as network
      servers and may therefore pose a remote security risk to the system.
/usr/local/sbin/mpd5

      This port has installed the following startup scripts which may cause
      these network services to be started at boot time.
/usr/local/etc/rc.d/mpd5

      If there are vulnerabilities in these programs there may be a security
      risk to the system. FreeBSD makes no guarantee about the security of
      ports included in the Ports Collection. Please type &#39;make deinstall&#39;
      to deinstall the port if this is a concern.

      For more information, and contact details about the security
      status of this software, see the following webpage: 
http://www.sourceforge.net/projects/mpd
===>  Cleaning for libpdel-0.5.3_4
===>  Cleaning for expat-2.0.1_1
===>  Cleaning for mpd-5.5
```

Now that we have `net/mpd5` installed, let's configure it.

## Configuration

As mentioned in the begining of this document, we will only cover
in this document how to install and configure Mpd as PPTP server.

Please also have a look at the
[online documentation of Mpd](http://mpd.sourceforge.net/doc5/mpd.html)
for more information on the different options of the `mpd5(8)` daemon.

Mpd5 keeps it's configuration in `/usr/local/etc/mpd5` directory.
The main configuration file of Mpd5 is `mpd.conf`

Here is an example configuration `mpd.conf` file:

```text
startup:
        # configure mpd users
        set user <pptpadmin> admin
        set user <pptpadmin> <pptpadminpass>
        # configure the console
        set console self 127.0.0.1 5005
        set console open
        # configure the web server
        set web self 0.0.0.0 5006
        set web open
default:
        load pptp_server

pptp_server:
        set ippool add pool1 <ip-range1> <ip-range2>

# Create clonable bundle template named B
        create bundle template B
        set iface enable proxy-arp
        set iface idle 1800
        set iface enable tcpmssfix
        set ipcp yes vjcomp
# Specify IP address pool for dynamic assigment.
        set ipcp ranges <mpd-ip-address>/<mask> ippool pool1
        set ipcp dns <dns-server>

# The five lines below enable Microsoft Point-to-Point encryption
# (MPPE) using the ng_mppc(8) netgraph node type.
        set bundle enable compression
        set ccp yes mppc
        set mppc yes e40
        set mppc yes e128
        set mppc yes stateless

# Create clonable link template named L
        create link template L pptp
# Set bundle template to use
        set link action bundle B
# Multilink adds some overhead, but gives full 1500 MTU.
        set link enable multilink
        set link yes acfcomp protocomp
        set link no pap chap eap
        set link enable chap
# Enable utmp/wtmp logging
        set auth enable system-acct
# We are reducing link mtu to avoid GRE packet fragmentation.
        set link mtu 1460
# Configure PPTP
        set pptp self <mpd-ip-address>
# Allow to accept calls
        set link enable incoming
```

In the above example configuration, you need to specify the
following things:

* `set user <pptpadmin> admin` - The administrator username for connecting to our `mpd5(8)` daemon.
* `set user <pptpadmin> <pptpadminpass>` - The password for the administrator account specified in the above option
* `set ippool add pool1 <ip-range1> <ip-range2>` - This is the IP Pool reserved for PPTP connections.
* `set ipcp ranges <mpd-ip-address>/<mask> ippool pool1` - Here we need to specify the IP address that our `mpd5(8)` daemon is listening on,
  along with the subnet mask for our network. Example setting might look like this: `set ipcp ranges 10.1.1.1/20 ippool pool1`
* `set ipcp dns <dns-server>` - In this setting we need to specify the DNS server IP address.
* `set pptp self <mpd-ip-address>` - Again this is the IP address that our `mpd5(8)` daemon is listening on

If you have a firewall in front of your PPTP server, also make sure
that your firewall is allowing traffic for the addresses in the
assigned IP Pool.

Now we can add user accounts that will be authorized to connect to
our PPTP server.

User accounts are stored in the `/usr/local/etc/mpd5/mpd.secret`
file. The file contains username/password pairs that are used to
authorize a user, separated by a whitespace.

An example `mpd.secret` that contains two usernames with their
passwords might look like this:

```text
foo   passwordfoo
bar   passwordbar
```

If you want to limit connections from user `foo` only from a
single IP address, you might change the configuration to this:

```text
foo   passwordfoo 192.168.1.100
bar   passwordbar
```

Let's secure the `mpd5` configuration, so that only root can read it:

```bash
$ sudo chown -R root:wheel /usr/local/etc/mpd5
$ sudo chmod -R 0600 /usr/local/etc/mpd5
```

## Enabling logging through syslog-ng3

If you are using `sysutils/syslog-ng3` you might also want to
enable logging for the `mpd5(8)` daemon.

All you have to do is to add the following lines to their
corresponding sections in `/usr/local/etc/syslog-ng.conf` file:

```text
# 
# destinations
#
destination mpd5 { file("/var/log/mpd5.log"); };

#
# program filters
#
filter f_mpd5 { program("mpd5"); };

#
# !mpd5
# *.*
#
log { source(src); filter(f_mpd5); destination(mpd5); };
```

Then create the `/var/log/mpd5.log` file and reload `syslog-ng(8)`
configuration.

```bash
$ sudo touch /var/log/mpd5.log
$ sudo /usr/local/etc/rc.d/syslog-ng reload
```

## Allowing Mpd traffic through your firewall

If you are running `mpd5(8)` in a firewalled environment, you will
need to pass the traffic through your firewall, so that clients can
connect to your PPTP server.

Below you will find a very basic `pf.conf` file, that contains sample
[Packet Filter (PF)](http://openbsd.org/faq/pf/index.html) rules,
that will allow your clients to communicate with your PPTP server:

```text
# --- MACROS section ---
ext_if = "<external-interface>"
int_if = "<internal-interface>"

# --- IP given by the ISP ---
ip_addr = "x.x.x.x"

# --- pptp server ---
PPTP_SERVER = "x.x.x.x"

# --- pptp services ---
PPTP_SERVICES = "{ 1723 47 }"

# --- hosts with internet access ---
table <allowed> { x.x.x.x/<mask> }

# --- OPTIONS section ---
set skip on lo0
set block-policy return

# --- SCRUB section ---
scrub in all

# --- TRANSLATION (NAT/RDR) section ---
nat on $ext_if from <allowed> to any -> $ip_addr

# --- redirect pptp traffic to the internal pptp server ---
rdr on $ext_if proto { tcp udp } from any to $ext_if port $PPTP_SERVICES -> $PPTP_SERVER

# --- FILTER RULES ---

# --- default policy ---
block log all

# --- antispoof protection ---
antispoof quick for $ext_if inet
antispoof quick for $int_if inet

# --- INTERNAL interface ---

pass in quick on $int_if inet from <allowed> to any keep state
pass out quick on $int_if inet from any to any keep state

# --- EXTERNAL interface ---

# --- pass incoming pptp connections to the internal pptp server ---
pass in quick on $ext_if inet proto { tcp udp } from any to $PPTP_SERVER port $PPTP_SERVICES keep state

pass out quick on $ext_if inet from any to any keep state
```

In the above example our IP Pool assigned to `mpd5(8)` is part of our
`allowed` table, so that we do NAT and also allow the traffic in and out.

## Starting Mpd5

Now we can start our *mpd5(8)* daemon, so that our clients can connect to it.

```bash
$ sudo /usr/local/etc/rc.d/mpd5 start
```

In case of errors, you should check the `/var/log/mpd5.log` log file.

In order to start the `mpd5(8)` daemon during boot-time, add the
following lines to your `/etc/rc.conf` file:

```bash
# Enable mpd5 daemon
mpd_enable="YES"
mpd_flags="-b -s mpd5"
```

And that's it, you should now have an installed and
configured PPTP server.

## Connecting to your mpd5(8) daemon

In order to connect to your `mpd5(8)` daemon you can use the
command-line or a browser.

To control your `mpd5(8)` daemon using command-line use the
following command (where of course, `mpd-server.example.org` is your
mpd5 server address/hostname):

```bash
$ telnet mpd-server.example.org 5005
Trying x.x.x.x...
Connected to localhost.
Escape character is '^]'
Multi-link PPP daemon for FreeBSD

Username: pptpadmin
Password: 

Welcome!
Mpd pid 881, version 5.5 (root@hostname 18:23 23-Jul-2010)
[]
```

In order to view the already established PPTP connections through
your browser, start up a browser to the *mpd5(8)* server on port 5006, like this:

* http://mpd-server.example.org:5006
