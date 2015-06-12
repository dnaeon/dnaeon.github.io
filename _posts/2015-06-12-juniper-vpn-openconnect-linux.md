---
layout: post
title: Connecting to Juniper SSL VPN using OpenConnect on GNU/Linux
tags: juniper vpn linux
---

[Network Connect has been replaced by Junos Pulse](http://www.juniper.net/techpubs/en_US/junos-pulse4.0/topics/concepts/a-c-c-nc-interoperating.html)
for some time already, but GNU/Linux users were left in the dark,
since there is currently no official package provided by Juniper
for Junos Pulse on GNU/Linux.

Fortunately there is [OpenConnect](http://www.infradead.org/openconnect/index.html),
which makes it possible for GNU/Linux users to connect to a Juniper
SSL VPN endpoint.

Here are instructions for setting up OpenConnect to establish a
connection to a remote Juniper SSL VPN endpoint.

These instructions were tested on a [Arch Linux](https://www.archlinux.org/),
system running Linux kernel `4.0.5-1-ARCH`.

First, let's install the needed packages.

```bash
$ sudo pacman -S git openconnect python2-pip
```

Next we will install the
[Python Juniper VPN Authenticator](https://github.com/russdill/juniper-vpn-py),
which is a script that authenticates with a Juniper SSL VPN endpoint
and generates a session cookie, which in turn is passed to a VPN client.

We also need to install [mechanize](https://pypi.python.org/pypi/mechanize/),
which is a Python module required by the [Python Juniper VPN Authenticator](https://github.com/russdill/juniper-vpn-py).

```bash
$ sudo pip2 install mechanize
```

Now, clone the `Python Juniper VPN Authenticator` repository.

```bash
$ git clone https://github.com/russdill/juniper-vpn-py
```

You can now connect to a remote Juniper SSL VPN endpoint by running
the command below.

```bash
$ sudo python2 juniper-vpn-py/juniper-vpn.py --host <sslvpn.example.org> \
       --user <username> --stdin DSID=%DSID% openconnect --juniper %HOST% --cookie-on-stdin
```
