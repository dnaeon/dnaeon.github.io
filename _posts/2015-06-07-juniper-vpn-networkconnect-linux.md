---
layout: post
title: Using Juniper Network Connect on GNU/Linux
tags: juniper vpn linux network connect
---

Connecting to a Juniper VPN from a GNU/Linux system using Network
Connect could be a bit tricky, so here are instructions on how to
properly configure your GNU/Linux system for establishing connection
to a Juniper VPN using Network Connect.

There is a 
[Juniper KB25230](http://kb.juniper.net/InfoCenter/index?page=content&id=KB25230)
article, which describes how to install the Network Connect client on
GNU/Linux systems, but every time I try installing the client following
that KB entry I find myself doing some extra work in order to get
everything installed and running correctly.

For that reason, I thought of having this documented somewhere
would really help me out when I need to install Network Connect client
again in the near future.

The following instructions were tested on a Debian GNU/Linux 7.8 amd64
system.

First, download and unpack the 32bit version of Oracle Java runtime for
GNU/Linux. The version that I've used at the time of writing this is
`jre-8u45-linux-i586.tar.gz`. Considering that you've downloaded the
archive, we will now unpack and install it.

```bash
$ tar zxvf jre-8u45-linux-i586.tar.gz
$ sudo mv jre1.8.0_45 /usr/lib/jvm/
$ sudo update-alternatives --install /usr/bin/java java /usr/lib/jvm/jre1.8.0_45/bin/java 10
```

Juniper Network Connect requires that the 32bit libraries are
installed, so let's do that first.

```bash
$ sudo dpkg --add-architecture i386
$ sudo apt-get update
$ sudo apt-get install ia32-libs
```

Now, let's install the Java browser plugin. Using OpenJDK 7
doesn't work (at least for me), that is why we use OpenJDK version 6
here instead.

```bash
$ sudo apt-get update
$ sudo apt-get install openjdk6-jre icedtea6-plugin xterm
```

Note, that you will also need `xterm` installed as the Network Connect
client requires `xterm` as well.

One final thing that you need to take care of is granting your user
`sudo(8)` rights. We will add our user to the `sudo` group, which we
already have on our Debian GNU/Linux system.

```bash
$ sudo gpasswd --add sudo <username>
```

Next thing you do is point your browser to the Juniper VPN URL and
connect using credentials.
