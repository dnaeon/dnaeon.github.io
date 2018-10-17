---
layout: post
title: Static IP addresses in OpenVPN
created: 1354122328
tags: linux vpn
---
Sometimes when working with OpenVPN it is nice to have a way to
tell the OpenVPN server that you'd like to get the same IP address
each time you connect to it, or in other words you'd like to get a
static IP address instead a dynamic one from the IP pool.

Having a static IP address for your OpenVPN clients can be not just
handy, but in some situations it could be even required.

One example of such situation would be to allow a person access to a
database, which is restricting access by IP addresses.

What you would want to have is grant access to that person only, and
not everyone in your OpenVPN subnet.

For completeness of this post I'm going to use the [OpenVPN server's
configuration as described here](http://dnaeon.github.io/installing-openvpn-on-debian-gnu-linux/) as a reference.

The way that we assign static IP addresses for our clients in OpenVPN
is done via the `client-config-dir` option. The `client-config-dir`
option points to a directory with files which contain client specific
configurations, like IP addresses for example.

Check `openvpn(8)` man page for more information on the
`client-config-dir` option.

So, lets enable *client-config-dir* in our OpenVPN server's
configuration. Add these lines to your */etc/openvpn/server.conf*
file:

```text
# Client config directory
client-config-dir /etc/openvpn/ccd
```

Next we create the `client-config-dir` we've specified above:

```bash
$ sudo mkdir /etc/openvpn/ccd
```

It is important to note that after placing files in `/etc/openvpn/ccd`
directory they should be readable by the OpenVPN user after dropping
privileges, e.g. by user *nobody*.

Also OpenVPN will load a client configuration file *only* if it
matches the clients common name. Say that we've created a client
certificate and we've used a CN *test.user* then in */etc/openvpn/ccd*
you should place a file called *test.user*.

And here's how we can tell the OpenVPN server to always provide the
same IP address to our *test.user* by adding the below contents to
*/etc/openvpn/ccd/test.user* file:

```bash
ifconfig-push 10.1.32.10 10.1.32.1
```

The OpenVPN client will get the 10.1.32.10 address and 10.1.32.1 is
the address of the OpenVPN server from the 10.1.32.0/20 subnet.

Fix the permissions, so that OpenVPN can read the files:

```bash
$ sudo chown -R nobody:nogroup /etc/openvpn/ccd
```

One last thing that you need to do is to reserve this IP address, so
that you don't end up with duplicate client addresses in your OpenVPN
subnet.

This is done by adding an entry in the persistent IP pool file, which
is usually called `ipp.txt` and defined by the `ifconfig-pool-persist`
option.

In `ipp.txt` you should add a pair of CommonName and IP address
separated by a comma, for each client you want to have this address
reserved.

Here's how our `test.user` gets its IP address reserved:

```bash
test.user,10.1.32.10
```

And now restart the OpenVPN server:

```bash
$ sudo service openvpn restart
```

Test your configuration by connecting with your client to the OpenVPN
server and verify that everything works fine.
