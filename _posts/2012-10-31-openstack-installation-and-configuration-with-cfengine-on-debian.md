---
layout: post
title: OpenStack installation and configuration with CFEngine on Debian
created: 1351702752
tags: cfengine automation virtualization
---
As promised in [previous post](/node/78) it is time to post the
Cfengine configuration of OpenStack for Debian Wheezy!

You can find the Cfengine promises for OpenStack Essex and Debian
Wheezy [my Github account](https://github.com/dnaeon/) in the link
below:

* [https://github.com/dnaeon/openstack-cfengine-debian](https://github.com/dnaeon/openstack-cfengine-debian)

The Cfengine promises are based on the following documentation for
installing OpenStack under Debian:

* [http://wiki.debian.org/OpenStackHowto](http://wiki.debian.org/OpenStackHowto)

Similar to the [Ubuntu Cfengine promises](/node/78) you need to
manually populate the Services databases of Keystone, Glance, etc.

The Cfengine promises for Debian are taking care of setting up for you
the following components:

* KVM
* Live migration
* Glance
* Horizon
* Keystone
* Nova
* GlusterFS
* RabbitMQ
* MySQL
* and some others..

Hope you find them useful! :)
