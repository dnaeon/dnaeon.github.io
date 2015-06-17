---
layout: post
title: Installing and configuring OpenStack Folsom on Ubuntu server
created: 1350978507
tags: linux virtualization openstack
---
Had to install OpenStack recently for our internal cloud management
solution and just wanted to share with you my experience.

## Issues with OpenStack during the installation and configuration

For the installation and configuration I've used the official
documentation from the [OpenStack website](http://openstack.org/),
which can be found here:

* [http://docs.openstack.org/trunk/openstack-compute/install/apt/content/](http://docs.openstack.org/trunk/openstack-compute/install/apt/content/)

The documentation is about installing and configuring OpenStack Folsom
release on Ubuntu 12.04 server.

During the installation and configuration of OpenStack following the
above documentation, I have few things to mention and warn you about.

* The official documentation is quite comprehensive

First, it's a must read documentation if you want to understand
OpenStack. It contains a lot of explanation and examples, so it is
something that you should consider reading.

Second, it's not something that you want to start with if you are
looking for a quick and dirty way to get started with OpenStack. It is
quite easy to get lost in the documentation during your journey with
OpenStack.

* The official documentation contains lots of typos and errors

I often found myself wondering why a specific component like Nova for
example would just refuse to start up. At the end the issue was purely
in the documentation itself - wrong usernames/passwords to be used,
while you should be using other usernames/passwords instead.

The commands are still using the old flags, which is something to take
into account if you are just blindly copy-pasting commands. My advice,
always check the commands and the flags they accept and do the
appropriate changes to the examples given.

* The official documentation does not provide fully working examples

I often found myself looking at why a specific component does not
work, while the documentation says that's all I need to get it up and
running.

The issue was that the examples provided are not fully working
configurations, and I had to dig into other documentations and
resources found on the Net, so be prepared.

Such an example was during the Horizon installation and configuration,
where things just simply didn't want to work out. I knew the problem
was some configuration issue, but I had no clue as I didn't have any
experience with OpenStack before.

## The solution to all my problems

At some point I decided to start from scratch, but this time following
a nice, short and straight-forward documentation, which was exactly
what I needed.

You can find the documentation at the link below:

* [OpenStack Folsom Install Guide](https://github.com/mseknibilel/OpenStack-Folsom-Install-guide/blob/master/OpenStack_Folsom_Install_Guide_WebVersion.rst)

Few points on the above documentation:

* It's easy to read and understand
* Contains fully-working configurations
* Is pretty straight-forward
* Is regularly updated

Following the above documentation I managed to get OpenStack up and
running in less than an hour.

My advice to you if you go the OpenStack way is:

* Read
  [this documentation](https://github.com/mseknibilel/OpenStack-Folsom-Install-guide/blob/master/OpenStack_Folsom_Install_Guide_WebVersion.rst)
  to get OpenStack installed and configured quickly
* Go and read
  [the official documentation](http://docs.openstack.org/trunk/openstack-compute/install/apt/content/)
  for more in-depth understanding of OpenStack
