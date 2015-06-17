---
layout: post
title: OpenStack installation and configuration with CFEngine under Ubuntu
created: 1351589331
tags: cfengine automation openstack virtualization
---
In a previous post I've blogged about the
OpenStack Folsom installation under Ubuntu 12.04 Server system.

Now a week later after spending lots of time installing and
configuring different OpenStack nodes, I've decided that it's time to
automate the process and save my time while CFEngine is doing all the
installation and configuration for me.

The result was a CFEngine configuration which takes care of setting up
the OpenStack controllers and nodes for my internal Cloud
infrastructure, and I'm eager to share this with you! :)

You can find the CFEngine promises for OpenStack Folsom and Ubuntu
12.04 at my Github account in the link below:

* [https://github.com/dnaeon/openstack-cfengine-ubuntu](https://github.com/dnaeon/openstack-cfengine-ubuntu)

The CFEngine promises are based on the documentations below, which
I've mentioned also in my [previous post about OpenStack](/node/77):

* [https://github.com/mseknibilel/OpenStack-Folsom-Install-guide/blob/master/OpenStack_Folsom_Install_Guide_WebVersion.rst](https://github.com/mseknibilel/OpenStack-Folsom-Install-guide/blob/master/OpenStack_Folsom_Install_Guide_WebVersion.rst)
* [https://github.com/EmilienM/openstack-folsom-guide](https://github.com/EmilienM/openstack-folsom-guide)

Everything that needs to be configured is now performed by
CFEngine. The only things that still needs to be done manually is to
sync the Services databases during a first-time install, e.g. doing a
`keystone-manage db_sync` for Keystone and the other Services during
your first-time install and some other minor steps of course.

The CFEngine promises are taking care of setting up for you the
following components:

* KVM
* Live migration
* Cinder
* Glance
* Horizon
* Keystone
* Nova
* Quantum
* Open vSwitch
* GlusterFS
* RabbitMQ
* MySQL
* and some others..

Hope you find it useful and it saves your time, as it saved mine!
