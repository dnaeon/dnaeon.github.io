---
layout: post
title: Proxmox VE installation with Cfengine under Debian
created: 1351847302
tags: linux automation virtualization kvm
---
Following up the CFEngine series of OpenStack for [Debian](/node/94)
and [Ubuntu](/node/78) here's another one, but this time for
[Proxmox VE](http://www.proxmox.com) and Debian.

The Proxmox VE installation and configuration is much, much simpler
than the OpenStack configuration as it consists of just a few package
installations.

You can get the Cfengine promises at the Github repository listed
below:

* [https://github.com/dnaeon/proxmoxve-cfengine-debian](https://github.com/dnaeon/proxmoxve-cfengine-debian)

The Cfengine policy is based on the documentation found at the Proxmox
VE Wiki:

* [http://pve.proxmox.com/wiki/Install_Proxmox_VE_on_Debian_Squeeze](http://pve.proxmox.com/wiki/Install_Proxmox_VE_on_Debian_Squeeze)

Hope you find them useful and happy CFEngine'ing! :)
