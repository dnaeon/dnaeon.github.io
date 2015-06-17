---
layout: post
title: How to detect VMware machines in CFEngine 2
created: 1348645758
tags: cfengine automation
---
CFEngine 2 does not provide a native class to determine a
VMware machine, thus if you want to put VMware machines under
CFEngine 2 control you have no native way of recognizing these
machines by CFEngine 2.

Here's one trick you could use in order to define a new class in
CFEngine 2 which detects if the client is running in a VMware machine.

```text
groups:

    vmware_machines = ( ReturnsZeroShell(/usr/sbin/dmidecode | /bin/grep ${quote}Product Name: VMware${quote}) )
```

If the client is running in a VMware machine the *vmware_machines*
class will be defined and you can use it just like any other class in
CFEngine 2, for example to install VMware Tools or anything else that
needs to be performed on these machines.
