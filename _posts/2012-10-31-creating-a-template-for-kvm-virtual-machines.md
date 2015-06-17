---
layout: post
title: Creating a template for KVM virtual machines
created: 1351711158
tags: linux virtualization kvm
---
These are just some notes I was keeping around for installing a KVM
machine that can be used as a template for creating new KVM instances.

I'm using KVM as a hypervisor and LVM volumes for the VM machines, and
the Operating System being used is Debian.

For more information on setting up KVM on Debian, please check the
[Debian KVM Wiki page](http://wiki.debian.org/KVM)

## Login to the KVM hypervisor

Download the system's ISO you will be using as a template and place it
in a word-readable directory.

I have used a Debian Squeeze ISO image for this purpose to create my
own Squeeze template system.

## Create a logical volume for the virtual machine

All virtual machines are installed inside an LVM volume, so first we
need to create a new volume for the machine, e.g.:

```bash
$ sudo lvcreate -L10G -n debian-squeeze vg0
```

## Create the new virtual machine

Now we can start the installation of the new template system.

The below command would create a new system called *debian-squeeze*
with *1024 Mb* of memory and *1 virtual cpu*. A VNC session will be
created as well for the new system, so that you can follow the install
process.

Below is an example command used while installing a Debian Squeeze
system as a template:

```bash
$ sudo virt-install --connect qemu:///system -n debian-squeeze \
       -r 1024 --vcpus=1 --disk path=/dev/mapper/vg0-debian-squeeze \
       -c /isos/debian-squeeze.iso --vnc --noautoconsole \
       --os-type linux --os-variant debiansqueeze \
       --network=bridge:br0 --hvm
```

## Complete the OS installation 

A VNC session will be created for you listening on
*localhost:port*. In order to get the port check the last created
*qemu* process which includes the port of the VNC session as well.

In order to connect to the VNC session, create an SSH tunnel to the
hypervisor and use a VNC client.

Example command would look like this if our new template system is
listening on port 5924 for incoming VNC sessions.

```bash
$ ssh -L 5924:localhost:5924 hv01.example.org
$ vncviewer localhot:5924
```

Complete the OS installation, install a basic set of required packages
and set *root* password.

## Minimum set of required packages

These below are the minimum set of required packages for all template
system:

* sudo
* cfengine3
* emacs

Of course, you can install your own package set well :)

## Post-install actions

Once the new template system has been installed and fully configured
we need to perform a few post-install actions.

That includes shutting down the new system and preparing it for
cloning.

First, shutdown the template system:

```bash
$ sudo virsh
virsh# shutdown debian-squeeze
```
	
Now, we need to prepare the system for cloning, which means removing
any persistent *udev* entries for network configurations and others as
well.

Below is an example command used for preparing the *debian-squeeze*
template machine.

```bash
$ sudo virt-sysprep --enable=cron-spool,dhcp-client-state,dhcp-server-state,logfiles,mail-spool,net-hwaddr,rhn-systemid,ssh-hostkeys,udev-persistent-net,utmp,yum-uuid -d debian-squeeze
```

One last thing that remains to be done is to mark the VM, so that it
does not start during boot-time.

```bash
$ sudo virsh
virsh# autostart --disable debian-squeeze
```

**NOTE: After every change you do on the template system you need to
  run the above command, so that the system is prepared for cloning.**

Once that is done you can start cloning the template machine and
create new systems.

See this blog post about [cloning a template system](/node/81) in
order to see how you can clone your template system and spawn new
instances in KVM.
