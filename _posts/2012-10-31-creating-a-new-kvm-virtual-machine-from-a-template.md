---
layout: post
title: Creating a new KVM virtual machine from a template
created: 1351712140
tags: linux virtualization kvm
---
If you've seen the blog post about
[creating a template for KVM virtual machines](/node/80) then you
probably were wondering how to actually clone an already existing
template system.

Well, this blog post is exactly about this - how you can clone an
existing KVM template system and spawn new instances.

## Login to the hypervisor

Create a logical volume first for the new virtual machine.

Below is an example command which creates a new LVM volume called
*www-proxy*, from the *vg0* volume group, which we'll use to install
the Varnish HTTP Accelerator.

```bash
$ sudo lvcreate -L10G -n www-proxy vg0
```

Next we clone the system from an existing template.

Below is an example command which creates the *www-proxy* virtual
machine using the *debian-squeeze* template system:

```bash
$ sudo virt-clone --original debian-squeeze --name www-proxy --file /dev/mapper/vg0-www-proxy
```

Wait for the clone to be completed and then proceed to the next steps.

## Post-clone actions

First, we need to start up the newly created machine and perform some
post-clone actions.

Login to the hypervisor and start up the new system.

```bash
$ sudo virsh
virsh# start www-proxy
```

You can watch the system booting by executing the `console www-proxy`
command.

Once the system boots up, login to it and do the post-clone steps
listed below.

* Update /etc/hosts
* Update network interfaces
* Set hostname
* Create ssh keys for the host

For creating the server's SSH keys execute the commands below:

```bash
$ sudo ssh-keygen -t rsa -f /etc/ssh/ssh_host_rsa_key
$ sudo ssh-keygen -t dsa -f /etc/ssh/ssh_host_dsa_key
$ sudo ssh-keygen -t ecdsa -f /etc/ssh/ssh_host_ecdsa_key
```

* Create the *atjobs* directory needed for running *atd(8)*:

This is needed because during the preparation of the template system
(via *virt-sysprep(8)*) the *atjobs* directory is being removed.

```bash
$ sudo mkdir /var/spool/cron/atjobs
```

* Create the */var/log/wtmp* file to store login information

To create the file, simply *touch(1)* it:

```bash
$ sudo touch /var/log/wtmp
```

One last thing that remains to be done is to autostart the system
during boot-time. To do that, execute the command below from the
*virsh(1)* console:

```bash
virsh# autostart www-proxy
```

Once ready, reboot the system and you are good to go.
