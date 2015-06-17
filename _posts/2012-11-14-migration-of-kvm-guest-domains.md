---
layout: post
title: Migration of KVM guest domains
created: 1352894638
tags: linux virtualization kvm
---
In this short blog I'll show you how to migrate a KVM guest machine
from host to another by using *virsh(1)*.

First, install the *libvirt-bin* package, which provides *virsh(1)*:

```bash
$ sudo apt-get install libvirt-bin
```
	
A short summary of our hypervisors and guest domain that we use in
this post:

* kvm01.example.org - is the first KVM hypervisor
* kvm02.example.org - is the second KVM hypervisor
* guest01 - is the guest domain that we will be migrating

Our guest domain *guest01* resides on *kvm01.example.org* and will be
migrated to the *kvm02.example.org* hypervisor.

Now, there are two possible scenarios for a migration. First one is
that you are running your VMs on a shared storage (NFS, GlusterFS,
etc.) and in which case you are able to perform a live migration of
your guest domains.

The second one is that you are not running your VMs on a shared
storage and cannot perform a live migration.

Here we'll see how to perform a migration of a guest domain with a
shared storage and also non-shared storage.

## Performing a live migration with a shared storage

Remember, live migration requires a shared storage? If that's your
cause then you are a lucky one.

So let's do the live migration. Login to *kvm01.example.org* and
execute the command below, which would migrate *guest01* domain to
*kvm02.example.org*:

List the guest domains at *kvm01.example.org*:

```bash
root@kvm01:~# virsh list
Id    Name                           State
----------------------------------------------------
2     guest01                        running
```

And now perform the migration:

```bash
# virsh migrate --live --persistent guest01 qemu+ssh://kvm02.example.org/system 
```

Wait until the migration is over and after that verify that the guest
domain has been migrated to *kvm02.example.org*:

```bash
root@kvm02:~# virsh list
Id    Name                           State
----------------------------------------------------
2     guest01                        running
```
	
## Performing a migration with a non-shared storage

In cases where you do not have a shared storage (like NFS, GlusterFS,
etc.) you can still migrate a guest domain from one hypervisor to
another. In such cases the migration involves also transferring the
guest domain images.

One thing to keep in mind when performing a migration on a non-shared
storage is that *libvirt* would expect that the guest images be
created on the destination host already.

So, the first thing you need to do before starting out with the
migration is to create the disk images for the guest on the
destination host. So, create the disk images either by using
`qemu-img(1)` or the LVM volumes if you are working with LVMs
upfront. Once you have that in place execute the command below in
order to start with the migration.

```bash
$ sudo virsh migrate --copy-storage-all --persistent guest01 qemu+ssh://kvm02.example.org/system
```
	
Wait until the migration is over and after that verify that the guest
domain has been migrated to *kvm02.example.org*:

```bash
root@kvm02:~# virsh list
Id    Name                           State
----------------------------------------------------
2     guest01                        running
```

And that should be all!
