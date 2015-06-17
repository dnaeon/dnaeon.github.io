---
layout: post
title: Resizing a KVM disk image on LVM, The Easy Way
created: 1354714045
tags: linux virtualization kvm
---
In a previous post we've seen how to resize a KVM disk image on LVM,
which was explained in the
[Resizing a KVM disk image on LVM, The Hard Way](/node/84) post.

In this post I'm going to show you how to do the same thing, but this
time in a bit safer, automated and less adventurous way.

If you are one of those adventurous souls who don't mind stepping into
deep waters, then I would recommend you checking out the [Resizing a
KVM disk image on LVM, The Hard Way](/node/84), which will take you to
a journey of resizing your KVM disk images. If you are just looking
for a quick and safe way for increasing your KVM disk images, then
this post is for you.

The difference between this post and the *Resizing a KVM disk image on
LVM, The Hard Way* is that here we are going to use `virt-resize(1)`
for performing the resize operations which saves us some typing on the
terminal and it's safer, while in the other post we are doing manually
all of the resize operation.

`virt-resize(1)` is safer in a way that it would expect you to have a
new LVM volume created first for the resize. That is all okay and
provides you with a nice rollback solution in case resizing fails, but
it also requires you to have extra free disk space in order to perform
the resize operation.

So, here's what I'd recommend - use this post for resizing your KVM
disk images easy and safely if you have plenty of free disk space for
the new LVM volume required by *virt-resize(1)*, and use the [Resizing
a KVM disk image on LVM, The Hard Way](/node/84) instructions if you
are limited at free disk space and thus cannot afford to create a new
LVM volume for the resize operation.

A bit of background information about our KVM hypervisor and the VM
guest domains - we run Debian Wheezy systems as the KVM hypervisors
and use LVM volumes as QEMU raw images for our guests.

Inside the VM guests we also use LVM volumes for our
filesystems. Commands prefixed with **host#** should be executed on
the KVM hypervisor and commands prefixed with **guest#** should be
executed on the VM guest domain.

Okay, now lets go straight to the point and see how to resize our VM
images using `virt-resize(1)`.

First, here's some info about the volume group *inside* the VM guest
domain:

```bash
guest# vgs
  VG   #PV #LV #SN Attr   VSize  VFree
  vg0    1   2   0 wz--n- 10.00g 1.62g
```

And about the LVM volumes as well:

```bash
guest# lvs
  LV   VG   Attr     LSize   Pool Origin Data%  Move Log Copy%  Convert
  root vg0  -wi-ao--   7.45g                                           
  swap vg0  -wi-ao-- 952.00m       
```

Now, login to the KVM hypervisor and shutdown the VM guest domain:

```bash
host# virsh shutdown guest-vm
```
	
Here's some information about disk image before the resize
operation. You can see from the output below the disk type and size as
well.

```bash
host# qemu-img info /dev/VG0/guest-vm
image: /dev/VG0/guest-vm
file format: raw
virtual size: 10G (10737418240 bytes)
disk size: 0
```

We are now going to rename the */dev/VG0/guest-vm* volume and create a
new one with the size we want to have it finally. The VM disk image is
10G in size and we are now going to resize it to 20G. Now, lets rename
the old volume and create the new one.

```bash
host# lvrename VG0 guest-vm guest-vm-OLD
  Renamed "guest-vm" to "guest-vm-OLD" in volume group "VG0"
```

Now create the new LVM volume with the size of your choice:

```bash
host# lvcreate -L 20G -n guest-vm VG0
  Logical volume "guest-vm" created
```

We are almost there. Now we are ready to start the resize
operation. To do that execute the command below:

```bash
host# virt-resize /dev/VG0/guest-vm-OLD /dev/VG0/guest-vm --expand /dev/vda1 --LV-expand /dev/vg0/root
```

The above command will resize the VM's disk using the new
*/dev/VG0/guest-vm* LVM volume and afterwards it will resize the
guest's disk image, which is */dev/vda1* and then resize the LVM
volume */dev/vg0/root* inside the guest domain.

Wait for the command to complete. Here's the output when we run the
above command:

```bash
host# virt-resize /dev/VG0/guest-vm-OLD /dev/VG0/guest-vm --expand /dev/vda1 --LV-expand /dev/vg0/root                                    
Examining /dev/VG0/guest-vm-old ...
 100% ⟦▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓⟧ --:--
**********

Summary of changes:

/dev/vda1: This partition will be resized from 10.0G to 20.0G.  The LVM 
	PV on /dev/vda1 will be expanded using the 'pvresize' method.

/dev/vg0/root: This logical volume will be expanded to maximum size.  
	The filesystem ext4 on /dev/vg0/root will be expanded using the 
	'resize2fs' method.
		
**********
Setting up initial partition table on /dev/VG0/guest-vm ...
Copying /dev/vda1 ...
 100% ⟦▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓⟧ 00:00
 100% ⟦▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓⟧ --:--
Expanding /dev/vda1 using the 'pvresize' method ...
Expanding /dev/vg0/root using the 'resize2fs' method ...

Resize operation completed with no errors.  Before deleting the old 
disk, carefully check that the resized disk boots and works correctly.
```

If everything looks OK, we can then start up our VM machine again and
verify that the disk has been resized:

```bash
host# virsh start guest-vm
```
	
Now we can verify that the volume group has been resized inside the
guest:

```bash
guest# vgs
  VG   #PV #LV #SN Attr   VSize  VFree
  vg0    1   2   0 wz--n- 20.00g    0 
```

And we can also see that the LVM volume has been resized successfully
as well:

```bash
guest# lvs
  LV   VG   Attr     LSize   Pool Origin Data%  Move Log Copy%  Convert
  root vg0  -wi-ao--  19.07g                                           
  swap vg0  -wi-ao-- 952.00m    
```
	  
You can see from the above outputs that the LVM group and volume were
successfully resized. If everything looks OK you can safely remove the
old LVM volume.
