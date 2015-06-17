---
layout: post
title: Resizing a KVM disk image on LVM, The Hard Way
created: 1353500426
tags: linux virtualization kvm
---
In this post I'm going to show you how to resize your KVM
Virtual Machine guest disk images.

And here's some background information about our setup - we run Debian
Wheezy systems as the KVM hypervisors and use LVM volumes as QEMU raw
images for our guests. Inside the VM guests we also use LVM volumes
for our filesystems.

You can also have a look at the [Resizing a KVM disk image on LVM, The
Easy Way](/node/85) post for a different and easy way of resizing KVM
disk images. The difference between this post and the other one when
it comes to resizing KVM disk images is explained in the [Resizing a
KVM disk image on LVM, The Easy Way](/node/85) post, so you might want
to check that one also.

While this guide shows you how to resize your VM guests using LVM
volumes you can still use this guide as a reference for resizing your
VM's disk images if you are using plain partitions inside your VM as
well. If that is your case you should simply skip the sections
involving the resize of LVM physical/logical volumes inside the VM
guest domains.

**DISCLAIMER**: Make sure that you have proper backups in place before
  starting out with the resize procedure on your VMs! Create any
  backups necessary to ensure that if something goes wrong you can
  always go back to a previous working state. Losing any information
  or wiping out your disks is all your fault if this happens. Using
  this procedure is all at your own responsibility.

In order to make things clear we will use a **guest#** prompt whenever
we execute commands inside the VM guest and **host#** prompt whenever
we execute commands on the KVM hypervisor.

So, now that we have things cleared, lets start with resizing our KVM
Virtual Machine disk images. First lets see how the disk image looks
like inside the VM guest machine.

```bash
guest# fdisk -l /dev/vda

Disk /dev/vda: 10.7 GB, 10737418240 bytes
255 heads, 63 sectors/track, 1305 cylinders, total 20971520 sectors
Units = sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disk identifier: 0x0008d801

   Device Boot      Start         End      Blocks   Id  System
/dev/vda1   *        2048    20969471    10483712   8e  Linux LVM
```

As you can see from the above output we have a disk of 10Gb in size
and one partition which we use for LVM. Now lets see our physical LVM
volume and some info about the volume groups we have before the
resize.

```bash
guest# pvdisplay 
  --- Physical volume ---
  PV Name               /dev/vda1
  VG Name               vg0
  PV Size               10.00 GiB / not usable 2.00 MiB
  Allocatable           yes 
  PE Size               4.00 MiB
  Total PE              2559
  Free PE               414
  Allocated PE          2145
  PV UUID               nG36C3-AHSr-9ax7-hJD7-UoNg-GGWH-JeVeLO
```

And the volume groups we have inside the guest machine:

```bash
guest# vgdisplay 
  --- Volume group ---
  VG Name               vg0
  System ID             
  Format                lvm2
  Metadata Areas        1
  Metadata Sequence No  3
  VG Access             read/write
  VG Status             resizable
  MAX LV                0
  Cur LV                2
  Open LV               2
  Max PV                0
  Cur PV                1
  Act PV                1
  VG Size               10.00 GiB
  PE Size               4.00 MiB
  Total PE              2559
  Alloc PE / Size       2145 / 8.38 GiB
  Free  PE / Size       414 / 1.62 GiB
  VG UUID               OPqZvj-RmWG-c1z1-0rEB-mHq4-GkWc-rw2Og1
```

From the hosts side we can check the LVM volume we use as a QEMU raw
disk image for the guest before the resize. In the command below
*/dev/VG0/guest01* is the LVM volume we use as QEMU raw image for our
guest machine.

```bash
host# qemu-img info /dev/VG0/guest01 
image: /dev/VG0/guest01
file format: raw
virtual size: 10G (10737418240 bytes)
disk size: 0
```

Ok, we've got enough information already, so lets start with the
resizing process now. First we need to shutdown the VM guest domain.

```bash
host# virsh shutdown guest01
```

Now we need to resize the LVM volume we are using as QEMU raw image
for the VM guest machine.

```bash
host# lvresize -L+10G /dev/VG0/guest01 
  Extending logical volume guest01 to 20.00 GiB
  Logical volume guest01 successfully resized
```

Lets check again the info of our QEMU raw image and verify that we
have successfully resized it:

```bash
host# qemu-img info /dev/VG0/guest01 
image: /dev/VG0/guest01
file format: raw
virtual size: 20G (21474836480 bytes)
disk size: 0
```

Okay, the VM's guest disk image has been resized, now we can start up
our guest machine and resize the LVM physical/logical volumes inside
the VM as well.

```bash
host# virsh start guest01
```

Now, if we check our disk inside the VM guest we can see that the disk
is larger.

```bash
guest01# fdisk -l /dev/vda

Disk /dev/vda: 21.5 GB, 21474836480 bytes
255 heads, 63 sectors/track, 2610 cylinders, total 41943040 sectors
Units = sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disk identifier: 0x0008d801

   Device Boot      Start         End      Blocks   Id  System
/dev/vda1   *        2048    20969471    10483712   8e  Linux LVM
```

As you can see from the above output the **Start** and **End** sectors
are still the same, which means that our partition needs first to be
increased, before we extend the LVM volumes in our guest machine. So,
what we need to do here is to re-create our partition, but this time
making it bigger.

Lets run `fdisk(8)` and create our LVM partition bigger:

```bash
# fdisk /dev/vda
```
	
First delete the current partition:

```bash
Command (m for help): d
Selected partition 1
```

Verify that the partition has been removed:

```bash
Command (m for help): p

Disk /dev/vda: 21.5 GB, 21474836480 bytes
255 heads, 63 sectors/track, 2610 cylinders, total 41943040 sectors
Units = sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disk identifier: 0x0008d801

   Device Boot      Start         End      Blocks   Id  System
```

Now re-create the partition, this time making it bigger. Note that the
**Start** sector should be the same as before the resize, and the only
change is in the **Last** sector.

```bash
Command (m for help): n
Partition type:
   p   primary (0 primary, 0 extended, 4 free)
   e   extended
Select (default p): p
Partition number (1-4, default 1): 1
First sector (2048-41943039, default 2048): 2048
Last sector, +sectors or +size{K,M,G} (2048-41943039, default 41943039): 
Using default value 41943039
```

Verify that the partition is created:

```bash
Command (m for help): p

Disk /dev/vda: 21.5 GB, 21474836480 bytes
255 heads, 63 sectors/track, 2610 cylinders, total 41943040 sectors
Units = sectors of 1 * 512 = 512 bytes
Sector size (logical/physical): 512 bytes / 512 bytes
I/O size (minimum/optimal): 512 bytes / 512 bytes
Disk identifier: 0x0008d801

   Device Boot      Start         End      Blocks   Id  System
/dev/vda1            2048    41943039    20970496   83  Linux
```

Write the partitions table:

```bash
Command (m for help): w
The partition table has been altered!

Calling ioctl() to re-read partition table.

WARNING: Re-reading the partition table failed with error 16: Device or resource busy.
The kernel still uses the old table. The new table will be used at
the next reboot or after you run partprobe(8) or kpartx(8)
Syncing disks.
```

The disk partition has been made bigger, but we do need to run an
`fsck(8)` before resizing the file systems. Reboot the system and
schedule a file system check on reboot by executing the command below:

```bash
guest01:~# shutdown -Fr now
```

Once the VM guest machine is up check the `fsck(8)` passed normally
and then proceed. Lets resize now the physical LVM volume in our
guest.

```bash
guest01# pvresize /dev/vda1
  Physical volume "/dev/vda1" changed
  1 physical volume(s) resized / 0 physical volume(s) not resized
```

And now our physical volume should be resized. Verify that the
physical volume has been resized:

```bash
guest01# pvdisplay 
  --- Physical volume ---
  PV Name               /dev/vda1
  VG Name               vg0
  PV Size               20.00 GiB / not usable 2.00 MiB
  Allocatable           yes 
  PE Size               4.00 MiB
  Total PE              5119
  Free PE               2974
  Allocated PE          2145
  PV UUID               nG36C3-AHSr-9ax7-hJD7-UoNg-GGWH-JeVeLO
```
	
Physical volume has been resized, which means that our volume group
should be bigger now as well. To verify that, execute the command
below:
	
```bash
guest01# vgdisplay 
  --- Volume group ---
  VG Name               vg0
  System ID             
  Format                lvm2
  Metadata Areas        1
  Metadata Sequence No  4
  VG Access             read/write
  VG Status             resizable
  MAX LV                0
  Cur LV                2
  Open LV               2
  Max PV                0
  Cur PV                1
  Act PV                1
  VG Size               20.00 GiB
  PE Size               4.00 MiB
  Total PE              5119
  Alloc PE / Size       2145 / 8.38 GiB
  Free  PE / Size       2974 / 11.62 GiB
  VG UUID               OPqZvj-RmWG-c1z1-0rEB-mHq4-GkWc-rw2Og1
```
	 
Physical volume and volume group have been successfully resized, so
now we can resize our logical volumes and file systems as well. This
is how our *root* partition looks like before the resize inside the
guest.

```bash
guest01# df -h /
Filesystem            Size  Used Avail Use% Mounted on
/dev/mapper/vg0-root  7.4G  989M  6.0G  14% /
```

First we resize the LVM volume:

```bash
guest01# lvresize -L+10G /dev/vg0/root 
  Extending logical volume root to 17.45 GiB
  Logical volume root successfully resized
```

And now lets perform an online resize of the file system:

```bash
guest01# resize2fs -p /dev/vg0/root 
resize2fs 1.42.5 (29-Jul-2012)
Filesystem at /dev/vg0/root is mounted on /; on-line resizing required
old_desc_blocks = 1, new_desc_blocks = 2
Performing an on-line resize of /dev/vg0/root to 4574208 (4k) blocks.
The filesystem on /dev/vg0/root is now 4574208 blocks long.
```

And this is how our *root* file system looks like after the resize
operations:

```bash
guest01# df -h /
Filesystem            Size  Used Avail Use% Mounted on
/dev/mapper/vg0-root   18G  993M   16G   6% /
```

And that's all!
