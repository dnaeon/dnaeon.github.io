---
layout: post
title: RAID-1 with GPT on FreeBSD
tags: freebsd
---
In this handbook we will see how you can create a software RAID-1 with
GPT on a FreeBSD host.

This setup has been tested and is working properly under FreeBSD 8.0.

In the following handbook we will see how to setup a freshly installed
FreeBSD 8.0 system with RAID-1 support and
[GPT](http://en.wikipedia.org/wiki/GUID_Partition_Table).

You can also use this guide in order to migrate your MBR to GPT.

One more thing to add about this documentation is that we are going to
create mirror of the partitions instead of the whole disks.

**NOTE:** In this documentation we will see how to create a RAID-1 on
  a FreeBSD host. During the implementation of the RAID-1 we will be
  creating mirror for the partitions, instead of the whole disks.

Some of you may be wondering what is the benefit of this approach, so
here is a short list of pros and cons when using this approach.

Pros for creating mirror of the partitions, instead of the whole
disks:

* Able to mirror different in size disks
* Can still create mirror for identical disks
* Able to dual-boot between different OS

Cons for creating mirror of the partitions, instead of the whole
disks:

* In case of a disk failure you need to manually create all the
  partitions on the newly inserted disk(s)

**NOTE:** Starting from FreeBSD 8.2 gpart(8) now supports two
  additional options - **backup** and **restore**, which makes it very
  easy to recover your partitions when needed.

So to summarize this again - in this handbook we will show you a
different approach of creating a RAID-1 on a FreeBSD host for
partitions only and also how you can migrate from MBR to GPT.

## Requirements

* root access
* 2 or more physical disks in order to create a RAID-1

## Preparing the system for GPT

There are a lot of different approaches you can use in order to get
FreeBSD booting from a GPT partition table. Currently *sysinstall*
does not have support to create GPT scheme on your disks, so we will
have to do this manually.

With the above being said, lets go into the real stuff..

## Make a fresh FreeBSD install

The first method demonstrates how we can make a fresh FreeBSD install
with RAID-1 and GPT.

**NOTE:** During the installation we will refer to the first disk as
  ad4 and the second disk as ad6.

Install FreeBSD 8.0 or later. Make partitions only on the first disk.

When you reach the *Disklabel Editor* menu, create the following
partitions on the first slice of the first disk. We won't be touching
the second disk at all during the installation of the system.

Create the following partitions - **root**, **swap**, **usr**, **tmp**
and **var**. Each of these partitions is of the following sizes:
**root** - 1GB, **swap** - 1GB, **tmp** - 1G, **usr** - 4GB and
**var** - 1G.

**NOTE:** The reason that I've chosen these sizes for the partitions
  is that these partitions are actually going to be temporary and used
  only during the installation. Later in this document we will be
  re-creating the disk partition scheme.

So after I've created my partitions on the first disk, the layout is
as follows:

```text
Part      Mount     Size    Newfs
-----     -----     ----    -----
ad4s1a    /       1024MB    UFS2
ad4s1b    swap    1024MB    SWAP
ad4s1d    /var    1024MB    UFS2+S
ad4s1e    /tmp    1024MB    UFS2+S
ad4s1f    /usr    4096MB    UFS2+S
```

Perform a **Minimal** install and go throught the post-install
configuration steps.

## Creating backups

Now that you have installed FreeBSD, boot into your freshly installed
system and create backups of the partitions you have. We will restore
their contents later when we create the
[GPT](http://en.wikipedia.org/wiki/GUID_Partition_Table) disk scheme.

You may want to use an external USB disk to store the backups or copy
the backups on some other machine once you are done with them. In the
following example I'm using an external USB disk to store the backups
- **da0**.

So let's go ahead and create those backups.

Create a filesystem on the USB disk, if you haven't done this already.

```bash
newfs -O2 -U /dev/da0
```

Now mount the USB disk:

```bash
# mkdir -p /mnt/usb
# mount /dev/da0 /mnt/usb
```

Now make a backup of each partition you have - **root**, **var**,
**tmp**, **usr**.

```bash
# dump -0aL -f /mnt/usb/root.dump /
# dump -0aL -f /mnt/usb/tmp.dump /tmp
# dump -0aL -f /mnt/usb/var.dump /var
# dump -0aL -f /mnt/usb/usr.dump /usr
```

If you do not have a USB disk, you may want to copy over the backups
to another machine using [OpenSSH](http://openssh.org). To do this
just create the backups this way:

```bash
# dump -0aL -f - / | ssh user@host dd of=/path/to/dumps/root.dump
# dump -0aL -f - /tmp | ssh user@host dd of=/path/to/dumps/tmp.dump
# dump -0aL -f - /var | ssh user@host dd of=/path/to/dumps/var.dump
# dump -0aL -f - /usr | ssh user@host dd of=/path/to/dumps/usr.dump
```

Once ready with the backups, unmount the USB disk if you have used
one:

```bash
# umount /mnt/usb
```

Now we can continue to the next step - creating the GPT disk scheme.

## Booting from a Fixit media

For this step we are going to boot into **Fixit** mode - for the
purpose, you can use the **livefs** image that can be found on the
[FreeBSD FTP Server](ftp://ftp.freebsd.org/).

Once you've booted from a **Fixit** media, go to **Fixit ->
CDROM/DVD** or** Fixit -> USB** whether you are booting from CD or
USB.

Now you can see the partitions we have created during the installation
of the system if you execute the following command:

```bash
Fixit# gpart show ad4
=>      63  488397101    ad4  MBR  (233G)
        63  488397038      1  freebsd  [active]  (233G)

=>       0  488397038  ad4s1  BSD  (233G)
         0    2097152      1  freebsd-ufs  (1.0G)
   2097152    2097152      2  freebsd-swap (1.0G)
   4194304    2097152      4  freebsd-ufs  (1.0G)
   6291456    2097152      5  freebsd-ufs  (1.0G)
   8388608    8388608      6  freebsd-ufs  (4.0G)
  16777216  471619822         - free -     (225G)
```

Now we are going to delete these partitions, and create GPT ones. So
execute the following *gpart(8)* commands

```bash
Fixit# gpart delete -i 1 ad4
ad4s1 deleted
Fixit# gpart show ad4
=>      63  488397101    ad4  MBR  (233G)
        63  488397101         - free - (233G)
Fixit# gpart destroy ad4
ad4 destroyed
```

You can now check that we have successfully removed the partitions
that were created during the installation of our FreeBSD system. Just
execute the following gpart command, so you are sure that we have
removed those partitions.

```bash
Fixit# gpart show
```

## Creating GPT partition tables

Now we can create the GPT tables on our disk. In this step we will
create our partitions with the sizes we want.

You may want to choose even bigger sizes for the partitions since once
we create them, we cannot resize them, so choosing bigger partition
sizes is something you should consider doing.

In the commands below I'm creating the following partitions with these
sizes: **root** - 2G, **swap** - 2G, **var** - 20G, **tmp** - 2G,
**usr** - 20G and for **home** - the rest of the disk. So let's create
these partitions.

Create the GPT table on the disk:

```bash
Fixit# gpart create -s gpt ad4
ad4 created
```

And now create the GPT partitions, including the boot one:

```bash
Fixit# gpart add -b 34 -s 128 -t freebsd-boot ad4   # GPT boot partition
Fixit# gpart add -s 2G -t freebsd-ufs ad4           # root partition
Fixit# gpart add -s 2G -t freebsd-swap ad4          # swap partition
Fixit# gpart add -s 20G -t freebsd-ufs ad4          # var partition
Fixit# gpart add -s 2G -t freebsd-ufs ad4           # tmp partition
Fixit# gpart add -s 20G -t freebsd-ufs ad4          # usr partition
Fixit# gpart add -t freebsd-ufs ad4                 # home partition
```

We also need to install the bootcode, so FreeBSD is able to boot from
our GPT partition:

```bash
Fixit# gpart bootcode -b /mnt2/boot/pmbr -p /mnt2/boot/gptboot -i 1 ad4
```

Now verify the partitions are created using the **gpart** command:

```bash
Fixit# gpart show ad4
=>       34  488397101  ad4  GPT  (233G)
         34        128    1  freebsd-boot (64K)
        162    4194304    2  freebsd-ufs  (2.0G)
    4194466    4194304    3  freebsd-swap (2.0G)
    8388770   41943040    4  freebsd-ufs  (20G)
   50331810    4194304    5  freebsd-ufs  (2.0G)
   54526114   41943040    6  freebsd-ufs  (20G)
   96469154  391927981    7  freebsd-ufs  (187G)
```

Now we can create filesystems on the partitions - no soft-updates for
the root partition:

```bash
Fixit# newfs -O2 ad4p2       # root partition
Fixit# newfs -U -O2 ad4p4    # var partition
Fixit# newfs -U -O2 ad4p5    # tmp partition
Fixit# newfs -U -O2 ad4p6    # usr partition
Fixit# newfs -U -O2 ad4p7    # home partition
```

If you are running FreeBSD 8.2 or later you may consider using the
**backup** option to gpart(8), which will dump the partition table of
the given provider. This way later on you can restore the partition
tables on another provider easily just by using the **restore** option
of gpart(8).

Now that we have created the GPT scheme and partitions we can restore
the data of our system on them.

## Restoring data from backups

Now we can restore the data we previously made a backup of.

Mount the USB disk, or manually configure the network device if you
have copied the backups on a remote system in order to get them back.

You might want to restore the **tmp** partition first and restore it's
data, and then set the *TMPDIR* environment variable first, so we can
take advantage of it's space during the restore.

In the commands below */mnt/usb* is the location we mount the USB disk
on, */mnt/tmp-partition* is the **tmp** partition's mount point -
**ad4p5**, and */mnt/partition* we will use as a mount point for the
rest of the partitions - **root**, **var**, **usr** and **home**.

So let's restore **tmp** first:

```bash
Fixit# mkdir -p /mnt/usb
Fixit# mkdir -p /mnt/tmp-partition
Fixit# mkdir -p /mnt/partition
Fixit# mount /dev/da0 /mnt/usb
Fixit# mount /dev/ad4p5 /mnt/tmp-partition
Fixit# export TMPDIR=/mnt/tmp-partition
Fixit# cd /mnt/tmp-partition
Fixit# restore -rf /mnt/usb/tmp.dump
```

Now we can restore the rest of the partitions. In the commands below
replace **partition** with your actual partition - **ad4p2**,
**ad4p4** and **ad4p6**, **ad4p7** and also replace **dumpfile-name**
with the actual filename of the dump you restore from - **root.dump**,
**var.dump** and **usr.dump**.

```bash
Fixit# mount /dev/<partition> /mnt/partition
Fixit# cd /mnt/partition && restore -rf /mnt/usb/<dumpfile-name>
Fixit# cd /mnt
Fixit# umount /mnt/partition
```

Repeat the commands above until you restore all of the partitions.

One last thing we need to setup is to edit **fstab** file. So mount
the **root** partition again and open the file for editing. Once
you've done editing your *fstab* file it would look like something
like this:

```text
# Device                Mountpoint      FStype  Options         Dump Pass#
/dev/ad4p3              none            swap    sw              0    0
/dev/ad4p2              /               ufs     rw              1    1
/dev/ad4p5              /tmp            ufs     rw              2    2
/dev/ad4p6              /usr            ufs     rw              2    2
/dev/ad4p4              /var            ufs     rw              2    2
/dev/ad4p7              /usr/home       ufs     rw              2    2
/dev/acd0               /cdrom          cd9660  ro,noauto       0    0
```

And that's it! You are now ready to boot into your FreeBSD system!
Just remove the *Fixit* media and reboot.

## Creating software RAID-1

Now it is time to create the GPT partition tables on the second disk.

Please refer to the [Creating GPT partition tables chapter](/node/35),
for information on how to do this.

Again, if you are running FreeBSD 8.2 or later, consider using the
**backup** and **restore** options of gpart(8) for easy recovering of
partition tables on a different provider.

Now boot your FreeBSD system into **single-user mode**. From the
loader prompt execute the following command:

```text
OK boot -s
```

You are now in single-user mode. The first thing that we are going to
do is to edit the *fstab* file, so next time we boot into FreeBSD
we'll be using the mirrored devices. Now mount the *root* partition in
read-write mode, and update the fstab file:

```text
# mount -u /
# cat << __EOF__ > /etc/fstab
> # Device                Mountpoint      FStype  Options         Dump    Pass#
> /dev/ad4p3              none            swap    sw              0       0
> /dev/ad6p3              none            swap    sw              0       0
> /dev/mirror/gm0-root    /               ufs     rw              1       1
> /dev/mirror/gm0-tmp     /tmp            ufs     rw              2       2
> /dev/mirror/gm0-usr     /usr            ufs     rw              2       2
> /dev/mirror/gm0-var     /var            ufs     rw              2       2
> /dev/mirror/gm0-home    /usr/home       ufs     rw              2       2
> /dev/acd0               /cdrom          cd9660  ro,noauto       0       0
> __EOF__
```

Do not worry about all those **/dev/mirror** entries if it's something
unknown for you. You will see how we create these devices in a few
steps later.

We also need to load the **geom_mirror** kernel module during
boot-time, so execute the following command to do so:

```bash
# echo 'geom_mirror_load="YES"' >> /boot/loader.conf
```

Now we can create the mirrors.

**NOTE**: Please note that the first disk, that FreeBSD is currently
  booting from needs to be the first component we add to the mirror!
  Otherwise you will end up with empty partitions if you add the
  second disk as first component!

Creating a mirror for the **root** partition:

```bash
# gmirror label -vb round-robin gm0-root /dev/ad4p2
# gmirror insert gm0-root /dev/ad6p2
```

Creating a mirror for the **tmp** partition:

```bash
# gmirror label -vb round-robin gm0-tmp /dev/ad4p5
# gmirror insert gm0-tmp /dev/ad6p5
```

Creating a mirror for the **var** partition:

```bash
# gmirror label -vb round-robin gm0-var /dev/ad4p4
# gmirror insert gm0-var /dev/ad6p4
```

Creating a mirror for the **usr** partition:

```bash
# gmirror label -vb round-robin gm0-usr /dev/ad4p6
# gmirror insert gm0-usr /dev/ad6p6
```

Creating a mirror for the **home** partition:

```bash
# gmirror label -vb round-robin gm0-home /dev/ad4p7
# gmirror insert gm0-home /dev/ad6p7
```

While the mirror is rebuilding you may monitor it's status with the
following command:

```bash
# gmirror status
```

Once the mirrors have been rebuilt you should see something similar:

```bash
# gmirror status
           Name    Status  Components
mirror/gm0-root  COMPLETE  ad0p2
                           ad4p2
 mirror/gm0-var  COMPLETE  ad0p4
                           ad4p4
 mirror/gm0-tmp  COMPLETE  ad0p5
                           ad4p5
 mirror/gm0-usr  COMPLETE  ad0p6
                           ad4p6
mirror/gm0-home  COMPLETE  ad0p7
                           ad4p7
```

And that was it! Now you can boot your FreeBSD system from the
mirrored devices we have created.
