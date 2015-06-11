---
layout: post
title: Finding and removing locked files on a GNU/Linux system
tags: linux locked files
---

In this post we will see how you can find and remove
locked files on a GNU/Linux system.

For this purpose we are going to use `lsof(8)` and `lslk(8)` tools.

Sometimes it may happen that an application is not able to start up,
or even not working properly due to some files being locked by other
processes. This may happen in an environment where multiple clients are
using shared resources, like for example a shared home which is
mounted on multiple machines.

In this post we will see how to find and remove such locked files
under a Debian GNU/Linux system.

## Requirements

* root access or sudo rights

## Installation

First let's install the needed tools:

```bash
$ sudo apt-get update
$ sudo apt-get install lsof lslk
```

Now that we have the tools, le's have a look at the files that are
currently being locked.

## Finding the locked files

In order to view all locked files on the current system,
simply execute `lslk(8)`.

In this document as an example, we will find and remove a locked file
from a KDE session on a shared storage, where multiple clients are
mounting their home partitions from an NFS server.

Now let's view the locked files on the system:

```bash
$ sudo lslk
SRC         PID   DEV      INUM     SZ TY M ST WH END LEN NAME
(unknown)  1190 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
(unknown)  2294 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
(unknown)  2392 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
(unknown)  2397 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
lpd        3028 254,0   1212425      5  w 0  0  0   0   0 /var/run/lpd.pid
master     3187 254,0    262203     17  w 0  0  0   0   0 /var/spool/postfix/pid/master.pid
atd        3260 254,0   1212456      5  w 0  0  0   0   0 /var/run/atd.pid
(unknown)  3262 254,0   1212458         w 0  0  0   0   0 /var (/dev/mapper/vg0-var)
nmbd      14654 254,0    688137  32768  r 0  4  0   4   0 /var/lib/samba/public/unexpected.tdb
nmbd      14654 254,0    688148    696  r 0  4  0   4   0 /var/lib/samba/public/messages.tdb
nmbd      14654 254,0   1212429      6  w 0  0  0   0   0 /var/run/samba/public/nmbd.pid
smbd      14656 254,0    688138   8192  r 0  4  0   4   0 /var/lib/samba/public/locking.tdb
smbd      14656 254,0    688132   8192  r 0  4  0   4   0 /var/lib/samba/public/brlock.tdb
smbd      14656 254,0    688133 163840  r 0  4  0   4   0 /var/lib/samba/public/connections.tdb
smbd      14656 254,0    688147 188416  r 0  4  0   4   0 /var/lib/samba/public/sessionid.tdb
smbd      14656 254,0    688148    696  r 0  4  0   4   0 /var/lib/samba/public/messages.tdb
smbd      14656 254,0   1212430      6  w 0  0  0   0   0 /var/run/samba/public/smbd.pid
nmbd      14669 254,0    655876  32768  r 0  4  0   4   0 /var/lib/samba/users/unexpected.tdb
nmbd      14669 254,0    655875    696  r 0  4  0   4   0 /var/lib/samba/users/messages.tdb
nmbd      14669 254,0   1212448      6  w 0  0  0   0   0 /var/run/samba/users/nmbd.pid
smbd      14671 254,0    655870   8192  r 0  4  0   4   0 /var/lib/samba/users/locking.tdb
smbd      14671 254,0    655719    696  r 0  4  0   4   0 /var/lib/samba/users/brlock.tdb
smbd      14671 254,0    655718  57344  r 0  4  0   4   0 /var/lib/samba/users/connections.tdb
smbd      14671 254,0    655843 106496  r 0  4  0   4   0 /var/lib/samba/users/sessionid.tdb
smbd      14671 254,0    655875    696  r 0  4  0   4   0 /var/lib/samba/users/messages.tdb
smbd      14671 254,0   1212450      6  w 0  0  0   0   0 /var/run/samba/users/smbd.pid
```

What is interesting in the above output are the first four lines,
which are marked as `unknown`.

```bash
SRC         PID   DEV      INUM     SZ TY M ST WH END LEN NAME
(unknown)  1190 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
(unknown)  2294 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
(unknown)  2392 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
(unknown)  2397 254,1 308115263        r 0  0  0   0   0 /mnt/homes (/dev/mapper/vg0-homes)
```

We get actually quite useful information from the `lslk(8)` output -
we know where the file resides on the file system
(in our case that is `/mnt/homes`), what is it's inode number,
a PID, etc. Please refer to the manual page of `lslk(8)` for more
information about the different options and what the different column
meaning is.

What we can also notice is that there are four processes that are
using the same file from `/mnt/homes`.

Now let's try to find the files opened by these processes using the
`lsof(8)` tool:

```bash
$ sudo lsof -p 1190,2294,2392,2397
```

The above command will list all open files for the specified processes.
Sometimes it may happen that we don't actually get any result from that command.

What we can do in that case is to try to find the file using it's
inode number. So let's search for the file by it's inode number:

```bash
$ sudo find /mnt/homes -inum 308115263
/mnt/homes/home/foo/.qt/.qtrc.lock
```

After searching for the file using it's inode number we've found the
locked file and we can remove it:

```bash
$ sudo rm -f /mnt/homes/home/foo/.qt/.qtrc.lock
```

Now that we have removed the locked file, we can start up our
applications again.
