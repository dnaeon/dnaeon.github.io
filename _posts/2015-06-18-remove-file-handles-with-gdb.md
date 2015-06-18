---
layout: post
title: Remove file handles with gdb(1)
tags: programming
---
A process holding an open handle to a file and eventually removing
that file while the process still runs often results in a
*stale* file handle.

What that mean is that resources occupied by the file instead of
being freed are actually not. Root cause of this is the process still
holding an open file handle to the now removed file.

Possible solutions to this issue are:

* Use `lsof(8)` to find the removed file, then kill the process holding it's file handle open
* Use `gdb(1)` to attach to the process, then simply close the file handle

First, let's find the files being removed using `lsof(8)`. Considering
that our file was located somewhere in `/var` we could use the
command below to find it.

```bash
$ sudo lsof /var
```

Next, we attach to the process holding the file handle:

```bash
$ sudo gdb attach <pid>
```

Find the files that are opened by the process. Either look at the
result from `lsof(8)`, or check `/proc/<pid>/fd`:

```bash
$ sudo ls -l /proc/19080/fd
total 0
lrwx------ 1 root root 64 May 17 14:30 0 -> socket:[5378591]
l-wx------ 1 root root 64 May 17 14:30 1 -> /var/log/kern.log
l-wx------ 1 root root 64 May 17 14:30 2 -> /var/log/messages
lrwx------ 1 root root 64 May 17 14:30 3 -> /dev/xconsole
lr-x------ 1 root root 64 May 17 14:30 4 -> /proc/kmsg
l-wx------ 1 root root 64 May 17 14:30 5 -> /var/log/syslog
l-wx------ 1 root root 64 May 17 14:30 6 -> /var/log/auth.log
```

Now we can close the file descriptor from the `gdb(1)` session that
we have, which is already attached to our process.

NOTE: using `"call"` instead of `"p"` works fine too.

```bash
gdb> p close(2)
$1 = 0
```

Let's redirect the file descriptor to a different file now.

```bash
gdb> p fopen("/tmp/file", "w")
$2 = 20746416
(gdb) p fileno($2)
$3 = 7
gdb> quit
```	
