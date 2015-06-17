---
layout: post
title: Remove locked files on a NetApp filer
created: 1354181852
---
In this post we'll see how to find and remove locked files on a
[NetApp](http://www.netapp.com/) filer. 

It may happen sometimes that an application that depends on a shared
storage is not starting up or not even working properly. Most of the
times this is caused by locked files on the shared storage and here we
will see how to remove them.

Our setup consists of one NetApp filer with a volume that is exported
over CIFS, which in turn is mounted on a Windows(R) client machine.

First login to the NetApp filer over SSH using account that has
administrative privileges:

```bash
$ ssh root@netapp.examle.org
```
	
Once logged in on the NetApp filer, lets see the currently locked
files:

```bash
netapp> lock status -p cifs
```

The above command will show you the locked files on the NetApp filer
which are using the CIFS protocol.

And here's the output we got after executing the above command on the
NetApp filer:

```bash
netapp> lock status -p cifs
CIFS path=\(/vol/testvol/testvol/) host=10.xxx.yyy.zzz() owner=cifsuser state=GRANTED mode=Read-denyN oplock=None fsid=0x187ddf33 fileid=0x00000064
CIFS path=\MyApp\Logs\log-file.log(/vol/testvol/testvol/MyApp/Logs/log-file.log) host=10.xxx.yyy.zzz() owner=cifsuser state=GRANTED mode=Oplock-Excl oplock=Excl fsid=0x187ddf33 fileid=0x000a20bc
CIFS path=\MyApp\Logs\another-log-file.log(/vol/testvol/testvol/MyApp/Logs/another-log-file.log) host=10.xxx.yyy.zzz() owner=cifsuser state=GRANTED mode=Oplock-Excl oplock=Excl fsid=0x187ddf33 fileid=0x000a20bd
CIFS path=\MyApp\Logs(/vol/testvol/testvol/MyApp/Logs) host=10.xxx.yyy.zzz() owner=cifsuser state=GRANTED mode=Read-denyN oplock=None fsid=0x187ddf33 fileid=0x0001dd03
```

From the above output we can see the locks of the different files,
which are using the CIFS protocol. You are also able to filter the
lock files by specifying the hostname of a machine that is supposed to
hold a locked file or even specify different protocols.

For more information on the supported options by the *lock* command
execute this:

```bash
netapp> lock help
```
	
Once you've found the locked file that needs to be released, you can
do so by executing the command below:

```bash
netapp> lock break -f </vol/path/to/file> -p cifs
```

This is how for example we can release the lock of the `log-file.log`
file:

```bash
netapp> lock break -f "/vol/testvol/testvol/MyApp/Logs/log-file.log" -p cifs
```

Afterwards you should again check the output of the `lock status`
command to verify that the locked file has been successfully removed.
