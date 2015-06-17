---
layout: post
title: How to reset an IBM Blade Chassis Management Module
created: 1348647030
---
Sometimes when a Blade's MM goes down and you cannot access the web
interface of the MM your last resort is the SSH and/or Telnet interface.

This document explains how to reset the Management Module (MM)
interface of a Blade chassis through it's Telnet command-line
interface.

## Reseting the MM from a Telnet session

In order to reset the MM from a Telnet session when, first telnet to
the target's MM, e.g.:

```bash
$ telnet blademm.example.org
Trying 10.xxx.yyy.zzz...
Connected to blademm.example.org.
Escape character is '^]'.

username: admin
password: 

Hostname:              blademm
Static IP address:     10.xxx.yyy.zzz
Burned-in MAC address: 5C:F3:00:01:02:03
DHCP:                  Disabled - Use static IP configuration.
Last login: Thursday August 9 2012 10:13 from 10.xxx.yyy.zzz (Web)
```

Now that you are connected to the Blade Chassis, change the target to
the MM you need to reset.

```bash
system> env -T system:mm[1]       
OK
```

The above command changes the target to the first MM on the chassis.

Note that the prompt changes to indicate the current target:

```bash
system:mm[1]>
```

Now simply reset the MM by executing the command below:

```bash
system:mm[1]> reset
```

You should be able to connect now to the MM by using the Web interface
after a successful reset.
