---
layout: post
title: Linux ignores kernel parameters defined in sysctl.conf file
created: 1352999127
tags: linux virtualization kvm
---
Recently I keep playing with KVM and testing things out, and today I
started noticing strange networking issues with the communication
between the VM guest domains after migration to a brand new
Debian Wheezy B2 hypervisor.

The networking setup is quite simple - the VM guests are attached to a
network bridge on the host. Everyone who ever worked with KVM knows
already that you need to put a few kernel parameters to your
*/etc/sysctl.conf* file related to brigding. These kernel parameters
are the ones listed below:

```text
net.bridge.bridge-nf-call-ip6tables=0
net.bridge.bridge-nf-call-iptables=0
net.bridge.bridge-nf-call-arptables=0
```

So I've added these to */etc/sysctl.conf* when I first setup the
hypervisor and in order to make these changes active I did:

```bash
$ sudo sysctl -p
net.bridge.bridge-nf-call-ip6tables = 0
net.bridge.bridge-nf-call-iptables = 0
net.bridge.bridge-nf-call-arptables = 0
```

But.. today I've decided to reboot the system and that's when I
started seening strange networking issues. 

I run CFEngine on each VM guest in my private network, and the problem
I had was that clients were no longer able to authenticate to the
CFEngine server by using their ppkeys.

After looking aroud for a while the problem was found to be that
clients were initiating the connection by using the bridge's IP
address, which clearly shouldn't be the case.

So what caused this strange network behaviour? 

The root cause after some investigation was that our *sysctl.conf(5)*
kernel parameters related to bridging were not set after the reboot.

For setting up the *sysctl.conf(5)* kernel parameters under Linux the
*/etc/init.d/procps* init.d script is being used, but clearly our
kernel parameters were not set.

So what really caused this?

The real problem here is that */etc/init.d/procps* script is being
executed too early in the boot process and our bridge kernel
parameters that we usually set in */etc/sysctl.conf* are not yet
available, as the bridge module is not loaded yet.

One solution to this problem as others have also pointed out is to use
*/etc/rc.local* and do a `sysctl -p` to set any kernel parameters, but
I don't really like this approach.

A more elegant way for fixing this issue would be to explicitely load
the *bridge* module during boot from */etc/modules*, thus making our
bridge related kernel parameters available early in the boot process.

To do that, execute this command:

```bash
echo "bridge" >> /etc/modules
```
