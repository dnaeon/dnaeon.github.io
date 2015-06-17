---
layout: post
title: Linux bonding, vlans, bridges & KVM
created: 1353084380
tags: linux virtualization kvm
---
In this blog post I'll share with you how to setup the
networking part of your virtualization solution using KVM.

We will talk about bonding, vlans and bridges.

Setting up a KVM host is an easy one. What people often forget to
think about is the networking part of it. I'm not going to explain
what bonds, vlans and bridges are here - there's plenty of good
documentation out there which explains it better.

So if you are new to bonds, vlans and bridges I'd suggest you spend
some time checking the links below:

* [http://www.kernel.org/doc/Documentation/networking/bonding.txt](http://www.kernel.org/doc/Documentation/networking/bonding.txt)
* [http://www.linuxjournal.com/article/7268](http://www.linuxjournal.com/article/7268)
* [http://www.linuxfoundation.org/collaborate/workgroups/networking/bridge](http://www.linuxfoundation.org/collaborate/workgroups/networking/bridge)

The target system I'm using as a KVM hypervisor is Debian Wheezy,
which is already configured for hosting KVM guest domains. The last
thing that remains is to configure the networks for the KVM guests.

The networking setup of the KVM hypervisors we have is more-or-less
described like this. First we configure bonding on the interfaces for
link aggregation and failover, then we create VLANs of the bond for
splitting the network into logical segments for our VMs, and lastly we
configure bridges of the VLANs, which will be attached to the running
KVM guest domains.

So, lets start, shall we? Install First, lets install some packages:

```bash
$ sudo apt-get install ifenslave-2.6 vlan bridge-utils
```
	
Then we create two bonding interfaces - *bond0* and *bond1* and put
our networking configuration in */etc/network/interfaces*, which looks
like this:

```text
auto lo
iface lo inet loopback

auto eth0
iface eth0 inet manual
	bond-master bond0

auto eth1
iface eth1 inet manual
	bond-master bond1

auto eth2
	iface eth2 inet manual
	bond-master bond0

auto eth3
iface eth3 inet manual
	bond-master bond1

auto bond0
iface bond0 inet static
	address 10.10.100.100
   	netmask 255.255.255.0
   	gateway 10.10.100.1
   	slaves none
   	bond-mode active-backup
   	bond-miimon 100
   	bond-downdelay 200
   	bond-updelay 200

auto bond1
iface bond1 inet manual
	slaves none
	bond-mode active-backup
	bond-miimon 100
	bond-downdelay 200
	bond-updelay 200
```

Now we have two bonds, so lets create the VLANs and bridges for our
KVM guest domains. Append the following lines to
*/etc/network/interfaces* file for setting up the VLANs and KVM
bridges.

```text
# vlan50 - management network
auto vlan50
	iface vlan50 inet manual
	vlan_raw_device bond1
		
# br50 - management network
auto br50
iface br50 inet static
   	address  192.168.100.1
	netmask  255.255.255.0
	network  192.168.100.0
	bridge_ports vlan50
	bridge_maxwait 5
	bridge_stp off
	bridge_fd 0

# vlan100 - Test network
auto vlan100
iface vlan100 inet manual
	vlan_raw_device bond1

# br100 - Test network
auto br100
iface br100 inet static
	address  192.168.200.1
	netmask  255.255.255.0
	network  192.168.200.1
	bridge_ports vlan100
	bridge_maxwait 5
	bridge_stp off
	bridge_fd 0
```

The reason we create VLANs is because we want to have different
networks for our VM machines - test network, engineering, production,
etc. Out of the VLANs we create bridges as well, so that we can attach
our KVM VMs to them.

Once you are ready you can restart the network or reboot your machine
in order to apply the changes. Once the changes are in place you can
check the status of your bonds in `/proc/net/bonding`*, e.g.:

```bash
# cat /proc/net/bonding/bond0 
Ethernet Channel Bonding Driver: v3.7.1 (April 27, 2011)

Bonding Mode: fault-tolerance (active-backup)
Primary Slave: None
Currently Active Slave: eth0
MII Status: up
MII Polling Interval (ms): 100
Up Delay (ms): 200
Down Delay (ms): 200

Slave Interface: eth0
MII Status: up
Speed: 100 Mbps
Duplex: full
Link Failure Count: 0
Permanent HW addr: 00:25:90:95:7f:70
Slave queue ID: 0

Slave Interface: eth2
MII Status: up
Speed: 100 Mbps
Duplex: full
Link Failure Count: 0
Permanent HW addr: 00:25:90:93:6e:04
Slave queue ID: 0
```

And for the VLANs:

```bash
$ sudo cat /proc/net/vlan/vlan50 
vlan50  VID: 50  REORDER_HDR: 1  dev->priv_flags: 4001
            total frames received          275
             total bytes received        25471
         Broadcast/Multicast Rcvd           76

         total frames transmitted          266
          total bytes transmitted        34553
      Device: bond1
      INGRESS priority mappings: 0:0  1:0  2:0  3:0  4:0  5:0  6:0 7:0
       EGRESS priority mappings: 
```

Finally lets check our bridge:

```bash
# brctl show br50
bridge name     bridge id               STP enabled     interfaces
br50            8000.002590957f71       no              vlan50
```
