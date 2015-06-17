---
layout: post
title: How to create a KVM template in Proxmox VE
created: 1351702869
tags: linux virtualization kvm proxmox
---
Probably this is something that most people working with Proxmox VE
have stumbled upon.

In Proxmox VE you can only use (at least for now) OpenVZ templates. If
you are intrested in running Proxmox VE with KVM (as I was), then you
will be surprised that the only supported templates are for OpenVZ.

Now, don't panic! We can still figure this out!

Well, Proxmox VE might not support KVM templates, but you can still
create such easily, and here I'll just drop a few lines to tell you
how to do it.

Considering that you already have Proxmox VE installed and configured,
we'll go straight forward on the topic.

Go ahead and create a new VM instance in Proxmox VE that we will use
as a template. Once ready with that, there are few things to do before
we can start cloning our template system.

Remove all DHCP client leases on the template system:

```bash
$ sudo rm -f /var/lib/dhcp/*.leases
```
	
Remove all persistent udev rules on the template system:

```bash
$ sudo rm -f /lib/udev/rules.d/*persistent*
```
	
The above is important as it removes persistent rules for the
networking, and you might get different network interfaces *ethX* if
you forget to remove them before actually starting to clone the
system.

That should be all for the template system!

Now, lets go and do a clone of our example template system.

Login to the Proxmox VE node, e.g. *hv01.example.org*:

```bash
$ ssh hv01.example.org
```

Navigate to the */etc/pve/qemu-server* where our KVM machines
configuration reside:

```bash
# cd /etc/pve/qemu-server
```
	
Here you can see a *.conf* file named after the VM machine's ID. Here
*100.conf* corresponds to my template system, I've already installed.

```bash
$ sudo ls -l /etc/pve/qemu-server/
total 1
-rw-r----- 1 root www-data 212 Oct 31 14:33 100.conf
```

And here's the contents of my template's *100.conf* file:

```text
bootdisk: virtio0
cores: 1
ide2: none,media=cdrom
memory: 2048
name: debian-wheezy-b2-template
net0: rtl8139=26:6E:0B:DC:D9:72,bridge=vmbr0
ostype: l26
sockets: 1
virtio0: images:100/vm-100-disk-1.qcow2,size=10G
```

Now, cloning that template system is a two-step thing. First we copy
the template's *.conf* file and then we copy the template's disk
image.

So, copy the template's *.conf* file and give it a name which is the
next available ID number, in my case it's *101*:

```bash
$ sudo cp /etc/pve/qemu-server/100.conf /etc/pve/qemu-server/101.conf
```

Open the newly copied file for editing and update the following
options - *name* and location to the disk image.

I also choose to remove the *net0* interface, as I usually add it
before starting the system, so that I get a nice looking
auto-generated MAC address by Proxmox VE.

After the update my *101.conf* file looks like this:

```text
bootdisk: virtio0
cores: 1
ide2: none,media=cdrom
memory: 2048
name: my-first-clone
ostype: l26
sockets: 1
virtio0: images:101/vm-101-disk-1.qcow2,size=10G
```

Now, lets clone the disk image. Navigate to your Proxmox VE datastore,
the default location is */var/lib/vz/images*.

There you will find again a folders with the VM IDs you have. Now
simply copy the template's folder to a folder with your new VM ID and
rename the disk image to match the new KVM instance.

```bash
$ sudo cp /var/lib/vz/images/100 /var/lib/vz/images/101
$ sudo mv /var/lib/vz/images/101/vm-100-disk-1.qcow2 /var/lib/vz/images/101/vm-101-disk-1.qcow2
```
	
And that's it! Now login to your Proxmox VE node using the web
interface and you should see your new cloned system. Don't forget to
add a network interface and after that just start your instance.
