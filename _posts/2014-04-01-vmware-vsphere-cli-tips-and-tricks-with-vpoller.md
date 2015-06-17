---
layout: post
title: VMware vSphere CLI tips & tricks with vPoller
created: 1396375518
tags: vmware virtualization programming python
---
As a sysadmin I often have the need to get information about an
environment quickly and efficiently. I am sure that most of the
fellow sysadmins out there also automate their daily tasks in some
form of scripts and tools, which make their life easier.

In this post I will share with you some of the CLI tools I use in
order to automate stuff when working in a VMware vSphere
environment. Over time they've proven to be invaluable to me and saved
me a lot of time, so hopefully it will do the same for you.

So, without further ado let's see some VMware vSphere CLI tips &
tricks, which hopefully will find it's place in your sysadmin toolkit
:)

## Whetting your appetite

Let's first see the tools we are going to talk about in this post.

![_config.yml]({{ site.baseurl }}/images/vcli-all-tools.jpg)

As you can see from the above screenshot we have a pretty decent
number of vSphere CLI tools which we can use. Now let's see some of
them in action.

## Getting 'about' information from a vSphere host

This is how you could get the vSphere version and build numbers from a
vSphere host from the command-line:

![_config.yml]({{ site.baseurl }}/images/vcli-about-1.jpg)

Wait, we can even request more info if we ask to:

![_config.yml]({{ site.baseurl }}/images/vcli-about-2.jpg)

Pretty useful, isn't it? :)

## Discovering objects in our vSphere environment

Let's now see how we can discover various objects from our VMware
vSphere environment.

This is how we can discover all datacenters for example:

![_config.yml]({{ site.baseurl }}/images/vcli-datacenter-discover.jpg)

What ESXi hosts do we have in our environment:

![_config.yml]({{ site.baseurl }}/images/vcli-host-discover.jpg)

Okay, that's good. We have one ESXi host in our environment, but what
is it's power state?

![_config.yml]({{ site.baseurl }}/images/vcli-host-discover-2.jpg)

What about our datastores?

![_config.yml]({{ site.baseurl }}/images/vcli-datastore-discover-1.jpg)

Let's see the datastore URLs as well:

![_config.yml]({{ site.baseurl }}/images/vcli-datastore-discover-2.jpg)

As you can see we can *virtually* discover any vSphere managed object
in our environment. What we can also do is during discovery we can
request additional properties to be collected, which I think is pretty
neat.

## Getting object properties from our vSphere environment

In the previous section we've seen how to discover various vSphere
objects, but what about getting properties for a single object? Is
that possible?

Sure, it is! :)

Let's see what is the capacity and free space of our `datastore1`
datastore:

![_config.yml]({{ site.baseurl }}/images/vcli-datastore-get.jpg)

Okay, seems like our datastore is in good shape and is accessible.

Let's see what is the amount of memory on our ESXi host and it's
status:

![_config.yml]({{ site.baseurl }}/images/vcli-host-get.jpg)

Let's see what disks do we have on a Virtual Machine:

![_config.yml]({{ site.baseurl }}/images/vcli-vm-disk-discover.jpg)

And what is the capacity and free space of the `/storage/db` file
system:

![_config.yml]({{ site.baseurl }}/images/vcli-vm-disk-get_0.jpg)

Let's find out on which host this Virtual Machine is running on:

![_config.yml]({{ site.baseurl }}/images/vcli-vm-host-get_0.jpg)

And what are the datastores used by this Virtual Machine:

![_config.yml]({{ site.baseurl }}/images/vcli-vm-datastore-get_0.jpg)

Let's also check the network configuration for our Virtual Machine:

![_config.yml]({{ site.baseurl }}/images/vcli-vm-net-discover.jpg)

This is just a fraction of the properties you can get out of your
VMware vSphere environment. For all the properties you can get please
check the [vSphere Web Services SDK
documentation](https://www.vmware.com/support/developer/vc-sdk/).

## Do we have any events?

Let's see what is the latest event registered in our vCenter server:

![_config.yml]({{ site.baseurl }}/images/vcli-event.jpg)

Okay, we can also see the events from our vCenter from the
command-line, which I find as cool! :)

## Wrapping it up

If you managed to get this far, probably you are already wondering
where to get the `vcli-*` tools, so in case you thought I've forgotten
to mention that, you were wrong :)

The `vcli-*` tools are shell aliases (or more precisely shell
functions) which are now part of the `vPoller` project repository.

This is how to get the `vcli-*` tools installed and ready for use:

* Go to the [vPoller project at Github](https://github.com/dnaeon/py-vpoller) and get `vPoller` installed and configured
* Source the `vcli.source` file which would get you the tools ready for use

```bash
$ source /path/to/py-vpoller/misc-tools/vcli.source
```

If you are working in a VMware vSphere environment frequently (as I
do) probably you would put the above line in your `.bashrc` or
similar, so you don't have to do it each time you need the tools.

And that was it, hope you find these tips & tricks useful! :)
