---
layout: post
title: vPoller version 0.1.1 - New Features & Improvements
created: 1390246479
tags: python programming
---
The VMware vSphere Distributed Pollers, a.k.a vPoller has reached
version 0.1.1. 

New features and improvements have been added to this version of
[vPoller](/node/104). Below you will find a summary of the changes in
*vPoller* version 0.1.1 along with a detailed changelog.

For more information about *vPoller* please refer to this article,
which explains the [design and goals of vPoller](/node/103).

## What's New in vPoller

Below you can find the summary of new features and improvements in
*vPoller* version 0.1.1:

* New discovery and polling methods
* Caching of vSphere Managed Object References
* Multiple properties support
* Support for VirtualMachine objects
* Support for Datacenter objects
* Support for ClusterComputeResource objects
* vPoller CSV helper module
* ... and other cosmetic changes

Please read further for more details on what's new in *vPoller*
version 0.1.1

## New discovery and polling methods

Some of the old methods for polling a vSphere Object property has been
renamed from `<object>.poll` to `<object>.get`.

The table below summarizes the methods used in previous versions and
the new names as found in version 0.1.1.

| Old Method           | New Method          | Description                                       |
|----------------------|-------------------------------------------------------------------------|
| host.poll            | host.get            | Get properties of a HostSystem object (ESXi host) |
| datastore.poll       | datastore.get       | Get properties of a Datastore object              |

New methods and support for other vSphere Objects has been added in
version 0.1.1 as well.

The table below provides a summary of the currently existing and
supported methods a `vPoller Worker` now accepts and processes.

| Method               | Description                                        |
|----------------------|----------------------------------------------------|
| host.get             | Get properties of a HostSystem object (ESXi host)  |
| datastore.get        | Get properties of a Datastore object               |
| vm.get               | Get properties of a VirtualMachine object          |
| datacenter.get       | Get properties a Datacenter object                 |
| cluster.get          | Get property of a ClusterComputeResource object    |
| host.discover        | Discovers all HostSystem objects (ESXi hosts)      |
| datastore.discover   | Discovers all Datastores objects                   | 
| vm.discover          | Discovers all VirtualMachine objects               |
| datacenter.discover  | Discovers all Datacenter objects                   |
| cluster.discover     | Discovers all ClusterComputeResources objects      |

As you can see we now support discovery and polling of the following
vSphere Objects.

* HostSystem (ESXi hosts)
* Datastore
* VirtualMachine
* Datacenter
* ClusterComputeResource

More methods and support for other vSphere Objects are to be supported
as well soon enough.

## Caching of vSphere Managed Object References

As with version 0.1.1 of *vPoller* we now use caching of vSphere MORs.

Caching of vSphere MORs helps avoiding unnecessary calls to discover a
vSphere Object during polling.

In order to enable caching you need to add a new config entry to your
`vPoller Worker Agents`:

```ini
cachettl = 30
```

The value of `cachettl` is the time in minutes in which we keep a
cache in memory for vSphere Objects.

Once that cache expires the `vPoller Worker Agent` will update it's
cache with any new objects we might have.

## Multiple properties support

Support for discovery and polling of multiple properties has been
added to this version as well.

With multiple properties support you can send a request to your
`vPoller Workers` asking for multiple properties to be returned.

You can request multiple properties for an object by appending the
properties separated by a comma at the command-line.

The example command below would request multiple properties to be
returned for an ESXi host:

```bash
$ vpoller-client -m host.get \
  		 -V vc01-test.example.org \
		 -e tcp://localhost:10123 \
		 -n esx01.example.org \
		 -p runtime.powerState,hardware.memorySize,summary.overallStatus
```
	
Additional properties can also be requested during discovery process
as well.
	
## Support for VirtualMachine objects

Support for `VirtualMachine` objects has been added in this version as
well.

The new methods related to `VirtualMachine` objects are `vm.discover`
and `vm.get` which take care of discovering and polling of
`VirtualMachine` properties respectively.

This is how we could discover all Virtual Machines on a vSphere host:

```bash
$ vpoller-client -m vm.discover -V vc01-test.example.org -e tcp://localhost:10123
```
	
And this is how we could request properties for a `VirtualMachine`
object:

```bash
$ vpoller-client -m vm.discover \
  		 -V vc01-test.example.org \
		 -e tcp://localhost:10123 \
		 -n vm01.example.org \
		 -p summary.overallStatus
```
	
## Support for Datacenter objects

Basic support for `Datacenter` objects has been added.

Now you can also discover all Datacenters on a vCenter server by using
a command similar to the one below:

```bash
$ vpoller-client -m datacenter.discover -V vc01-test.example.org -e tcp://localhost:10123
```
	
Support for polling of `Datacenter` object properties should be
available soon as well.

## Support for ClusterComputeResource objects

Basic support for `ClusterComputeResource` objects has been added.

Now you can also discover all clusters on a vCenter server by using a
command similar to the one below:

```bash
$ vpoller-client -m cluster.discover -V vc01-test.example.org -e tcp://localhost:10123
```
	
Support for polling `ClusterComputeResource` object properties should
become available soon as well.

## vPoller CSV helper module

A new `vPoller Helper Module` has been added in version 0.1.1.

The new helper module takes care of translating a result `vPoller
Worker` message to CSV format.

A summary of the currently supported and existing `vPoller Helper
Modules` can be found on the table below:

| Helper                    | Description                                          |
|---------------------------|------------------------------------------------------|
| vpoller.helpers.zabbix    | Helper which returns data in Zabbix-friendly format  |
| vpoller.helpers.csvhelper | Helper which returns data in CSV format              |

In order to use the `vPoller Helper Modules` specify the helper you
need at the command-line similar similar to the command below:

```bash
$ vpoller-client -H vpoller.helpers.csvhelper \
  		 -m datastore.get \
		 -e tcp://localhost:10123 \
		 -V vc01.example.org \
		 -n <datastore-url> \
		 -p summary.capacity
```


