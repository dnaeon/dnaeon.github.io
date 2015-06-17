---
layout: post
title: vPoller version 0.2.0 - pyVmomi support, new features & highlights
created: 1396355938
tags: python programming vmware virtualization
---
The v0.2.0 release of vPoller comes with a lot of new features, bug
fixes and lots of new methods for discovery and collecting of
vSphere Object properties!

Below you will a summary of the changes in `vPoller` version 0.2.0
along with a detailed changelog.

For more information about `vPoller` please refer to this article,
which explains the [design and goals of vPoller](/node/103).

You can also find the [vPoller project repository at
Github](https://github.com/dnaeon/py-vpoller).

## What's New in vPoller

Below you can find the summary of new features, improvements and
highlights in `vPoller` version 0.2.0:

* Integration with [pyVmomi API bindings](https://github.com/vmware/pyvmomi)
* Implemented efficient way for retrieving object properties using [vSphere Views](https://pubs.vmware.com/vsphere-50/index.jsp#com.vmware.wssdk.apiref.doc_50/vim.view.View.html)
* Improved performance during discovery and polling (thanks to [pyVmomi](https://github.com/vmware/pyvmomi)!)
* Lots of new discovery and polling methods for various vSphere Managed Objects
* `vSphere Agent` configuration is now managed by an SQLite backend
* ... and other small fixes and improvements

Please read further for more details on what's new in `vPoller`
version 0.2.0

## Integration with pyVmomi API bindings

Before release *v0.2.0*, *vPoller* was using
[pysphere](https://code.google.com/p/pysphere/) for managing
connections to the vSphere host and retrieving of properties.

[pysphere](https://code.google.com/p/pysphere/) is good enough for
most of the things we do, but unfortunately it can be quite slow when
we need to deal with lots of properties and objects, so we had to look
for a way to improve this.

Somewhere last year (near Christmas, 2013) VMware has officially
released the [VMware's vSphere API Python
bindings](https://github.com/vmware/pyvmomi). Compared to *pysphere*,
the *pyVmomi* bindings seems to be much better.

A few (but very important!) advantages of `pyVmomi` worth mentioning
here:

* Official API bindings released by VMware
* Function and object names map directly to what is documented in the [vSphere Web Services SDK](https://www.vmware.com/support/developer/vc-sdk/)
* Feature-full API bindings
* Much better performance
* ... and more ...

The only disadvantage of `pyVmomi` I can find so far is the lack of
documentation, so it takes some time getting used to it. Though, once
you wrap your head around you start to really appreciate how good
`pyVmomi` is.

So, in summary having `pyVmomi` integration in `vPoller` is a great
improvement by itself!

## Better way to retrieve object properties

Starting with `vPoller version 0.2.0` we are now collecting *vSphere
object properties* using [vSphere
Views](https://pubs.vmware.com/vsphere-50/index.jsp#com.vmware.wssdk.apiref.doc_50/vim.view.View.html).

By using *pyVmomi* with *vSphere Views* we are able to efficiently
select a subset of the objects in inventory and collect their
properties. And this gives us a better performance!

## Unified configuration of vPoller

From v0.2.0 of vPoller the configuration of all components now resides
in a single file, which makes it easier to maintain and configure.

In previous versions of `vPoller` we were using separate config file
for each component - `Proxy`, `Worker` and `Client`, but now all that
resides in a single file.

Example configuration file for `vPoller version 0.2.0`:

```ini
[proxy]
frontend = tcp://*:10123
backend  = tcp://*:10124
mgmt     = tcp://*:9999

[worker]
db       = /var/lib/vconnector/vconnector.db
proxy    = tcp://localhost:10124
mgmt     = tcp://*:10000
```

The table below provides information about the config entries used
along with a description for each of them.

| Section | Option    | Description                                                                       |
|---------|-----------|-----------------------------------------------------------------------------------|
| proxy   | frontend  | Endpoint to which clients connect and send their requests to                      |
| proxy   | backend   | Endpoint to which `vPoller Workers` connect and get requests for processing       |
| proxy   | mgmt      | Management endpoint, used for sending management messages to the `vPoller Proxy`  |
| worker  | db        | Path to the `vSphere Agents` SQLite database file                                 |
| worker  | proxy     | Endpoint to the `vPoller Proxy` backend to which the `vPoller Worker` connects    |
| worker  | mgmt      | Management endpoint, used for sending management messages to the `vPoller Worker` |

## More vPoller methods supported

The list of supported vPoller methods is growing. The table below
summarizes the supported vPoller methods as of version 0.2.0:

| vPoller Method         | Description                                                              |
|------------------------|--------------------------------------------------------------------------|
| about                  | Get 'about' information for the vSphere host this agent is connected to  |
| event.latest           | Get the latest registered event in the vSphere host the agent is connected to  |
| datacenter.discover    | Discover all vim.Datacenter managed objects                              |
| datacenter.get         | Get properties for a vim.Datacenter managed object                       |
| cluster.discover       | Discover all vim.ClusterComputeResource managed objects                  |
| cluster.get            | Get properties for a vim.ClusterComputeResource managed object           |
| resource.pool.discover | Discover all vim.ResourcePool managed objects                            |
| resource.pool.get      | Get properties for a vim.ResourcePool managed object                     |
| host.discover          | Discover all vim.HostSystem managed objects                              |
| host.get               | Get properties for a vim.HostSystem managed object                       |
| host.vm.get            | Get all Virtual Machines running on a specified vim.HostSystem           |
| host.datastore.get     | Get all datastores available to a vim.HostSystem                         |
| vm.discover            | Discover all vim.VirtualMachine managed objects                          |
| vm.disk.discover       | Discover all guest disks on a vim.VirtualMachine object                  |
| vm.net.discover        | Discover all network adapters on a vim.VirtualMachine object             |
| vm.get                 | Get properties for a vim.VirtualMachine object                           |
| vm.datastore.get       | Get all datastore used by a vim.VirtualMachine object                    |
| vm.disk.get            | Get information about a guest disk for a vim.VirtualMachine object       |
| vm.host.get            | Get the HostSystem in which a specified vim.VirtualMachine is running on |
| datastore.discover     | Discover all vim.Datastore objects                                       |
| datastore.get          | Get properties for a vim.Datastore object                                |

## vSphere Agents configuration is handled by an SQLite backend

The `vSphere Agents` are responsible for managing connections and
retrieve properties from a vSphere host. Their configuration such as
username, password, hostname, etc. used to be stored in plain text
files in previous versions.

With vPoller version 0.2.0 all that has been moved to an SQLite
database and the whole configuration can now be done from the
command-line using the `vconnector-cli` tool.

Here is how to add a new vSphere Agent in version 0.2.0 of vPoller.

```bash
$ sudo vconnector-cli -H vc01.example.org -U root -P p4ssw0rd add
```

Make sure you enable the `vSphere Agent`, otherwise it will not be
used by the `vPoller Worker`:

```bash
$ sudo vconnector-cli -H vc01.example.org enable
```

At any time you can view the currently registered `vSphere Agents` by
running the `vconnector-cli get` command, e.g.:

```bash
$ sudo vconnector-cli get
+--------------+---------------------+-------------+-----------+
| Hostname     | Username            | Password    |   Enabled |
+==============+=====================+=============+===========+
| vc01         | root                | p4ssw0rd    |         1 |
+--------------+---------------------+-------------+-----------+
```
