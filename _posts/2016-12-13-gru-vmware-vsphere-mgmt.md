---
layout: post
title: Managing VMware vSphere environment with Go and Lua by using Gru orchestration framework
tags: configuration management orchestration golang lua programming gru vmware vsphere virtualization
---
Amongst the various improvements and fixes in
[Gru orchestration framework](https://github.com/dnaeon/gru) one of the
highlights of
[version 0.5.0](https://github.com/dnaeon/gru/blob/master/docs/CHANGELOG.md#050-november-24-2016) of the
project is the added support for managing VMware vSphere environments.

If you haven't heard about Gru yet - Gru is a concurrent
orchestration and configuration management framework written
in [Go](https://golang.org/) and [Lua](https://www.lua.org/) 
as the [DSL language](https://en.wikipedia.org/wiki/Domain-specific_language),
which allows you to manage your environment in an idempotent manner.

You can read more about the project at
[Github](https://github.com/dnaeon/gru), where you can also find
various code examples and use cases.

In this post we will see how we can use Gru for automating the
management of VMware vSphere environment by using the recently
introduced features in the project.

The *resources* we will talk about in this post are `Datacenter`,
`Cluster`, `ClusterHost`, `Host`, `DatastoreNfs` and `VirtualMachine`.

In order to keep things simple and not repeat ourselves we will first
create a [Lua table](https://www.lua.org/pil/2.5.html), which will
contain the credentials used to authenticate against the remote
VMware vSphere API endpoint.

```lua
--
-- The credentials to the remote VMware vSphere API endpoint
--
credentials = {
   username = "root",
   password = "r00tp4ssw0rd",
   endpoint = "https://vc01.example.org/sdk",
   insecure = true, --> Needed if the vCenter is using a self-signed certificate
}
```

The very first thing that we are going to automate is the creation of the
vSphere Datacenter. In order to do that we will use the *Datacenter*
resource as shown in the code below.

```lua
--
-- Manage the VMware vSphere Datacenter
--
dc = vsphere.datacenter.new("MyDatacenter")
dc.username = credentials.username
dc.password = credentials.password
dc.endpoint = credentials.endpoint
dc.insecure = credentials.insecure
dc.state = "present"

catalog:add(dc)
```

The next thing we need to take care of in our VMware vSphere
environment is the cluster. The code below takes care of
creating a vSphere Cluster and also enables the DRS service for it. 

```lua
--
-- Manage the VMware vSphere Cluster
--
cluster = vsphere.cluster.new("MyCluster")
cluster.endpoint = credentials.endpoint
cluster.username = credentials.username
cluster.password = credentials.password
cluster.insecure = credentials.insecure

cluster.state = "present"
cluster.path = "/MyDatacenter/host"
cluster.config = {
   enable_drs = true,
   drs_behavior = "fullyAutomated",
}
cluster.require = { dc:ID() } --> The cluster depends on the datacenter

catalog:add(cluster)
```

One thing to note in the code above is that the *Cluster* resource 
depends on the *Datacenter* resource we've created previously, which
will ensure proper evaluation and processing of resources in the resulting
resources [DAG graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph).

Once we have the cluster ready we can now add some
compute resources to it by adding an ESXi host to the cluster.

The code below shows how to add an ESXi host a vSphere cluster.

```lua
--
-- Add an ESXi host to the VMware vSphere Cluster
--
host = vsphere.cluster_host.new("esxi01.example.org")
host.endpoint = credentials.endpoint
host.username = credentials.username
host.password = credentials.password
host.insecure = credentials.insecure

host.state = "present"
host.path = "/MyDatacenter/host/MyCluster"
host.esxi_username = "root"
host.esxi_password = "esxi_p4ssw0rd"
host.ssl_thumbprint = "ssl-thumbprint-of-host"
host.require = { cluster:ID() } --> The ESXi host depends on the cluster

catalog:add(host)
```

Make sure to provide the correct SSL thumbprint of your ESXi host.
[This post in virtualGhetto](http://www.virtuallyghetto.com/2012/04/extracting-ssl-thumbprint-from-esxi.html)
provides details on how to extract the SSL thumbprint of an
ESXi host.

Gru also provides a resource for managing various configuration
settings of ESXi hosts such as DNS and lockdown mode settings.
For more information about the various settings that you can
manage on ESXi hosts make sure to check the
[Host](https://godoc.org/github.com/dnaeon/gru/resource#Host)
resource.

Another resource that we will use is the `DatastoreNfs` one,
which allows you to manage NFS datastores on ESXi hosts.

The following example shows how to mount an NFS datastore on our
ESXi host.

```lua
--
-- Mount an NFS datastore on our ESXi host
--
datastore = vsphere.datastore_nfs.new("vm-storage01")
datastore.endpoint = credentials.endpoint
datastore.username = credentials.username
datastore.password = credentials.password
datastore.insecure = credentials.insecure

datastore.state = "present"
datastore.path = "/MyDatacenter/datastore"
datastore.hosts = {
   "/MyDatacenter/host/MyCluster/esxi01.example.org",
}
datastore.nfs_server =  "nfs01.example.org"
datastore.nfs_path = "/storage/vm-storage01"
datastore.mode = "readWrite"
datastore.require = { host:ID() } --> The datastore depends on the ESXi host

catalog:add(datastore)
```

The last resource that we will see in this post is the
*VirtualMachine* resource.

Using the Lua-based DSL of the project we will create a few
Virtual Machines.

```lua
--
-- Manage VMware vSphere Virtual Machines
--
names = { "kevin", "bob", "stuart" } --> You know these guys, right?

for _, name in ipairs(names) do
   vm = vsphere.vm.new(name)
   vm.endpoint = credentials.endpoint
   vm.username = credentials.username
   vm.password = credentials.password
   vm.insecure = credentials.insecure

   vm.state = "present"
   vm.path = "/MyDatacenter/vm"
   vm.pool = "/MyDatacenter/host/MyCluster"
   vm.datastore = "/MyDatacenter/datastore/vm-storage01"
   vm.wait_for_ip = true
   vm.power_state = "poweredOn"
   vm.template_config = {
      use = "/MyDatacenter/vm/Templates/centos-7-x86-64-template",
   }
   vm.require = { host:ID(), datastore:ID() } --> The VM depends on the ESXi host and datastore

   catalog:add(vm)
end
```

Before we run the code we have written so far, let us first have a
look at the resulting resource dependency graph.

In order to do that we will use the `gructl graph` command, e.g.

```bash
$ gructl graph site/code/vsphere.lua | dot -Tpng -O
```

The resulting DAG graph looks like this.

[![]({{ site.baseurl }}/images/gru-vsphere-resources-dag.png)]({{ site.baseurl }}/images/gru-vsphere-resources-dag.png){:.glightbox}

What is worth mentioning about Gru is that it is good in executing
operations concurrently. Looking at the above dependency graph we can
see that if all dependencies are satisfied by the time we reach the
*VirtualMachine* resources then we could actually create our
Virtual Machines concurrently.

Being able to execute operations like this in concurrent way can
greatly reduce the time required for standing up a new
environment.

Time to actually run the code we've written so far!

```bash
$ gructl apply site/code/vsphere.lua
```

The result of the above command looks like this.

```bash
$ gructl apply site/code/vsphere.lua
2016/12/12 13:59:55 Loaded 7 resources
2016/12/12 13:59:55 Starting 4 goroutines for concurrent processing
2016/12/12 13:59:55 datacenter[MyDatacenter@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 13:59:55 datacenter[MyDatacenter@https://vc01.example.org/sdk] creating datacenter in /
2016/12/12 13:59:55 cluster[MyCluster@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 13:59:55 cluster[MyCluster@https://vc01.example.org/sdk] creating cluster
2016/12/12 13:59:55 cluster[MyCluster@https://vc01.example.org/sdk] property 'cluster-config' is out of date
2016/12/12 13:59:55 cluster[MyCluster@https://vc01.example.org/sdk] setting cluster config
2016/12/12 13:59:55 cluster_host[esxi01.example.org@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 13:59:55 cluster_host[esxi01.example.org@https://vc01.example.org/sdk] adding host to /MyDatacenter/host/MyCluster
2016/12/12 14:00:16 datastore_nfs[vm-storage01@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 14:00:16 datastore_nfs[vm-storage01@https://vc01.example.org/sdk] mounting datastore on esxi01.example.org
2016/12/12 14:00:18 vm[stuart@https://vc01.example.org/sdk] is concurrent
2016/12/12 14:00:18 vm[bob@https://vc01.example.org/sdk] is concurrent
2016/12/12 14:00:18 vm[kevin@https://vc01.example.org/sdk] is concurrent
2016/12/12 14:00:18 vm[stuart@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 14:00:18 vm[bob@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 14:00:18 vm[kevin@https://vc01.example.org/sdk] is absent, should be present
2016/12/12 14:00:18 vm[stuart@https://vc01.example.org/sdk] cloning virtual machine from /MyDatacenter/vm/centos-7-x86-64-template
2016/12/12 14:00:18 vm[kevin@https://vc01.example.org/sdk] cloning virtual machine from /MyDatacenter/vm/centos-7-x86-64-template
2016/12/12 14:00:18 vm[bob@https://vc01.example.org/sdk] cloning virtual machine from /MyDatacenter/vm/centos-7-x86-64-template
2016/12/12 14:02:03 vm[bob@https://vc01.example.org/sdk] property 'power-state' is out of date
2016/12/12 14:02:03 vm[bob@https://vc01.example.org/sdk] setting power state to poweredOn
2016/12/12 14:02:03 vm[stuart@https://vc01.example.org/sdk] property 'power-state' is out of date
2016/12/12 14:02:03 vm[stuart@https://vc01.example.org/sdk] setting power state to poweredOn
2016/12/12 14:02:04 vm[kevin@https://vc01.example.org/sdk] property 'power-state' is out of date
2016/12/12 14:02:04 vm[kevin@https://vc01.example.org/sdk] setting power state to poweredOn
2016/12/12 14:02:06 vm[stuart@https://vc01.example.org/sdk] waiting for IP address
2016/12/12 14:02:06 vm[kevin@https://vc01.example.org/sdk] waiting for IP address
2016/12/12 14:02:06 vm[bob@https://vc01.example.org/sdk] waiting for IP address
2016/12/12 14:03:04 vm[bob@https://vc01.example.org/sdk] virtual machine IP address is 10.xxx.yyy.zzz
2016/12/12 14:03:04 vm[stuart@https://vc01.example.org/sdk] virtual machine IP address is 10.xxx.yyy.zzz
2016/12/12 14:03:04 vm[kevin@https://vc01.example.org/sdk] virtual machine IP address is 10.xxx.yyy.zzz
2016/12/12 14:03:04 0 up-to-date, 7 changed, 0 failed
```

What is interesting to note in the output above is that Gru is
actually creating the Virtual Machines concurrently.

Since all dependencies have been satisfied by the time we have 
reached the Virtual Machine resources and the *VirtualMachine*
resource type itself is concurrent this allows Gru to schedule such
operations for concurrent execution.

This example shows one of the key features of the project - it's
ability to execute operations concurrently.

Hopefully this introduction gave you a nice overview on how to start
managing your virtual infrastructure using Gru.

Make sure to also check the
project at [Github](https://github.com/dnaeon/gru) for more
information and other code examples.
