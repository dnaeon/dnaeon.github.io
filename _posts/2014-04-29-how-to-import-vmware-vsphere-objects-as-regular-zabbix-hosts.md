---
layout: post
title: How to import VMware vSphere objects as regular Zabbix hosts
created: 1398776568
tags: zabbix monitoring virtualization vmware programming python
---
In a previous post we have seen how we can use Zabbix with
vPoller working together in order to perform monitoring of our
VMware vSphere environment.

The way we did it is by using
[vPoller](https://github.com/dnaeon/py-vpoller) in order to discover
VMware vSphere objects and then use the [Zabbix Low-level discovery
protocol](https://www.zabbix.com/documentation/2.2/manual/discovery/low_level_discovery)
in order to create hosts based on the discovered data.

While [Zabbix Low-level
discovery](https://www.zabbix.com/documentation/2.2/manual/discovery/low_level_discovery)
is a powerful feature of Zabbix which you could use in order to
automate the process of discovering and adding hosts to your Zabbix
server, it still has some limitations and disadvantages.

One disadvantage of using `Zabbix LLD` is that once a host is being
created by a `Zabbix Discovery Rule` that host becomes immutable - you
cannot manually change or update anything on the host, unless these
changes come from the discovery rule or the host profile applied to
the host.

You can imagine that this might be a bit of frustrating, especially
when you want to group your hosts in a better way, which obviously you
cannot do since this host is now immutable.

Adding additional templates to a discovered host is also not possible,
which is another big issue. Now that you've discovered your VMware
Virtual Machines you probably wanted to add some additional templates
to them, but you will soon discover that this is not possible either.

You cannot even add more interfaces to your hosts if needed... Like
mentioned earlier - your host is immutable, so that means no changes
at all.

And all these things are quite frustrating, at least to me, because
Zabbix does not allow me to manage my environment the way I want, the
way I believe I should be able to.

So, what can we do about it?

Well, we still can solve this issue. One way of solving this issue for
me was to create a tool which would take care of discovering and
importing discovered objects as regular Zabbix hosts. That way I get
all the advantages of regular Zabbix hosts - I can add the host to any
groups I want, adding more templates to my hosts, etc.

Now, let's see how to we can do that. We are going to use the
[zabbix-vsphere-import](https://github.com/dnaeon/py-vpoller/blob/master/src/zabbix/zabbix-vsphere-import)
tool in order to discover and import vSphere objects as regular hosts
into our Zabbix server.

The `zabbix-vsphere-import` tool uses `vPoller` for discovery of
vSphere objects, so make sure you have
[vPoller](https://github.com/dnaeon/py-vpoller) up and running first.

First, let's create the config file which `zabbix-vsphere-import` will
be using. Below is an example config file used by
`zabbix-vsphere-import`:

```yaml
---
vsphere:
  hostname: vc01.example.org

vpoller:
  endpoint: tcp://localhost:10123
  retries: 3
  timeout: 3000

zabbix:
  hostname: http://zabbix.example.org/zabbix
  username: Admin
  password: zabbix

  vsphere_object_host:
    proxy: zbx-proxy.example.org
    templates:
      - Template VMware vSphere Hypervisor - vPoller
    macros:
      VSPHERE_HOST: vc01.example.org
    groups:
      - Hypervisors

  vsphere_object_vm:
    templates:
      - Template VMware vSphere Virtual Machine - vPoller
    macros:
      VSPHERE_HOST: vc01.example.org
    groups:
      - Virtual Machines

  vsphere_object_datastore:
    templates:
      - Template VMware vSphere Datastore - vPoller
    macros:
      VSPHERE_HOST: vc01.example.org
    groups:
      - Datastores
```

In the example config file we have defined various config entries -
Zabbix server, vPoller settings and also templates to be applied for
the various vSphere objects, e.g. hosts, virtual machines and
datastores.

Time to import our vSphere objects as regular Zabbix hosts. To do that
simply execute the command below:

```bash
$ zabbix-vsphere-import -f /path/to/zabbix-vsphere-import.yaml
```

Once the import completes you should have your VMware hosts, virtual
machines and datastores imported to Zabbix as regular hosts and
already being monitored.

This is one way you could mimic the Zabbix Low-level discovery process
and import vSphere objects as regular hosts into your Zabbix server.

Generally you would want to run the import perhaps once an hour
(e.g. from `cron(8)`), so that your Zabbix server is in sync with your
vSphere environment.
