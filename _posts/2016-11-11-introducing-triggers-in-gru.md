---
layout: post
title: Introducing triggers in Gru orchestration framework
tags: configuration management golang lua programming gru triggers
---
Latest version of
[Gru orchestration framework](https://github.com/dnaeon/gru) ships
with a new feature called *triggers*.

Triggers allow for a resource to subscribe for changes in other
resources and react to them by executing Lua code.

An example scenario of where triggers could be helpful is having a
package resource and a service resource. If the package resource
state has changed (e.g. package has been upgraded) you would want to
notify the service to perform a full restart, but if only the
configuration file has changed you might want to reload the service,
rather than restarting it.

Support for triggers has been implemented as part of
[issue #33](https://github.com/dnaeon/gru/issues/33), where you can
also find more details about the initial implementation.

So how do triggers work?

In order for a resource to be able to determine whether a
resource to which it is subscribed to has changed, we need to make
sure that the resource we subscribe to is executed first. We do this,
so that we can ensure that by the time our resource is scheduled for
execution we would know the state of the resource to which we
subscribe to.

In other words this means that we need to create an
[edge](https://en.wikipedia.org/wiki/Graph_theory) between the
graph nodes in the resulting
[DAG graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) of our
resources.

Consider the following example code, which creates a couple of
resources.

```lua
foo = resource.file.new("/tmp/foo")
foo.state = "present"

bar = resource.file.new("/tmp/bar")
bar.state = "present"

catalog:add(foo, bar)
```

The resources above do not have any direct relations between each
other and we can confirm that by looking at the resulting resource
DAG graph.

```bash
$ gructl graph site/code/foo-bar.lua | dot -Tpng -O
```

The resulting graph looks like this.

[![]({{ site.baseurl }}/images/gru-foo-bar-dag.png)]({{ site.baseurl }}/images/gru-foo-bar-dag.png){:.glightbox}

Since `file` is a concurrent resource type once we run this code
we would see the two resources being executed concurrently.

```bash
$ gructl apply site/code/foo-bar.lua
2016/11/11 12:55:46 Loaded 2 resources
2016/11/11 12:55:46 Starting 4 goroutines for concurrent processing
2016/11/11 12:55:46 file[/tmp/foo] is concurrent
2016/11/11 12:55:46 file[/tmp/foo] is absent, should be present
2016/11/11 12:55:46 file[/tmp/foo] creating file
2016/11/11 12:55:46 file[/tmp/bar] is concurrent
2016/11/11 12:55:46 file[/tmp/bar] is absent, should be present
2016/11/11 12:55:46 file[/tmp/bar] creating file
2016/11/11 12:55:46 0 up-to-date, 2 changed, 0 failed
```

Now, suppose that `bar` wants to subscribe for changes in `foo` and
execute something if that happens. Here's where triggers come in
place.

In order to do that we will add the following Lua code to the initial
example we've used so far.

```lua
bar.subscribe[foo:ID()] = function()
   print("This is bar, just letting you know that foo has changed")
end
```

Remeber that I've mentioned that subscribing to resources creates
edges in the resource graph?

```bash
$ gructl graph site/code/foo-bar.lua | dot -Tpng -O
```

The new graph representation looks like this.

[![]({{ site.baseurl }}/images/gru-foo-bar-dag-v2.png)]({{ site.baseurl }}/images/gru-foo-bar-dag-v2.png){:.glightbox}

Here's another, more real-world example of using triggers in
resources.

The following Lua code creates a few resources for managing the SNMP
service on a GNU/Linux system - it creates resources for managing the
package, configuration file and service of the SNMP daemon.

```lua
-- Manage the SNMP package
pkg = resource.package.new("net-snmp")
pkg.state = "present"

-- Manage the config file for SNMP daemon
config = resource.file.new("/etc/snmp/snmpd.conf")
config.state = "present"
config.content = "rocommunity public"
config.require = { pkg:ID() }

-- Manage the SNMP service
svc = resource.service.new("snmpd")
svc.state = "running"
svc.enable = true
svc.require = { pkg:ID(), config:ID() }

-- Subscribe for changes in the config file resource.
-- Reload the SNMP daemon service if the config file has changed.
svc.subscribe[config:ID()] = function()
   os.execute("systemctl reload snmpd")
end

-- Subscribe for changes in the package resource.
-- Restart the SNMP daemon service if the package has changed.
svc.subscribe[pkg:ID()] = function()
   os.execute("systemctl restart snmpd")
end

-- Add resources to the catalog
catalog:add(pkg, config, svc)
```

The resource dependency graph of the above code looks like this.

[![]({{ site.baseurl }}/images/gru-snmpd-dag.png)]({{ site.baseurl }}/images/gru-snmpd-dag.png){:.glightbox}

What is worth noticing in the above example is that now we use
triggers for the service in a way that any changes in the
package resource (e.g. upgrade of the package) would trigger a
service restart, while a configuration file change would trigger a
service reload.

Make sure to check out the
[Gru project at Github](https://github.com/dnaeon/gru) for more
information about the project and other examples.
