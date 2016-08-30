---
layout: post
title: Puppet vs Gru - Benchmarking Speed & Concurrency
tags: configuration management golang lua programming gru puppet
---
Traditional configuration management systems perform operations on the
target systems they manage after doing
[topological sorting](https://en.wikipedia.org/wiki/Topological_sorting)
of the operations that need to be performed.

The operations that need to be performed on the systems are usually
described as a
[directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph),
which is also known as a
[dependency graph](https://en.wikipedia.org/wiki/Dependency_graph).

The result of a successful topological sort gives the proper order
of execution for the operations that need to be performed, that is
which operation gets executed first, which is next, and so on.

In case of any errors while doing a topological sort they are also
able to determine and pinpoint
[circular dependencies](https://en.wikipedia.org/wiki/Dependency_graph)
in the graph, which indicates an impossible evaluation of the
dependency graph.

Successful topological sort of the graph often results in a
serialized or sequential execution of operations, where only one
operation gets executed at any given time.

As you might guess, the more operations we have to perform results in
more time required for the whole execution to complete.

In this post we will see how two different configuration management
systems perform operations in terms of speed and concurrency.

First on the list is [Puppet](https://puppet.com/) - a very popular
and well established configuration management system with a rich
library of features and third party modules available at the
[Puppet Forge](https://forge.puppet.com/).

The second one is [Gru](https://github.com/dnaeon/gru) - a relatively
new project, described as a distributed orchestration & configuration
management framework, which is written in [Go](https://golang.org/) and
uses [Lua](https://www.lua.org/) as the DSL language.

The focus of Gru is to be a simple, fast and concurrent framework for
doing orchestration and configuration management.

While tradional configuration management systems execute operations
sequentially, Gru tries it's best to determine which operations can be
executed concurrently, thus reducing the overall amount of time,
required for an execution run to complete.

In this post we will see how both systems compare to each other in
terms of speed and concurrency by looking into some examples.

The system that was used for the tests below has 2 CPUs, 4GB of memory
and is running CentOS Linux release 7.2.1511 (Core).

This is the version of Puppet that was used.

```bash
$ puppet --version
4.6.1
```

And this is the version of Gru.

```bash
$ gructl --version
gructl version 0.4.0
```

Let's start off with a simple resource for managing a single file.

This is the Puppet code that we use for this test. Note that we don't
use any classes or modules here in order to keep things simple.

```puppet
file { '/tmp/foo':
  ensure => 'present',
  owner  => 'root',
  group  => 'root',
}
```

The same configuration written in the Lua-based DSL of Gru looks like
this.

```lua
-- Create a new file resource
foo = resource.file.new("/tmp/foo")
foo.state = "present"
foo.owner = "root"
foo.group = "root"

-- Add the resource to the catalog
catalog:add(foo)
```

Measuring the Puppet run gives us the following result.

```bash
$ time puppet apply --verbose init.pp
Notice: Compiled catalog for <fqdn> in environment production in 0.12 seconds
Info: Applying configuration version '1472481236'
Notice: /Stage[main]/Main/File[/tmp/foo]/ensure: created
Notice: Applied catalog in 0.02 seconds

real    0m2.403s
user    0m2.171s
sys     0m0.166s
```

Running the Gru code gives us the following result.

```bash
$ time gructl apply site.lua
2016/08/29 17:34:39 Loaded 1 resources
2016/08/29 17:34:39 Starting 2 goroutines for concurrent processing
2016/08/29 17:34:39 file[/tmp/foo] is concurrent
2016/08/29 17:34:39 file[/tmp/foo] is absent, should be present
2016/08/29 17:34:39 file[/tmp/foo] creating resource
2016/08/29 17:34:39 Applied 1 resources, 0 of which have failed and 1 have succeeded

real    0m0.009s
user    0m0.005s
sys     0m0.006s
```

This was a simple test, but it does show one of the key features of
Gru - it is fast, and it is concurrent.

The next test extends upon the previous example by adding a few
more resources.

The Puppet code that we use for the next test relies on the
[puppetlabs/stdlib](https://github.com/puppetlabs/puppetlabs-stdlib)
module, from which we use the `range` function.

```bash
$ puppet module install puppetlabs/stdlib
Notice: Preparing to install into /etc/puppetlabs/code/environments/production/modules ...
Notice: Downloading from https://forgeapi.puppetlabs.com ...
Notice: Installing -- do not interrupt ...
/etc/puppetlabs/code/environments/production/modules
└── puppetlabs-stdlib (v4.12.0)
```

The Puppet code below creates resources for managing five files.

```puppet
range('1', '5').each |$i| {
  file { "/tmp/foo${i}":
    ensure => 'present',
    owner  => 'root',
    group  => 'root',
  }
}
```

And the Lua code that we use for the Gru module looks like this.

```lua
for i=1, 5 do
   f = resource.file.new("/tmp/foo" .. i)
   f.state = "present"
   f.owner = "root"
   f.group = "root"
   catalog:add(f)
end
```

Running the Puppet code produces the following results.

```bash
$ time puppet apply --verbose init.pp
Info: Loading facts
Notice: Compiled catalog for <fqdn> in environment production in 0.13 seconds
Info: Applying configuration version '1472541787'
Notice: /Stage[main]/Main/File[/tmp/foo1]/ensure: created
Notice: /Stage[main]/Main/File[/tmp/foo2]/ensure: created
Notice: /Stage[main]/Main/File[/tmp/foo3]/ensure: created
Notice: /Stage[main]/Main/File[/tmp/foo4]/ensure: created
Notice: /Stage[main]/Main/File[/tmp/foo5]/ensure: created
Notice: Applied catalog in 0.05 seconds

real    0m4.017s
user    0m3.085s
sys     0m0.266s
```
And this is the result of running the Lua code from Gru.

```bash
$ time gructl apply site.lua
2016/08/30 10:24:52 Loaded 5 resources
2016/08/30 10:24:52 Starting 2 goroutines for concurrent processing
2016/08/30 10:24:52 file[/tmp/foo2] is concurrent
2016/08/30 10:24:52 file[/tmp/foo1] is concurrent
2016/08/30 10:24:52 file[/tmp/foo2] is absent, should be present
2016/08/30 10:24:52 file[/tmp/foo2] creating resource
2016/08/30 10:24:52 file[/tmp/foo1] is absent, should be present
2016/08/30 10:24:52 file[/tmp/foo1] creating resource
2016/08/30 10:24:52 file[/tmp/foo3] is concurrent
2016/08/30 10:24:52 file[/tmp/foo3] is absent, should be present
2016/08/30 10:24:52 file[/tmp/foo3] creating resource
2016/08/30 10:24:52 file[/tmp/foo4] is concurrent
2016/08/30 10:24:52 file[/tmp/foo4] is absent, should be present
2016/08/30 10:24:52 file[/tmp/foo4] creating resource
2016/08/30 10:24:52 file[/tmp/foo5] is concurrent
2016/08/30 10:24:52 file[/tmp/foo5] is absent, should be present
2016/08/30 10:24:52 file[/tmp/foo5] creating resource
2016/08/30 10:24:52 Applied 5 resources, 0 of which have failed and 5 have succeeded

real    0m0.010s
user    0m0.009s
sys     0m0.005s
```

One thing to note here, is that the file resources in this Lua code
are concurrent. That is because the
[file resource](https://godoc.org/github.com/dnaeon/gru/resource#File)
has support for concurrency of multiple instances of the same
type and since these resources have no forward or reverse
dependencies in the dependency graph, they are scheduled for concurrent
execution by Gru.

In this particular example it wouldn't matter much if the
resources were concurrent or not, since the time required to
execute the same operation in a serialized way would result in the
same amount of time, but it does show one of the key strengths of Gru -
its support for concurrent execution of operations.

You can read more about how Gru determines whether a resource is a
good candidate for concurrent execution in the
[catalog package](https://github.com/dnaeon/gru/tree/master/catalog).

Another resource which supports concurrent execution of multiple
instances from the same type is the
[shell resource](https://godoc.org/github.com/dnaeon/gru/resource#Shell).

In order to illustrate better the concurrency features of Gru, let's
see another example, which will create five resources, each sleeping
for five seconds. We will use the `sleep` command in order to
simulate some work being done by our resources.

```puppet
range('1', '5').each |$i| {
  $cmd = "command-${i}"
  notify { "running ${cmd}": }
  exec { $cmd:
    command => '/usr/bin/sleep 5',
  }
}
```

The above Puppet code creates five `exec` resources, each sleeping
for five seconds.

And this is the Lua code that we use for the Gru module.

```lua
for i=1, 5 do
   s = resource.shell.new("sleep-command-" .. i)
   s.command = "/usr/bin/sleep 5"
   catalog:add(s)
end
```

Running the Puppet code produces the following result.

```bash
$ time puppet apply --verbose init.pp
Info: Loading facts
Notice: Compiled catalog for <fqdn> in environment production in 0.20 seconds
Info: Applying configuration version '1472544399'
Notice: running command-1
Notice: /Stage[main]/Main/Notify[running command-1]/message: defined 'message' as 'running command-1'
Notice: /Stage[main]/Main/Exec[command-1]/returns: executed successfully
Notice: running command-2
Notice: /Stage[main]/Main/Notify[running command-2]/message: defined 'message' as 'running command-2'
Notice: /Stage[main]/Main/Exec[command-2]/returns: executed successfully
Notice: running command-3
Notice: /Stage[main]/Main/Notify[running command-3]/message: defined 'message' as 'running command-3'
Notice: /Stage[main]/Main/Exec[command-3]/returns: executed successfully
Notice: running command-4
Notice: /Stage[main]/Main/Notify[running command-4]/message: defined 'message' as 'running command-4'
Notice: /Stage[main]/Main/Exec[command-4]/returns: executed successfully
Notice: running command-5
Notice: /Stage[main]/Main/Notify[running command-5]/message: defined 'message' as 'running command-5'
Notice: /Stage[main]/Main/Exec[command-5]/returns: executed successfully
Notice: Applied catalog in 25.09 seconds

real    0m28.943s
user    0m3.317s
sys     0m0.275s
```

Due to the way that Puppet executes resources in a serialized way, this
results in at least a 25 seconds execution run as we can see from the
output above.

And this is the result of running the Lua module using Gru.

```bash
$ time gructl apply site.lua
2016/08/30 11:08:49 Loaded 5 resources
2016/08/30 11:08:49 Starting 2 goroutines for concurrent processing
2016/08/30 11:08:49 shell[sleep-command-2] is concurrent
2016/08/30 11:08:49 shell[sleep-command-2] is absent, should be present
2016/08/30 11:08:49 shell[sleep-command-2] executing command
2016/08/30 11:08:49 shell[sleep-command-3] is concurrent
2016/08/30 11:08:49 shell[sleep-command-3] is absent, should be present
2016/08/30 11:08:49 shell[sleep-command-3] executing command
2016/08/30 11:08:54 shell[sleep-command-2]
2016/08/30 11:08:54 shell[sleep-command-4] is concurrent
2016/08/30 11:08:54 shell[sleep-command-4] is absent, should be present
2016/08/30 11:08:54 shell[sleep-command-4] executing command
2016/08/30 11:08:54 shell[sleep-command-3]
2016/08/30 11:08:54 shell[sleep-command-5] is concurrent
2016/08/30 11:08:54 shell[sleep-command-5] is absent, should be present
2016/08/30 11:08:54 shell[sleep-command-5] executing command
2016/08/30 11:08:59 shell[sleep-command-4]
2016/08/30 11:08:59 shell[sleep-command-1] is concurrent
2016/08/30 11:08:59 shell[sleep-command-1] is absent, should be present
2016/08/30 11:08:59 shell[sleep-command-1] executing command
2016/08/30 11:08:59 shell[sleep-command-5]
2016/08/30 11:09:04 shell[sleep-command-1]
2016/08/30 11:09:04 Applied 5 resources, 0 of which have failed and 5 have succeeded

real    0m15.014s
user    0m0.007s
sys     0m0.015s
```

The output above shows that Gru has used only two
[goroutines](https://golang.org/doc/effective_go.html#goroutines) for
executing concurrent resources. This results in a reduced
overall time for the whole execution run compared to the Puppet
run, but still this is a lot of time and we can do much better than
that.

Raising the concurrency level means we will have more
goroutines processing resources concurrently. Let's do that now
and run our Lua code again.

In order to set the concurrency level we use the `--concurrency`
flag of the `gructl apply` command.

```bash
$ time gructl apply --concurrency=10 site.lua
2016/08/30 11:15:39 Loaded 5 resources
2016/08/30 11:15:39 Starting 10 goroutines for concurrent processing
2016/08/30 11:15:39 shell[sleep-command-2] is concurrent
2016/08/30 11:15:39 shell[sleep-command-5] is concurrent
2016/08/30 11:15:39 shell[sleep-command-2] is absent, should be present
2016/08/30 11:15:39 shell[sleep-command-2] executing command
2016/08/30 11:15:39 shell[sleep-command-5] is absent, should be present
2016/08/30 11:15:39 shell[sleep-command-5] executing command
2016/08/30 11:15:39 shell[sleep-command-3] is concurrent
2016/08/30 11:15:39 shell[sleep-command-3] is absent, should be present
2016/08/30 11:15:39 shell[sleep-command-3] executing command
2016/08/30 11:15:39 shell[sleep-command-4] is concurrent
2016/08/30 11:15:39 shell[sleep-command-1] is concurrent
2016/08/30 11:15:39 shell[sleep-command-4] is absent, should be present
2016/08/30 11:15:39 shell[sleep-command-4] executing command
2016/08/30 11:15:39 shell[sleep-command-1] is absent, should be present
2016/08/30 11:15:39 shell[sleep-command-1] executing command
2016/08/30 11:15:44 shell[sleep-command-3]
2016/08/30 11:15:44 shell[sleep-command-2]
2016/08/30 11:15:44 shell[sleep-command-4]
2016/08/30 11:15:44 shell[sleep-command-1]
2016/08/30 11:15:44 shell[sleep-command-5]
2016/08/30 11:15:44 Applied 5 resources, 0 of which have failed and 5 have succeeded

real    0m5.022s
user    0m0.006s
sys     0m0.012s
```

As you can see from the output above now we have enough goroutines to
handle all of the resources at the same time, which results in much
less time required to complete the whole execution run.

Hopefully these short examples gave you an idea of how Gru can help
you on the configuration management side of things by providing you a
fast and concurrent orchestration and configuration management
framework.

Of course configuration management is not just about speed and
concurrency. While Gru clearly outperforms Puppet in terms of
speed and concurrency support, in other areas Puppet stands much
better - and that is the number of features and third party modules.

If you like what you've seen about Gru so far and want to see it evolve
further, please consider contributing to the
[project at Github](https://github.com/dnaeon/gru) by suggesting new
features, sending patches or reporting bugs.
