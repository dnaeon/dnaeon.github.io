---
layout: post
title: vPoller -- VMware vSphere Distributed Poller written in Python
created: 1389533024
tags: python programming
---
vPoller is a distributed system written in Python for discoverying and
polling of vSphere Objects properties.

vPoller uses the [vSphere
API](https://www.vmware.com/support/developer/vc-sdk/) in order to
perform discovery and polling of Objects from a vSphere host
(e.g. VMware ESXi or VMware vCenter server instance).

The vPoller distributed system consists of a number of components,
each performing a different task:

* *vpoller-proxy* - A ZeroMQ proxy which load-balances client requests to a pool of workers
* *vpoller-worker* - A vSphere Worker, which does the actual polling and discovering from a vSphere host
* *vpoller-client* - A client program used for sending and receiving requests to a vSphere Worker

On the diagram below you can view the principal work of the *vPoller*
distributed system:

![_config.yml]({{ site.baseurl }}/images/vPoller-diagram.jpg)

The distributed nature of vPoller is provided by
[ZeroMQ](http://zeromq.org/), which also load balances client requests
to a pool of vSphere Workers.

`vPoller` was designed to be easy for integration into systems which
require access to vSphere Objects properties, but do not have native
support for it.

Possible scenarios where *vPoller* could be used is to integrate it
into monitoring systems as part of the polling process in order to
provide monitoring of your VMware vSphere environment. It could also
be used in applications for collecting statistics and other metrics
from your VMware vSphere environment.

vPoller can also be described as a VMware vSphere API-Proxy, because
it translates user requests to vSphere API requests thus allowing the
user to use the API without having the need to know how the vSphere
API works internally.

vPoller has been tested with vSphere 5.x and with very limited testing
on vSphere 4.x

In this article we will see how to install, configure and manage the
*vPoller* components.

For more and up-to-date information about *vPoller* you are advised to
also check the [vPoller documentation on
Github](https://github.com/dnaeon/py-vpoller), which contains more
comprehensive information about the components and their purpose.

Here we will focus on the actual usage of the *vPoller* distributed
system in order to perform discovery and polling of vSphere Object
properties.

## Requirements

* Python 2.7.x
* [vconnector](https://github.com/dnaeon/py-vconnector)
* [pysphere](https://code.google.com/p/pysphere/)
* [pyzmq](https://github.com/zeromq/pyzmq)
* [docopt](https://github.com/docopt/docopt)

The C client of vPoller also requires the following packages to be
installed in order to build it:

* Python development files (on Debian systems this is usually provided by the *python-dev* package)
* [ZeroMQ library](https://github.com/zeromq/zeromq3-x)

## Installation

First, make sure that you have all the requirements listed above
installed before starting with the installation of *vPoller*.

In order to install vPoller simply execute the command below:

```bash
$ sudo python setup.py install
```

We need to create a few directories as well, which are used by the
vPoller components to store their log files and lock files as well, so
make sure you create them first:

```bash
$ sudo mkdir /var/log/vpoller /var/run/vpoller
```

Now, go to the next section where we will see how to configure the
*vPoller* components.

## Configuration

First, let's configure the *vPoller Proxy*, which is component that
takes care of dispatching the client requests to our *vPoller Workers*
and does the automatic load-balancing using
[ZeroMQ](http://zeromq.org/).

The default configuration file of the vpoller-proxy resides in
`/etc/vpoller/vpoller-proxy.conf`, although you can specify a
different config file from the command-line as well.

Below is an example configuration file used by the `vpoller-proxy`:

```ini
[Default]
frontend = tcp://*:10123
backend  = tcp://*:10124
mgmt     = tcp://*:9999
```

Now, let's configure the *vPoller Workers*. The vPoller Worker is what
does the actual polling and discovering of vSphere objects from a
vSphere host.

Internally it runs the vSphere Agents, which take care of connecting
to a vSphere host, initiating the user's API requests and send back
the result to the client.

A vPoller Worker can connect to any number of vSphere hosts, where
each vSphere connection is handled by a separate vSphere Agent object.

In this example we will be running a `vPoller Worker` on two nodes of
our infrastructure in order to provide redundancy and
load-balancing. Each `vPoller Worker` will be connected to a single
VMware vCenter server instance from where we will be getting our
results.

The default configuration file of the vpoller-worker resides in
`/etc/vpoller/vpoller-worker.conf`, although you can specify a
different config file from the command-line as well.

Below is an example configuration file used by the vpoller-worker:

```ini
[Default]
proxy             = tcp://<hostname-of-vpoller-proxy>:10124
mgmt              = tcp://*:10000
vsphere_hosts_dir = /etc/vpoller/vsphere
```

Now, it is time to configure the `vSphere Agents` of our `vPoller
Worker`. A vSphere Agent is what takes care of establishing a
connection to the vSphere host and perform any poll or discovery
operations. For each vSphere host you want to use you need a
configuration file describing the connection details.

Below is an example configuration file
`/etc/vpoller/vsphere/my-vc0.conf`, which describes the connection
details to our vCenter server.

```ini
[Default]
hostname = vc01-test.example.org
username = root
password = myp4ssw0rd
timeout  = 3
```

You should take care of securing the files as well, as they contain
the password in plain text.

And that was it with the configuration, now let's go ahead and start
up our `vPoller Proxy` and `Workers`.

## Starting services

Once you configure the `vPoller Proxy` and `vPoller Worker` we can go
ahead and start them up.

In order to start `vPoller Proxy`, login to your `vPoller Proxy` node
and execute the command below.

```bash
$ vpoller-proxy start
```

You might also consider using the init.d scripts from the [vPoller
Github repository](https://github.com/dnaeon/py-vpoller), in which
case you could start it using `service(8)`.

```bash
$ sudo service vpoller-proxy start
```

Now, let's start the `vPoller Workers`. Login to each of your `vPoller
Worker` nodes and execute the command below:

```bash
$ vpoller-worker start
```

You might also consider using the init.d scripts from the [vPoller
Github repository](https://github.com/dnaeon/py-vpoller), in which
case you could start it using `service(8)`.

```bash
$ sudo service vpoller-worker start
```

Check the log files at `/var/log/vpoller` which should indicate that
the components started up successfully or contain errors in case
something went wrong.

## Using vPoller

The examples below show how you can use *vPoller* in order to perform
discovery and polling of VMware vSphere environment.

When we send a request to our *vPoller Workers* we usually send the
*method* to be performed (e.g. discovery or polling) and the vSphere
Object property name.

You can find the available vSphere Object property names you can use
at the [vSphere API online
documentation](https://www.vmware.com/support/developer/vc-sdk/).

## Discovering ESXi hosts on a vCenter server

Time to test out *vPoller*. In this example we will see how we can
discover all ESXi hosts, which are already registered to our VMware
vCenter server instance.

We will be using `vpoller-client` tool in order to send requests to
our `vPoller Workers`.

For more information about the different command-line options
supported by `vpoller-client`, execute the command below to get usage
information.

```bash
$ vpoller-client --help
```

Our `vPoller Proxy` endpoint is `tcp://localhost:10123`.

Considering that the vPoller Proxy endpoint is at
`tcp://localhost:10123` in order to discover the ESXi hosts on vCenter
*vc01-test.example.org* you would execute a similar command:

On the screenshot below you can see how we send a request to our
`vPoller Proxy`, which in turn is dispatched to the `vPoller Workers`
connected to the Proxy. Once the request is processed by our `vPoller
Workers` result is sent back to the client in JSON format.

![_config.yml]({{ site.baseurl }}/images/vpoller-discover-esxi-hosts.jpg)

The returned result shows that we have only a single ESXi host
registered to our vCenter server instance.

In the next examples we will use the information that we've discovered
already in order to perform polling of Object properties.

## Discovering Datastores on a vCenter server

Now, let's see how we can discover all datastores on our vCenter
server instance.

On the screenshot below you can see the request we send to our
`vPoller Proxy` and the result returned to the client from the
`vPoller Workers`.

![_config.yml]({{ site.baseurl }}/images/vpoller-discover-datastores.jpg)

The returned result shows us all datastores that we have on our
vCenter server instance.

In the next examples we will use the information that we've discovered
already in order to perform polling of Object properties.

## Polling vSphere Object Properties

Let's see now how we can perform polling of vSphere Object
properties. By using the information we've got from discoverying the
ESXi hosts and datastores from our vCenter we will now send a request
for different properties of our vSphere Objects.

Example request to get the power state of an ESXi host. The property
we want to retrieve is `runtime.powerState`.

![_config.yml]({{ site.baseurl }}/images/vpoller-poll-esxi-powerstate.jpg)

What amout of memory do we have on our ESXi host? The property we want
to retrieve is `hardware.memorySize`.

![_config.yml]({{ site.baseurl }}/images/vpoller-poll-esxi-memorysize.jpg)

Let's see what was the time our ESXi hosts booted. The property we
want to retrieve is `runtime.bootTime`.

![_config.yml]({{ site.baseurl }}/images/vpoller-poll-esxi-boottime.jpg)

Okay, let's poll some properties now about datastores. Using the
discovery information we got previously we will now see how to get
various information about our datastores.

What is the capacity of our datastore? The property we want to
retrieve is `summary.capacity`.

![_config.yml]({{ site.baseurl }}/images/vpoller-poll-datastore-capacity.jpg)

What is the name of our datastore? The property we want ot retrieve is
`info.name`.

![_config.yml]({{ site.baseurl }}/images/vpoller-poll-datastore-name.jpg)

Let's see the amount of free space we have on our datastore. The
property we want to retrieve is `info.freeSpace`.

![_config.yml]({{ site.baseurl }}/images/vpoller-poll-datastore-freespace.jpg)

Again you are advised to check the [vSphere API online
documentation](https://www.vmware.com/support/developer/vc-sdk/) for
more information about the object property names you can use.

## Using the management interface of vPoller

At any time you can request status information from your `vPoller
Proxy` or `Worker` by sending a request to the management socket of
your `Proxy` or `Worker`.

This is how you could get status information from your `vPoller
Proxy`:

![_config.yml]({{ site.baseurl }}/images/vpoller-proxy-status.jpg)

And this is how you could get status information from your `vPoller
Workers`:

![_config.yml]({{ site.baseurl }}/images/vpoller-worker-status_0.jpg)

The management interface of `vPoller Proxy` and `Worker` also accepts
commands for shutting down the components.

This is how you could shutdown your `vPoller Proxy` by sending a
`shutdown` message to your node:

![_config.yml]({{ site.baseurl }}/images/vpoller-proxy-shutdown.jpg)

And this is how you could shutdown your `vPoller Worker` by sending a
`shutdown` message to your nodes:

![_config.yml]({{ site.baseurl }}/images/vpoller-worker-shutdown.jpg)

You can also perform these operations using the `init.d` scripts from
the [vPoller Github repository](https://github.com/dnaeon/py-vpoller).

## vPoller Helpers

The *vPoller Helpers* were implemented in order to provide an easy way
for connecting your applications to vPoller.

The result messages returned by a vPoller Worker are always in JSON
format.

This could be okay for most applications, which require to process a
result message, but in some cases you might want to receive the result
in different formats.

By using the vPoller Helpers you are able to convert any result
messages to a format which your application can understand and
use. This is useful for presenting the information in a way which you
can feed it to your application.

An example of such a vPoller Helper is the [Zabbix vPoller Helper
module](https://github.com/dnaeon/py-vpoller/tree/master/src/vpoller/helpers),
which can translate a result message to [Zabbix LLD
format](https://www.zabbix.com/documentation/2.2/manual/discovery/low_level_discovery)
and return property values ready to be used in Zabbix items.

Let's see how the discovery of datastores looks like if we the *Zabbix
vPoller Helper*:

![_config.yml]({{ site.baseurl }}/images/vpoller-zabbix-helper-discover-datastores.jpg)

The result is returned in [Zabbix LLD
format](https://www.zabbix.com/documentation/2.2/manual/discovery/low_level_discovery)
format which is useful for creating *discovery items* in Zabbix.

This is how things look like if we try to poll a property using the
`vpoller.helpers.zabbix` helper:

![_config.yml]({{ site.baseurl }}/images/vpoller-zabbix-helper-datastore-poll.jpg)

As you can see we only got the result value of our property which
makes it useful for using in items, which require a single value to be
returned to Zabbix.

Examples of other *vPoller Helpers* could be an HTML vPoller Helper,
which returns the result in HTML format in order to present the
information nicely in a web browser.

## Further reading

This was just an introduction to *vPoller* and what it can do. For
more and up-to-date information about *vPoller* you are advised to
check the [vPoller documentation on
Github](https://github.com/dnaeon/py-vpoller).

In case you experience any bug issues, please report them to the
[vPoller issue tracker on
Github](https://github.com/dnaeon/py-vpoller/issues).
