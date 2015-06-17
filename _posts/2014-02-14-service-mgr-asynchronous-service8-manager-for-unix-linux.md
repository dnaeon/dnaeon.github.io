---
layout: post
title: service-mgr -- Asynchronous service(8) manager for UNIX/Linux
created: 1392384752
tags: python programming
---
Another project I've been playing with recently is the
`Asynchronous Service Manager`.

The `Asynchronous Service Manager` as it's name suggests is a
`service(8)` manager for UNIX/Linux systems. It allows you to remotely
manage and control the services of your UNIX/Linux systems in
asynchronous way.

By design the `Asynchronous Service Manager` is a distributed system,
which uses the [Publish-subscribe messaging
pattern](http://en.wikipedia.org/wiki/Publish%E2%80%93subscribe_pattern).

The `Service Manager` was built in Python and uses the
[ZeroMQ](http://zeromq.org/) messaging library.

The diagram below shows the typical message flow of a service request:

![_config.yml]({{ site.baseurl }}/images/async-service-mgr.jpg)

With `Service Manager` you can manage various UNIX/Linux system
services without having to worry about the underlying methods for
managing a service. By doing that it provides you with a standard
interface for managing services on various systems, e.g. GNU/Linux,
BSD, etc.

For more information on `Service Manager`, how to install and
configure it please refer to the [Service Manager Github
repository](https://github.com/dnaeon/async-service-mgr)
