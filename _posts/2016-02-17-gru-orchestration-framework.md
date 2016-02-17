---
layout: post
title: Creating an orchestration framework in Go
tags: golang programming orchestration
---
During the last few months or so I've been playing with
[Go](https://golang.org/) in my spare time in order to get myself
familiar with the language and build some experience with it.

People often learn easier by doing things and I am no exception in
that regard, so in order to get myself familiar with the language I
thought I'd need a project to work on, where I could practice my
skills and learn.

It was back then when I started working on my first Go project named
[Gru](https://github.com/dnaeon/gru), which is described as a simple
orchestration framework.

### TL;DR

[![asciicast](https://asciinema.org/a/35920.png)](https://asciinema.org/a/35920)

I must say that so far I've really enjoyed coding in Go.

The language itself tries to keep things simple and minimalistic,
which I liked a lot. This makes things much easier to understand and
learn even when you start with your first projects in Go.

Maybe because of my C background (or something else), but coding in Go
to me almost feels like home!

Most of my coding time recently has been occupied with
[Python](https://www.python.org/), which is another great language,
but there are areas when simply doing things in Python don't bring the
required performance due to the interpreted nature of the language.

On the other hand Go is well known for its concurrency primitives
called [goroutines](https://golang.org/ref/spec#Go_statements),
which makes things so easy to create concurrent applications.

I also like the way how easy it is to communicate between your
goroutines by using [channels](https://golang.org/ref/spec#Channel_types).

Of course in Go we still have the low-level synchronization pritimives
such as [sync.Mutex](https://golang.org/pkg/sync/#Mutex) which
provides a mechanism to manage access to shared resources using
locks.

[Interfaces](https://golang.org/doc/effective_go.html#interfaces_and_types)
in Go are neat too. Interfaces in Go are used to declare
a behaviour of an object, so they give you the layer of abstraction.

A type can then implement a given interface
(or multiple interfaces) by defining the required methods of the
interface types. Interfaces in Go are similar to the
[Traits in Rust](https://doc.rust-lang.org/book/traits.html), although
in Go an interface is implicitely implemented while in
[Rust](https://www.rust-lang.org/) they are explicit.

Of course there is so much more that can said about the language, but
probably that would be best done if I leave that for another post.

This post is about [Gru](https://github.com/dnaeon/gru) and how Gru
can be used to orchestrate your systems.

Gru is written in Go and is designed around two core
[interface types](https://golang.org/doc/effective_go.html#interfaces_and_types) -
a `minion.Minion` and `client.Client` interfaces.

A `Client` sends some tasks for processing and the `Minion` processes
them and returns some result. That simple. Well, maybe not that
simple, because Gru also gives you a way to better orchestrate your
environment based on various `classifiers` which a minion has, e.g.
send task to minions which run only on FreeBSD or GNU/Linux operating
systems, filter minions based on their architecture, etc.

Gru can also generate reports about your environment based on the
minion classifiers, so that you could quickly get an overview of your
environment. Of course Gru also keeps a log for each executed task,
which makes it easy for you to run audits and see what was done and
when in your environment.

Because Gru is built around Go interfaces we could use any backend
implementation for our minions and clients. We may chose to use
[AMQP](https://en.wikipedia.org/wiki/Advanced_Message_Queuing_Protocol)
and pass messages between our minions and clients and some SQL/NoSQL
backends for storing the results of our tasks. 

We may even implement the whole communication layer using
[ZeroMQ](http://zeromq.org/) and turn our minions into a distributed
system quite easily.

Eventually, it is up to the developers to pick an implementation of
Gru's interfaces, depending on their needs.

Currently Gru comes with an implementation of its core interface types
using [etcd](https://github.com/coreos/etcd), which is a distributed
key/value store. Tasks are being sent to and received by minions
from `etcd`. Using `etcd` also makes things easy in terms of discovery.

Getting started with Gru takes a few minutes. Literally. 

All you need to do is get [etcd](https://github.com/coreos/etcd) and
start your minions.

Get `etcd` and start it:

```bash
$ curl -O -L https://github.com/coreos/etcd/releases/download/v2.2.5/etcd-v2.2.5-linux-amd64.tar.gz
$ tar xzvf etcd-v2.2.5-linux-amd64.tar.gz
$ cd etcd-v2.2.5-linux-amd64
$ ./etcd
```

Next we install [Gru](https://github.com/dnaeon/gru):

```bash
$ go get -v github.com/dnaeon/gru
```

And now we can start our minions:

```bash
$ gructl serve
```

Everything in Gru is managed by the `gructl` tool. For instance
in order to send a task to all your minions and get their uptime
you would execute:

```bash
$ gructl run uptime
```

This would send the task to all registered minions. If we want to
send the task to specific set of minions we can use a
`classifier pattern` instead, e.g.:

```bash
$ gructl run --with-classifier os=freebsd uptime
```

Now this task will be sent only to minions which are running FreeBSD
as their Operating System.

And of course there are other things we can do with Gru, such as
generating reports based on minion classifiers, getting detailed info
about a minion, reviewing logs of previously executed tasks, etc.

And that was all for now about Gru. For more information about Gru,
please make sure to check the
[Github repository of Gru](https://github.com/dnaeon/gru).

Meanwhile I will continue coding in Go and build more
experience with the language.
