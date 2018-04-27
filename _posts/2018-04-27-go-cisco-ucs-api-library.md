---
layout: post
title: Go UCS API library
tags: programming go golang ucs api library bindings
---
As mentioned in a
[previous blog post](2018-01-12-convert-big-endian-uuid-to-middle-endian.md)
one of the past projects I've been involved in was an internal
[CMDB](https://en.wikipedia.org/wiki/Configuration_management_database)
system and the development of collection of Cisco UCS
Configuration Items (CIs) such as Fabric Interconnects, Chassis and
Blade servers.

The internal CMDB system is primarily written in the Go programming
language where collectors receive messages from a NATS queue whenever
something needs to be collected from remote systems and populate the
database.

At the time I started working on this project (November 2017)
there wasn't any Cisco UCS API library for Go, so I had to create one
first before I was going to start working on the collector itself.

The Cisco UCS Manager API uses XML as the payload for requests
and responses, and also provides
[XML Schema Definitions](https://en.wikipedia.org/wiki/XML_Schema_(W3C))
which describe each API call and it's respective
response.

The nice thing about XML Schema Definitions (XSD) is that you could
use it in order to generate client API packages.

Unfortunately the state of XSD code generation in Go is not in a good
shape and there are many projects which at this point which are no longer
maintained. And since we had a deadline to deliver some results
spending time on XSD code generation was not an option at that point,
so I had to write the Cisco UCS API library from scratch.

The library I've developed is now being released as open-source,
with the hope that it will be useful to someone else as well.

My experience with Cisco UCS API endpoints shows that a
UCS Manager is really sensitive about how frequently you
send requests to it, so the API library also ships with a
builtin [token bucket](https://en.wikipedia.org/wiki/Token_bucket)
rate limiter.

You can also find various examples in the repository which show
how to establish a connection to a remote Cisco UCS API endpoint
and retrieve various managed objects from it.

The library at this point primarily provides methods for fetching
data out of a Cisco UCS Manager API endpoint, so it doesn't
provide any methods for doing actual re-configuration of objects,
but the code itself can serve as a good base for someone if such a
need arises.

You can find the code in the [dnaeon/go-ucs](https://github.com/dnaeon/go-ucs)
repository at Github.

Hope you find it useful!
