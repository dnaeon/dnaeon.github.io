---
layout: post
title: Wake On Lan in Common Lisp
tags: common-lisp lisp wake-on-lan wol
---
Back in the 90s IBM and Intel got together and came up with a
technology that is able to power on remote systems on the network by
sending a special packet known as the [magic
packet](https://en.wikipedia.org/wiki/Wake-on-LAN#Magic_packet).

This technology is now known as [Wake on Lan
(WoL)](https://en.wikipedia.org/wiki/Wake-on-LAN).

You can find various implementations of Wake On Lan in different
languages, e.g. C, Python, Go, Rust, etc.

However, there was no implementation of WoL in Common Lisp, so I've
decided to implement one. The [cl-wol](https://github.com/dnaeon/cl-wol)
repo provides three Common Lisp systems:

* `cl-wol.core` - provides the core functionallity for powering on
  remote systems using Wake on LAN (WoL) by broadcasting a magic
  packet to a destination port and address.
* `cl-wol.test` - provides the test suite for `cl-wol.core`
* `cl-wol.cli` - provides a command-line interface application, built
  on top of `cl-wol.core`, which comes with support for looking up
  hosts and their MAC addresses from a local
  [SQLite](https://www.sqlite.org/index.html) database.

Note, that if you are building the command-line application provided
by `cl-wol.cli` you will also need a recent version of
[clingon](https://github.com/dnaeon/clingon), because at the time of
writing this the `clingon` system is not the latest one you will find
in Quicklisp.

And here's a demo of the command-line application provided by the
`cl-wol.cli` system.

![_config.yml]({{ site.baseurl }}/images/cl-wol-demo.gif)

Make sure to check the [cl-wol](https://github.com/dnaeon/cl-wol) repo
for additional examples and information.
