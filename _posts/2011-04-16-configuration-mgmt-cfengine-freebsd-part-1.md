---
layout: post
title: Configuration Management on FreeBSD with CFEngine - Part I
tags: configuration management cfengine bsd
---

Administrating a couple hundreds of systems might easily become a
tedious tasks, even for the very experienced system administrators.

Keeping a mindset where you automate every possible aspect of an
Operating System and the tasks you do on it, is a must when you are
responsible for that many (or even more) systems.

The [FreeBSD Ports Collection](http://www.freebsd.org/doc/handbook/ports.html)
is a great thing to use when installing software distributions, but
installing multiple ports on several hundreds of machines is not a
pleasant tasks - it is time consuming, needs user attention, and not
to forget - things might blow up at some point.

Having central systems that take care of building the packages and
then distributing them to all systems in the domain, not only
saves the time needed to build a package, but also allows us to be
more flexible in the package deployment, in the sense of having
different environments - a test environment where we test the newly
built packages, which we can later safely deploy to our
production environment.

The following handbook is organized into different chapters, each of
them outlining the steps needed to setup a certain part of our environment.

The first chapter takes a look into
[Tinderbox](http://tinderbox.marcuscom.com/) - a system for testing
and building packages under FreeBSD.

We will be using Tinderbox for building our packages,
which will then be distributed to all the clients.

The second chapter is about [CFEngine 3](http://cfengine.org/), which
describes how to install and configure Cfengine 3 and put it's
configuration under revision control with [Git](http://git-scm.com/).

We will see how to automate a configuration deployment to our systems
with CFEngine 3, and also do package installations and upgrades.

In the last chapter we will take a look into different examples of
using CFEngine 3, and how to accomplish specific tasks with it.

## Requirements

* root access or sudo rights
* Apache web server with PHP support. If you do not have Apache
  installed, you may consider checking the
  [handbook for installing and configuring Apache](/node/15)
* FTP server for storing the Tinderbox packages. If you do not have an
  FTP server installed, you may want to check the
  [handbook for installing and configuring vsFTPd](/node/9)
* Git repository for storing the CFEngine 3 configuration.
  The following handbook explains [how to get Git installed on your
  FreeBSD system](/node/30), if you do not have Git installed already.
* MySQL database for Tinderbox.
  If you do not have a MySQL database, consider checking the
  [MySQL handbook](/node/2), which explains how to install and
  configure MySQL under FreeBSD.

## Installation of CFEngine

In this chapter we will take a look into CFEngine 3 - a configuration
management framework for configuring and maintaining UNIX and UNIX-like
systems.

Anything you can do on a system in general can be automated via
CFEngine 3, which makes it perfect for deploying mass changes to a
lot of systems.

CFEngine 3 is a suite of programs, each of them responsible for
executing a specific task.

* cf-agent - Active agent
* cf-execd - CFEngine Executor / Scheduler
* cf-know - Knowledge modeling agent
* cf-monitord - Passive monitoring agent
* cf-promises - Promise validator
* cf-runagent - Remote run agent
* cf-serverd - CFEngine Server
* cf-report - Self-knowledge extractor
* cf-key - Key generation tool
* cf-hub - Data aggregator

CFEngine 3 uses the client/server model, in which one (or more)
systems act as the master servers (also referred as the `policy servers`),
which keeps the promises and files which will be in turn
distributed to the clients (also referred as the `agents`).

Using CFEngine 3 you can keep your systems in a desired state.
When an agent is running it will try to conform to the promises it made,
in order to keep the system to the state we want it to be.

Since the following handbook is just a basic introduction to CFEngine 3
and it's capabilities, you are also advised to have a look at the
[online documentation of CFEngine 3](http://www.cfengine.org/pages/manual_guides)
for more in-depth explanation of Cfengine 3 and it's features.

Now let's go ahead and prepare our systems for CFEngine 3.

## Building the CFEngine 3 package

Now that we have a Tinderbox system installed and configured already
for us, for all of the package builds from now on, we are going to
make use of Tinderbox.

So, now go ahead and build your
[sysutils/cfengine3](http://www.freshports.org/sysutils/cfengine3/)
package in Tinderbox, as it was described in the previous chapter.

After building the package, we will use it in order to install it on the
CFEngine 3 master server, and it's clients.

The version of the CFEngine 3 package that we use in this handbook is
*cfengine-3.1.4_1*

We are also going to put the CFEngine 3 configuration under version control
with Git, so we will need the FreeBSD package for Git as well.

So build the [devel/git](http://www.freshports.org/devel/git/)
package in your Tinderbox as well, so we can later install it.

The version of Git we use in this handbook is *git-1.7.4.3*

## Preparing the CFEngine 3 master servers

In this step we will see how to install and prepare the CFEngine 3
master server, which is also referred to as the `policy server`.

This is the server which keeps the promises and files, which will be
in turn distributed to the clients, also referred to as the `agents`.

The CFEngine 3 master server will be installed on a FreeBSD jail.
So, the first step is to create a new FreeBSD jail, and then login to it.

I would also advise you on using
[sysutils/ezjail](http://www.freshports.org/sysutils/ezjail/) for
managing your FreeBSD jails, since it provides easy to use interface
to your FreeBSD jails.

Also, we will make use of the `ezjail flavours` later on so that
newly created jails will also be configured with CFEngine 3, once they
are installed.

For more information on
[sysutils/ezjail](http://www.freshports.org/sysutils/ezjail/), please
refer to the [official web page of ezjail](http://erdgeist.org/arts/software/ezjail/).

Using the `ezjail flavours` we can have different types of
configurations for different jails.

In the following handbook we will have at least two ezjail
flavours - one for the test environment and one for our production
environment.

Each of these ezjail flavours will in turn install the CFEngine 3
package we have built already, and the corresponding CFEngine 3
configuration for that environment.

Now, create the jails, which will be our CFEngine 3 policy servers.
In this handbook we will have two CFEngine 3 master servers - one for
the test environment, and one for our production environment.

```shell
$ sudo ezjail-admin create cfengine-test 192.168.0.1
$ sudo ezjail-admin create cfengine 192.168.0.2
```

For more information on the syntax of `ezjail-admin(1)`,
please refer to it's manual page.

## Installing the CFEngine 3 package

Now, that we have a FreeBSD jail, let's install the CFEngine 3
package that we have built already in Tinderbox.

```shell
$ sudo ezjail-admin console cfengine
```

In the Tinderbox chapter we mentioned something about transferring the
Tinderbox packages to a local FTP server using either a Tinderbox hook,
or a simple cron job that rsyncs our packages to the FTP server.

In the next command we are actually using the local FTP package
repository we have in place already.

So now let's install the Git and CFEngine 3 packages on our CFEngine 3
master server.

```shell
# setenv PACKAGEROOT ftp://ftp.example.org/
# pkg_add -r git-1.7.4.3
# pkg_add -r cfengine-3.1.4_1
```

The CFEngine 3 package installs in `/usr/local`, but CFEngine 3
needs to have a trusted work directory which is set to
`/var/cfengine` - this is also the place where the CFEngine promises
and files are stored.

But before creating the CFEngine work directory in `/var/cfengine`, we
are going to put all the CFEngine configuration under version control
with Git.

## Preparing CFEngine Git repository

Assuming you have read the [handbook for installing and configuring Git](/node/30)
on your FreeBSD system, we will now create the Git repository for
CFEngine on our Git server.

On the Git server:

```shell
$ cd /home/git/public && mkdir cfengine
$ cd cfengine && git init --bare --shared
```

Now we can go to the next chapter, where we'll start preparing the
basic CFEngine 3 configuration.
