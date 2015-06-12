---
layout: post
title: Configuration Management on FreeBSD with CFEngine - Part IV
tags: configuration management cfengine bsd
---

If you've followed the handbook by this step, that means that now you
have a basic CFEngine 3 policy server and clients already
installed and configured.

But let's do something useful with CFEngine.
In this and the next chapters you will find various CFEngine example
configurations, which purpose is only to show you how and what you can
do with CFEngine.

The presented here examples do basic things, but after reading the
next chapters you will get the idea, of how you can extend them
to more complex and useful ones.

## Domain configuration with CFEngine

In this example we will see how we can use CFEngine for configuring
various settings of our domains.

In the example shown here, we will add one additional CFEngine
configuration file, which will take care of configuring
various settings for our domains.

For the purposes of the example we will have a fictional network, that
is spreaded accross multiple sites, each site having it's own domain.

First we start by updating our main configuration file - `promises.cf`.

```text
#####################################################
#                                                   #
# promises.cf - Main CFEngine configuration file    #
#                                                   #
#####################################################
 
body common control {
 
    any::
 
        bundlesequence => { @(g.bundlesequence) };

    site1::

        domain => "site1.example.org";

    site2::

        domain => "site2.example.org";
 
    any:: 
 
        inputs => {
            "update.cf",
            "library.cf",
            "classes.cf",
            "cf-execd.cf",
            "cf-serverd.cf",
            "cf-report.cf",
            "domain.cf",
            "cleanup.cf"
        };
 
    output_prefix => "cf3>";
}
 
# global vars
bundle common g {
 
vars:
 
    "workdir"           string => "/var/cfengine";
    "masterfiles"       string => "$(workdir)/masterfiles";
    "inputfiles"        string => "$(workdir)/inputs";
    "policyhost"        string => "cfengine.example.org";
    "bundlesequence"    slist  => { "update", "executor", "server", "domain", "cleanup" };
}
 
body runagent control {
    hosts => { "127.0.0.1", "10.1.16.0/20" };
}
```

In `promises.cf` we have added a few things.

First we have defined the domain names of the two fictional sites
we have - `site1` and `site2`.

`site1` and `site2` here are just user defined classes, which we will
define in our `classes.cf` file.

We have also added one new configuration file - `domain.cf`

And finally we've updated the global bundlesequence, so that now it
contains one new bundle - `domain`.

Now let's define the site classes. The site classes will contain
all the systems from that site.

For example if we have a system called `www.site1.example.org`,
that means that the `site1` class will be defined for that system.

Classes are just a context in CFEngine, remember?

Now let's update our `classes.cf` to include our sites.

```text
#################################################
#                                               #
# classes.cf - CFEngine user-defined classes    #
#                                               #
#################################################

bundle common myclasses {

vars:

    "sysctl_jailed" string => execresult("/sbin/sysctl -n security.jail.jailed", "noshell");

classes:

    "freebsd_jail" expression => strcmp("$(sysctl_jailed)", "1");
    "freebsd_host" expression => strcmp("$(sysctl_jailed)", "0");

    "site1" or => {
        classify("www.site1.example.org"),
        classify("www-proxy.site1.example.org"),
        classify("svn.site1.example.org"),
        classify("pkgmaster.site1.example.org"),
        classify("mysql.site1.example.org"),
        classify("monitor.site1.example.org"),
        classify("jail-test.site1.example.org")
    };

    "site2" or => {
        classify("webserver.site2.example.org"),
        classify("mail.site2.example.org"),
        classify("squid.site2.example.org")
    };

    "policy_servers" or => {
        classify("$(g.policyhost)")
    };
}
```

In `classes.cf` we now have two new classes defined, each
containing the systems of the two fictional sites we have.

Now it is time to actually do something with CFEngine and that will be
done by the bundle `domain`, which we define in the `domain.cf`
configuration file.

```text
#############################################
#                                           #
# domain.cf - Domain specific configuration #
#                                           #
#############################################

bundle agent domain { 

files:

    "/etc/rc.conf"

        comment     => "Copy rc.conf file for $(sys.fqhost)",
        create      => "true",
        perms       => mog("0644", "root", "wheel"),
        copy_from   => remote_copy("$(g.masterfiles)/rc.conf/rc.conf-$(sys.fqhost)", "$(g.policyhost)");

    "/etc/resolv.conf"

        comment     => "Copy resolv.conf file for domain $(sys.domain)",
        create      => "true",
        perms       => mog("0644", "root", "wheel"),
        copy_from   => remote_copy("$(g.masterfiles)/domain/resolv.conf-$(sys.domain)", "$(g.policyhost)");

    freebsd_host::

        "/etc/ethers"

            comment     => "Copy ethernet address database for domain $(sys.domain)",
            create      => "true",
            perms       => mog("0644", "root", "wheel"),
            copy_from   => remote_copy("$(g.masterfiles)/domain/ethers-$(sys.domain)", "$(g.policyhost)");
}
```

In the `domain` bundle we have defined the following:

First we deploy `/etc/rc.conf` configuration files for the systems
under CFEngine control.

This promise simply copies the `/etc/rc.conf` file on the systems,
which are under CFEngine control.

An `/etc/rc.conf` on the CFEngine policy server is in the form of
`rc.conf-<hostname>`.

The `$(sys.fqhost)` CFEngine variable contains the
fully-qualified domain name of the system that `cf-agent(8)` runs on.

Then we deploy the `/etc/resolv.conf` file on the systems in the domain.

This promise copies the `/etc/resolv.conf` file on the systems under
CFEngine control for their domain. The `$(sys.domain)` CFEngine
variable contains the domain name of the system.

After that we deploy `/etc/ethers` file, which contains
hostname / MAC addresses of the systems in the domain.

If you often need to wake a system over the lan like I do, keeping a
record of the systems hostname and MAC address in that file allows you
to use `wake(8)` only by specifying the system hostname.

Finally we have also defined one new promise body - `remote_copy`.

`remote_copy` is a promise body, which we will add to our
[CFEngine Community Open Promise Body Library](http://www.cfengine.org/manuals/CFEngineStdLibrary.html)
file - `library.cf`

```text
body copy_from remote_copy(source, server) {
    source      => "$(source)";
    servers     => { "$(server)" };
    compare     => "digest";
    purge       => "true";
    copy_backup => "false";
}
```

Now add the files you've created and updated to Git, commit them, and
then push them to the correct branch of the remote Git repository.

The last thing we need to do is to add the `rc.conf`, `resolv.conf`
and `ethers` files of our client machines and domain to CFEngine.

These files will be in the `masterfiles` directory in the CFEngine
Git repository.

## Adding /etc/rc.conf files of the systems to CFEngine

The */etc/rc.conf* file for each system under CFEngine control will be in the *masterfiles/rc.conf* directory in our Git repository.

```bash
$ cd ~/PROJECTS/cfengine/masterfiles
$ mkdir rc.conf && cd rc.conf
```

Now, get the `/etc/rc.conf` files for each system in your domains and
copy them to the `masterfiles/rc.conf` directory of your Git repository
in the form of `rc.conf-<hostname>`, e.g. for the system
`www.site1.example.org` it's rc.conf file will be
`rc.conf-www.site1.example.org`.

Do this for all systems under CFEngine control, then add the files
to Git, commit them, and push them to the correct branch of the
remote Git repository.

## Adding /etc/resolv.conf files to CFEngine

Similarly to the previous step, where we have added the `/etc/rc.conf`
files of our systems to CFEngine, in this step we will add the
`/etc/resolv.conf` files for our domains.

The `/etc/resolv.conf` files will be in the `masterfiles/domain`
directory in our Git repository.

```bash
$ cd ~/PROJECTS/cfengine/masterfiles
$ mkdir domain && cd domain
```

Now, get the `/etc/resolv.conf` files for your domains and add them
to the `masterfiles/domain` directory of your Git repository in the
form of `resolv.conf-<domain-name>`, e.g. if the domain name for
`site1` is `site1.example.org`, then your `resolv.conf` filename for
this domain will be `resolv.conf-site1.example.org`.

Do this for all domains you have, then add the files to Git,
commit them, and push them to the correct branch of the
remote Git repository.

## Adding /etc/ethers files to CFEngine

The `/etc/ethers` file simply contains pairs of hostname and
MAC addresses of the systems in your domain.

I often have to `wake(8)` a system up over the network, and
remembering it's MAC address is not always possible.

For all your systems of the domain (or those you only need) add their
hostnames and MAC addresses in the
`masterfiles/domain/ethers-<domain-name>` file, in a similar way you
did it in the previous step for `resolv.conf`.

When ready, add the files to Git, commit them, and then push them
to the correct branch of the remote Git repository.

## Updating the CFEngine policy server

When you are done setting up your configuration files for systems and
domains, it is time to update the configuration on the
CFEngine policy server.

To do so, login to the CFEngine policy server and execute:

```bash
cd /var/cfengine && git pull
```

Verify that the files you've added/updated are present and validate the
new promises.

```bash
$ sudo cf-promises -v
```

If everything is OK, then just login to a system in your domain and
execute `cf-agent(8)` - now your `/etc/rc.conf`, `/etc/resolv` and
`/etc/ethers` files will be under CFEngine control.

To verify that this works as expected, modify manually
`/etc/rc.conf` and then again execute `cf-agent(8)`.

When you do a change to a file that is under CFEngine control, and
CFEngine sees this change it will try to conform to the
promises it made, and will restore the file to it's original state.

Having these basic promises for managing domain settings, you should
get an idea how to further automate your systems with CFEngine.
