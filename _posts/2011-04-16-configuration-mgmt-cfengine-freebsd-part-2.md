---
layout: post
title: Configuration Management on FreeBSD with CFEngine - Part II
tags: configuration management cfengine bsd
---

The work directory of CFEngine consists of a number of other
directories and files:

* /var/cfengine/bin - CFEngine binaries
* /var/cfengine/inputs - Main configuration files of CFEngine
* /var/cfengine/lastseen - Contains records of last-seen agents
* /var/cfengine/masterfiles - Master files on the server, that agents will request from the server
* /var/cfengine/modules - Contains additional variables and classes definition based on user-defined code
* /var/cfengine/outputs - Contains reports of previous runs of *cf-agent(8)*
* /var/cfengine/ppkeys - Stores the authentication keys
* /var/cfengine/reports - The output directory used by *cf-report(8)*
* /var/cfengine/state - Directory containing the various states of promises

Most of these directories are populated by CFEngine.

The directories in which we are interested are the `inputs`,
`masterfiles` and `ppkeys`. We will put only these directories and
their related files under Git control.

So, now clone the Git repository you've created earlier and do our
changes from there, which will later be pulled by the CFEngine server.

So let's clone the empty Git repository to our local machine and start
preparing the CFEngine configuration.

```bash
$ mkdir -p ~/PROJECTS && cd ~/PROJECTS
$ git clone <user>@git.example.org:/home/git/public/cfengine
$ cd cfengine
$ mkdir inputs masterfiles ppkeys
```

We will first start with some basic configuration of CFEngine, then we
will create a new branch in Git for the test environment, so we can do
all of our tests there, which if successful then we can bring to the
production environment. The test environment will be under the `TEST`
branch in Git, and our production environment configuration will in the
`master` branch of Git.

The basic configuration of CFEngine generally consists of these files.
Once we have that ready, later on we will add additional files with
promises, so we can extend CFEngine with more complex configuration.

* promises.cf - The main CFEngine configuration file
* update.cf - Contains promises for the agents, just to ensure that the
  latest promises are updated on the clients.
* failsafe.cf - This file is run by the agents if there are no
  configuration files found. It is being used by the agents in order
  to recover from a failure.
* cf-serverd.cf - Contains configuration for the CFEngine master servers
* cf-execd.cf - Contains configuration for the CFEngine Executor/Scheduler
* cf-report.cf - Contains configuration for the CFEngine
  self-knowledge extractor
* classes.cf - Global CFEngine classes.
* cleanup.cf - Contains configuration for tyding up of old files/directories.
* library.cf - This is the
  [CFEngine Community Open Promise-Body Library](http://www.cfengine.org/manuals/CfengineStdLibrary.html).
  A file containg pieces of reusable code.

So let's start with preparing the configuration for the above files,
and later we will extend our configuration to a more complex one.

```bash
$ cd ~/PROJECTS/cfengine/inputs
```

If there is something not clear in the configuration, please also
consider checking the
[CFEngine reference manual](http://www.cfengine.org/manuals/cf3-reference.html),
which contains more detailed explanation.

## promises.cf

In this step we will create the main CFEngine configuration file.

```text
#####################################################
#                                                   #
# promises.cf - Main CFEngine configuration file  #
#                                                   #
#####################################################

body common control {

    any::

        bundlesequence => { @(g.bundlesequence) };

    any:: 

        inputs => {
            "update.cf",
            "library.cf",
            "classes.cf",
            "cf-execd.cf",
            "cf-serverd.cf",
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
    "bundlesequence"    slist  => { "update", "executor", "server", "cleanup" };
}

body runagent control {
    hosts => { "127.0.0.1", "10.1.16.0/20" };
}
```

In the above configuration for `promises.cf` what we did is the following:

* bundlesequence - this defines the `bundlesequence` to be executed by the agents.
  Think of it as a list of actions, that the agents will perform when
  `cf-agent(8)` is running.
* inputs - defines a list of additional configuration files to be
  included. This is where we will define our additional promises.
* output_prefix - this sets the prefix you will see when running
  `cf-agent(8)`. The default value of this is `community>`
* global vars - In the `"g"` bundle we define global variables,
  that we can later refer to and use in other promises.
* runagent body - defines a list of hosts/networks, that we can
  connect to the running `cf-serverd(8)` and ask for executing of
  `cf-agent(8)`.

The `bundlesequence` as mentioned above defines a list of
promises (actions) that the agent will try to conform to, when running.

In the above configuration we define one global bundlesequence
that will be common to all clients.

This allows us to have a standard list of promises for agents, but also
allows us to define additional promises for a certain group of
clients if need to, as we will see in the example configurations in the
last chapter.

## update.cf

The next file we need to setup is the `update.cf` one.

This file keeps the promises for updating the configuration on the
clients. The `update` promise usually needs to be the first one that
runs in order to ensure that the agents have the latest configuration.

One other thing to note about `update.cf` - this file generally does
not change at all, once configured properly.

It's only purpose is to ensure that clients are having the latest
configuration files and nothing more. So once you have this
configured - you do not change it, at all.

```text
#####################################################
#                                                   #
# promises.cf - Promises for updating policy files  #
#                                                   #
#####################################################

bundle agent update {

vars:

    "u_workdir"     string => "/var/cfengine";
    "u_policyhost"  string => "cfengine.example.org";

classes:

    "u_policy_servers" or => { classify("$(u_policyhost)") };

files:

    "$(u_workdir)/."

        comment         => "Set proper permissions of the work directory",
        create          => "true", 
        perms           => u_workdir_perms("0600");

    "$(u_workdir)/bin/." 

        comment         => "Copy CFEngine binaries to $(u_workdir)/bin",
        create          => "true",
        perms           => u_workdir_perms("0700"),
        depth_search    => u_recurse("inf"),
        file_select     => u_cf3_bin_files,
        copy_from       => u_copy_cf3_bin_files("/usr/local/sbin");
 
    "$(u_workdir)/masterfiles/."
    
        comment         => "Set proper permissions of the $(u_workdir)/masterfiles directory",
        create          => "true",
        perms           => u_workdir_perms("0600"),
        depth_search    => u_recurse("inf");

    u_policy_servers::

        "$(u_workdir)/inputs/."

            comment             => "Set proper permissions of input files on policy server",
            create              => "true",
            perms               => u_workdir_perms("0600"),
            depth_search        => u_recurse("inf");

    !u_policy_servers::

        "$(u_workdir)/inputs/." 

            comment         => "Update input files from policy server",
            create          => "true",
            perms           => u_workdir_perms("0600"),
            depth_search    => u_recurse("inf"),
            copy_from       => u_policy_copy("$(u_policyhost)");
}

#
# u_cf3_bin_files
# 

body file_select u_cf3_bin_files {
    leaf_name   => { "cf-.*" };
    file_result => "leaf_name";
}

#
# u_workdir_perms
# 

body perms u_workdir_perms(mode) {
    mode    => "$(mode)";
    owners  => { "root" };
    groups  => { "wheel" };
}

# 
# u_policy_copy
#

body copy_from u_policy_copy(server) {
    source      => "$(u_workdir)/inputs";
    servers     => { "$(u_policyhost)" };
    compare     => "digest";
    purge       => "true";
    copy_backup => "false";
}

#
# u_copy_cf3_bin_files
#

body copy_from u_copy_cf3_bin_files(path) {
    source  => "$(path)";
    compare => "digest";
}

# 
# u_recurse
#

body depth_search u_recurse(d) {
    depth   => "$(d)";
    xdev    => "true";
}
```

In the above configuration for `update.cf` what we did is the following.

First we started with defining some local variables - `u_workdir` and
`u_policyhost`.

You might be wondering why we define the same variables, which we
already have in `promises.cf`, but with different names.

The reason for having these variables here as well is to make the
`update.cf` configuration self-contained, or in other words we do not
want `update.cf` to be dependent on other configuration files.

As mentioned earlier we generally do not want to do any changes to
this file, once we set it up correctly, since the only purpose of
this configuration is to properly update the policy files on the agents.

If we mess up the CFEngine configuration and distribute it to our
clients, what we want is that after fixing it, the `update` promise
will take care of properly updating the files on the clients as well.

We have then created a `class` called `u_policy_servers` which defines
our policy servers (CFEngine master servers).

We want to be able to know whether or not `cf-agent(8)` is running on
our clients or the master server itself.

With that being said, we do not update policy files on the master
server, but on the clients only. Having `u_policy_servers` class we
restrict the updating of policy files only to the clients.

Afterwards we defined promises for updating policy files and set of
permissions.

In the `files` section we have defined promises for updating the
policy files and also set proper permissions on files and
directories. The comments in each of them should be
pretty self-explanatory what the promise does.

In each of the promises, you will notice that we use the so
called `promise bodies` - this just represents a part of a promise,
which details and constrains it's nature, and they are defined at the
end of the file - `u_cf3_bin_files`, `u_workdir_perms`, `u_policy_copy`,
etc.

Each of these promise bodies contains details about what they do.

## failsafe.cf

This file is run by the agents when there are no
configuration files found.

Generally this file should contain promises for recovering from
failure, thus allowing our systems to self-heal.

```text
#########################################
#                                       #    
# failsafe.cf - Failsafe configuration  #
#                                       #
#########################################

body common control {

    any::

        bundlesequence => { "update" };

        inputs => { "update.cf" };
}

bundle common failsafe_globals {

vars:
    
    "f_policyhost" string => "cfengine.example.org";

classes:

    "f_policy_servers" or => { classify("$(f_policyhost)") };
}
```

In the above configuration we define only one bundle in the
`bundlesequence`, and that is the `update` one.

What we want to happen when a system fails, is that it is able to
update it's policy files to the latest ones from the server.

Note that this file relies on `update.cf` in order for the clients to
be able to update their policy files.

## cf-serverd.cf

`cf-serverd.cf` contains the configuration for the master CFEngine
servers (a.k.a policy servers).

```text
#####################################
#                                   #
# cf-serverd.cf - CFEngine Server #
#                                   #
#####################################

body server control {
    skipverify          => { "10.1.16.0/20" };
    allowconnects       => { "10.1.16.0/20" };
    allowallconnects    => { "10.1.16.0/20" };
    maxconnections      => "100";
    logallconnections   => "true";
    bindtointerface     => "x.x.x.x";
    cfruncommand        => "$(sys.workdir)/bin/cf-agent";
    allowusers          => { "root" };
}

# Make sure that the server is running on the policy servers
bundle agent server {

vars:

    "rc_d" string => "/usr/local/etc/rc.d";

processes:
    
    policy_servers::

        "cf-serverd"
            
            comment       => "Make sure cf-serverd runs on the policy servers",
            restart_class => "start_cfserverd";

commands:

    start_cfserverd::
    
        "$(rc_d)/cf-serverd start";
}

bundle server access_rules {

access:

    # Allow clients access to the input files
    "$(g.inputfiles)"

        admit => { "10.1.16.0/20" };

    # Allow clients access to the masterfiles
    "$(g.masterfiles)"

        admit => { "10.1.16.0/20" };
}
```

In the [server control promise](http://www.cfengine.org/manuals/cf3-reference.html#control-server)
we have defined various configuration settings like:

* skipverify - this tells the server to do no DNS lookups for the 10.1.16.0/20 subnet
* allowconnects - defines the hosts/networks that are allowed to connect to it.
* maxconnections - maximum number of connections to the server.
* logallconnections - no need to explain what this does.
* bindtointerface - specifies the IP address on which the server will bind.
* cfruncommand - path to the cf-agent command or cf-execd wrapper for remote execution
* allowusers - list of usernames who may execute requests from this server

We have defined one local variable which points to the location of rc
scripts on our systems - `rc_d`.

In the `processes` section we made a promise that we want to have
`cf-serverd(8)` running on the policy servers.

The `policy_servers` here is a `class` that is already defined in
`classes.cf` as we will see a bit later.

In the `commands` section we have defined the actual command to use
for starting up `cf-serverd(8)` if there is no process of
`cf-serverd(8)` running on the policy servers.

And lastly we have defined the subnets that are allowed to have
access to the policy files and master files on the server.

All clients should be able to update their policy files from the
server, so that's why we list the subnet of our clients here.

## cf-execd.cf

This file contains the configuration of the CFEngine Executor/Scheduler.

```text
#####################################
#                                   #
# cf-execd.cf - CFEngine Executor #
#                                   #        
#####################################

body executor control {
    splaytime           => "1";
    mailto              => "admin_AT_example_DOT_org";
    mailfrom            => "cfengine_AT_example_DOT_org";
    smtpserver          => "mail.example.org";
    mailmaxlines        => "100";
    schedule            => { "Min05" };
    executorfacility    => "LOG_DAEMON";
}

# Make sure the executor is running
bundle agent executor {

vars:

    "rc_d" string => "/usr/local/etc/rc.d";

processes:

    "cf-execd"
    
        comment       => "Make sure that cf-execd runs on all hosts",
        restart_class => "start_cfexecd";

commands:

    start_cfexecd::

        "$(rc_d)/cf-execd start";
}
```

In the [executor control promise](http://www.cfengine.org/manuals/cf3-reference.html#control-executor)
we have defined various configuration settings like:

* splaytime - time in minutes to splay this host based on its name hash
* mailto - where mails should be send to
* mailfrom - sender of the email
* smtpserver - smtp server address.
* mailmaxlines - maximum lines of the email.
* schedule - when `cf-execd(8)` to schedule an execution of
  `cf-agent(8)`. In the configuration above that means each hour and 5 minutes
* executorfacility - syslog facility level

Similar to the `cf-serverd.cf` configuration, here we have defined a
promise to start up the CFEngine executor if it is not running already.

## cf-report.cf

NOTE: You no longer need cf-report.cf as of CFEngine 3.5.
This information was left here for historical reasons.

`cf-report.cf` contains configuration for the CFEngine
self-knowledge extractor.

```text
#####################################
#                                   #
# cf-report.cf - CFEngine Reports #
#                                   #
#####################################

body reporter control {
    reports         => { "performance", "last_seen", "monitor_history" };
    build_directory => "$(sys.workdir)/reports";
    report_output   => "text";
}
```

In the above configuration we define the reports we want to have,
where to store them and what format to use for the reports.

## classes.cf

We will use this file for user-defined CFEngine classes.

Classes in CFEngine refer to a specific context, e.g. we might have a
class called `webservers` which defines our web servers in the domain,
and later on we can make promises for that class, like for example
ensuring that the Apache configuration is installed, the server is
running, etc..

```text
#################################################
#                                               #
# classes.cf - CFEngine user-defined classes  #
#                                               #
#################################################

bundle common myclasses {

vars:

    "sysctl_jailed" string => execresult("/sbin/sysctl -n security.jail.jailed", "noshell");

classes:

    "freebsd_jail" expression => strcmp("$(sysctl_jailed)", "1");
    "freebsd_host" expression => strcmp("$(sysctl_jailed)", "0");

    "policy_servers" or => {
        classify("$(g.policyhost)")
    };
}
```

In the above configuration we start with very simple definition of
CFEngine classes, which we will later extend.

Here's what has been done.

To the `sysctl_jailed` variable is assigned the result of the
command from [execresult()](http://www.cfengine.org/manuals/cf3-reference.html#Function-execresult).

In `classes` we define two user-defined classes -
`freebsd_jail` and `freebsd_host`.

As mentioned earlier `classes` in CFEngine is a context.
User-defined classes are evaluated during run of `cf-agent(8)`.

Depending on the value of `sysctl_jailed` we will have one of the
`freebsd_jail` or `freebsd_host` classes defined, depending whether
`cf-agent(8)` is running on a FreeBSD jail, or a host instead.

Using these classes later on we can have promises for our FreeBSD
jails and hosts, where for example we want to have one
configuration of a package installed on a host, and another one on a
FreeBSD jail.

We've defined the `policy_servers` class which will contain the
policy servers only.

## cleanup.cf

In this file we will have configuration for tidying up old files and
directories that are no longer needed.

```text
#####################################################
#                                                   #
# cleanup.cf - CFEngine promises for tidying up   #
#                                                   #
#####################################################

# A bundle for cleaning up old and not needed files and directories
bundle agent cleanup {
    
files:

    # Cleanup old reports
    "$(sys.workdir)/outputs"
    
        comment         => "Clean up reports older than 3 days",
        delete          => tidy,
        file_select     => days_old("3"),
        depth_search    => recurse("inf");
}
```

The above configuration only adds a single promise for tidying up old
CFEngine reports from the `outputs` directory of CFEngine.

## library.cf

This file contains the
[Cfengine Community Open Promise Body Library](http://www.cfengine.org/manuals/CFEngineStdLibrary.html).

It simply contains pieces of reusable code, that you can use in your promise bodies.

After installing the CFEngine package on your FreeBSD system this file
is installed in `/usr/local/share/doc/cfengine3/inputs/cfengine_stdlib.cf`,
so just simply copy it to your working tree and rename it as `library.cf`

And that was our basic CFEngine configuration. If you've followed the
handbook by this step, you should have a working basic
configuration of CFEngine.

## Commiting the configuration to Git

The last thing to do is to push this configuration to the Git
repository, and then clone it from on the CFEngine master servers.

In the beginning of this handbook we've mentioned that we are going to
have two environments - a test one, and a production one.

Each time we want to bring something to production we are going to
deploy the changes to the test environment first, and when they are
verified to be OK - we bring the changes to the production
environment as well.

The test environment configuration will be under the test branch in
Git, and the configuration for our production environment will be
under the `master` branch in Git.

So, considering you have followed the handbook to this step,
let's now add the configuration we've prepared to Git's `master`
branch, which is our production environment configuration.

Remember that all of the above CFEngine configuration files we've
prepared already need to be under the `inputs` directory,
so considering that you are now in that directory, let's add the
files to Git.

```bash
$ git add .
$ git commit -m "Initial commit of the CFEngine configuration"
$ git push origin master
```

The above commands add the files we've created to Git,
commits them to the local Git repository, and then pushes them to the
remote Git repository in the `master` branch of Git.

Now we are going to create a new branch in Git for our test
environment and push the changes there as well,
so that we have our `TEST` environment configuration prepared also.

```bash
$ git checkout -b TEST
```

The changes that we need to do for the test environment are
just a few - we need to change the policy server hostname from
`cfengine.example.org` to `cfengine-test.example.org` and set the
proper IP address of the `cfengine-test.example.org` policy server.

Below are the files that you need to update for the test environment:

* cf-execd.cf
* cf-serverd.cf
* classes.cf
* failsafe.cf
* promises.cf
* update.cf

Once you are ready with the updates of the above configuration
files, let's add them to the `TEST` branch of Git and push the
changes to the remote repository.

```bash
$ git add .
$ git commit -m "Initial commit of the CFEngine configuration for TEST environment" 
$ git push origin TEST
```

Once you have the branches for your environments in Git,
bringing changes from the  environment to the production one,
can be done in several ways in Git - branch merging, patching, etc.

Since this is already well discussed in a lot of books and
tutorials about Git, it will not be included in this handbook in order
to keep it clean and tight. 

## Cloning the Git repository on the policy servers

Now that we have the basic configuration of CFEngine under Git control,
login to the CFEngine master servers and clone the configuration.

Each time we do an update of the configuration,
we will also have to pull the changes on the CFEngine servers as well.

As mentioned in the beginning of this chapter the CFEngine work
directory resides in `/var/cfengine`, so let's clone the
configuration there now.

On the cfengine.example.org server (production policy server):

```bash
# cd /var
# git clone --branch master <username>@git.example.org:/home/git/public/cfengine
```

On the cfengine-test.example.org server (test policy server):

```bash
# cd /var
# git clone --branch TEST <username>@git.example.org:/home/git/public/cfengine
```

## Creating the authentication keys

Similarly to [OpenSSH](http://www.openssh.com/),
CFEngine uses a client-server authentication using keys, which are
called `ppkeys`.

Each client needs to have the public key of the
CFEngine master server, and the server needs to have the
public key of each client.

To create the ppkeys on the policy servers, login to them and execute:

```bash
# cf-key
Making a key pair for cfengine, please wait, this could take a minute...
```

Along with the ppkeys of CFEngine the above command will also
create the needed directories of the CFEngine trusted work directory,
which were explained before.

Now let's add the ppkeys of the policy servers to the
Git repository as well.

On our local machine where we do all our changes to the
CFEngine configuration using Git, we will now add the public
ppkeys of the CFEngine policy servers to their corresponding branches.

First, secure copy the `/var/cfengine/ppkeys/localhost.pub` files from
the policy servers to your local machine and rename them
to `root-x.x.x.x.pub` and `root-y.y.y.y.pub`, where `x.x.x.x` and
`y.y.y.y` are the IP addresses of the your policy servers -
the test and production one.

In CFEngine 2 the public ppkeys were in the form of
`root-x.x.x.x.pub`.

CFEngine now uses an MD5 hash instead of the IP address of the host,
but we can still use the legacy form since it allows us to easily
recognize a ppkey for a host only using it's IP address.

When the CFEngine servers notices that the ppkeys are in the legacy
form, they will be automatically converted to the new form.

Adding the public ppkey for the production CFEngine server:

```bash
# cd ~/PROJECTS/cfengine
# git checkout master
# cp /path/to/root-x.x.x.x.pub ppkeys/
# git add .
# git commit -m "Adding ppkey for production CFEngine policy server"
# git push origin master
```

Adding the public ppkey for the TEST CFEngine server:

```bash
cd ~/PROJECTS/cfengine
git checkout TEST
cp /path/to/root-y.y.y.y.pub ppkeys/
git add .
git commit -m "Adding ppkey for TEST CFEngine policy server"
git push origin TEST
```

Now, login to the test and production CFEngine policy servers,
and update the Git repository of CFEngine, so that the
ppkey is added as well.

```bash
cd /var/cfengine && git pull
```

## Validating the CFEngine promises

The first thing you want to do before starting up CFEngine is to do a
validation of your promises and configuration.

To do this we need to first copy the `/usr/local/sbin/cf-promises`
binary to the CFEngine trusted work directory in `/var/cfengine/bin`.

When `cf-agent(8)` is running it will check for `cf-promises(8)` to
be present in `/var/cfengine/bin` and if it cannot find it there to
validate it's promises it will fail.

Later on our promises we have already defined in the
configuration files will also take care of copying the
CFEngine tool-suite to `/var/cfengine/bin` directory.

To validate your CFEngine promises, now execute:

```bash
$ sudo cf-promises
```

If there are no errors reported, then everything is OK and you can
proceed, otherwise you will need to check your configuration again
and fix the problems before continuing further.

For more verbose output use the `-v` flag to `cf-promises(8)`.

## Enabling CFEngine during boot-time

The next thing we need to do is to enable the CFEngine Server
and Executor on the policy servers during boot-time.

To do this, add the following lines to your `/etc/rc.conf` file:

```bash
# Enable CFEngine Executor
cf_execd_enable="YES"

# Enable CFEngine Server
cf_serverd_enable="YES"
```

## First time run of CFEngine!

Now it is time that we start up CFEngine, and get it to actually do
something for us.

Login to the policy servers and then let's start up `cf-agent(8)`:

```bash
$ sudo cf-agent -v
```

The above command will start `cf-agent(8)` in verbose mode,
and will also try to conform to the promises we've made before
in our configuration.

Inspect the output of the `cf-agent(8)` run for any errors or
misconfigurations.

A simple way to confirm that `cf-agent(8)` conforms to our
promises is to check after the agent run that the `cf-serverd(8)` and
`cf-execd(8)` processes were started by the agent,
since we made promise in our configuration that we want these
processes running on the policy servers.

Now that we have the basic configuration and setup, let's add
clients to our configuration, which we'll see how to do in the
next chapter of the handbook.
