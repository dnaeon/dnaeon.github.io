---
layout: post
title: Configuration Management on FreeBSD with CFEngine - Part III
tags: configuration management cfengine bsd
---

Now that we have our CFEngine policy servers configured properly and
running, the next thing we are going to do is to configure the
clients (agents), that will be controlled via CFEngine.

Throughout the handbook we've talked about having different
environments in our setup - a test and a production one.

When adding a new client to CFEngine we need to decide
whether this isa TEST host/jail or a production one, and after that
we need to install the CFEngine package on the client machine along
with the configuration files and ppkeys of the
CFEngine server for that environment.

To do this we are going to make use of the
[ezjail(5) flavours](http://erdgeist.org/arts/software/ezjail/#Flavours"). 

The `ezjail(5)` flavours allows us to predefine what packages, 
configurations, users, etc. we want to have when installing a new jail.

In our case we will create two ezjail flavours - `TEST` and `STABLE`.

Each of these flavours will install on the newly created jails the
CFEngine packages, and will also install the configuration and 
ppkeys of the corresponding environment.

This makes it very easy to setup a CFEngine client system upon it's creation.

## Creating the ezjail(5) flavours of the environments

Considering you are managing your
[FreeBSD Jails](http://www.freebsd.org/doc/handbook/jails.html) with
[sysutils/ezjail](http://www.freshports.org/sysutils/ezjail), we will
now create two *ezjail(5)* flavours for our production and
test environment, and we will call them `STABLE` and `TEST` respectively.

In the commands below `${ezjail_jaildir}` refers to the directory 
you've specified in your `/usr/local/etc/ezjail.conf` file.

On the host machine where your jails are installed:

```bash
$ cd ${ezjail_jaildir}/flavours
$ sudo cp -a default TEST
$ sudo cp -a default STABLE
```

Now let's prepare the flavours. In the newly created flavours 
directories - `TEST` and `STABLE`, you will have a similar
directory structure:

```text
etc
|
|
|\_ make.conf
|\_ periodic.conf
|\_ rc.conf
 \_ resolv.conf 
 
ezjail.conf
pkg
root
usr
	
var
|
|\_ cfengine/bin
|\_ cfengine/inputs
 \_ cfengine/ppkeys
```

Most of these directories already contain predefined configuration, so
we'll just add to it what is needed in order to setup our newly
created jail with CFEngine.

In the `pkg` directory of the flavour copy your CFEngine package with
it's dependencies, so that they will be installed when we
first start the jail.

In the `etc/rc.conf` file of the flavour add the following lines, so
that the CFEngine Executor is enabled during boot-time:

```bash
# Enable CFEngine Executor
cf_execd_enable="YES"
```

In the `etc/resolv.conf` file of the flavour put your domain name server.

In the `ezjail.conf` file add the following lines, which will just
setup the correct timezone for the jails, symlink `/home` to
`/usr/home`, and copy `cf-promises(8)` to the CFEngine trusted work directory.

```bash
# symlink /home to /usr/home
ln -s /usr/home /home

# set the timezone to Europe/Sofia
ln -s /usr/share/zoneinfo/Europe/Sofia /etc/localtime

# copy cf-promises(8) to /var/cfengine/bin
cp /usr/local/sbin/cf-promises /var/cfengine/bin
```

In the `var/cfengine` directory of the flavour create three other
directories - `inputs`, `ppkeys` and `bin`. In the `inputs` directory
copy the `failsafe.cf` and `update.cf` files for your corresponding
environment. 

In the `ppkeys` directory copy the ppkey for the
CFEngine policy server of the corresponding environment in the form of
`root-x.x.x.x.pub`.

The `ppkeys` directory needs to be secure enough, 
otherwise CFEngine will refuse to run, so change it's permissions now:

```bash
$ sudo chmod -R 0600 ${ezjail_jaildir}/flavours/TEST/var/cfengine/ppkeys
$ sudo chmod -R 0600 ${ezjail_jaildir}/flavours/STABLE/var/cfengine/ppkeys
```

Once you are ready with the `ezjail(5)` flavours, when you are
installing a new FreeBSD jail you will be creating it in a similar way,
like this for example.

For the test environment:

```bash
$ sudo ezjail-admin create -f TEST jail-test x.x.x.x
```

For the production environment:

```bash
$ sudo ezjail-admin create -f STABLE jail y.y.y.y
```

## Creating the authentication keys for the clients

Now that you have your FreeBSD jails installed, start up your jails and
login to them.

Depending on the flavour you've used during the jail creation,
you should have the CFEngine package package installed with it's
dependencies and the ppkey of the CFEngine policy server already
present in your jail.

Now we need to create a ppkey for the client as well, and have it
deployed to the policy server, so that the client can connect to the
CFEngine policy server.

On the jail system (client), create the authentication keys:

```bash
$ sudp cf-key
Making a key pair for cfengine, please wait, this could take a minute...
```

Now secure copy the `/var/cfengine/ppkeys/localhost.pub` file from the
jail (client system) to your local machine and rename it to 
`root-x.x.x.x.pub`, where `x.x.x.x` is the jail's (client system) 
IP address, then add the ppkey to the Git repository.

```bash
$ cd ~/PROJECTS/cfengine/ppkeys
$ cp /path/to/root-x.x.x.x.pub .
$ git add .
$ git commit -m "Adding ppkey for <jail-name>"
$ git push origin <branch>
```

Where `<branch>` is the Git branch for the environment you are 
adding the ppkey - `TEST` for the test environment and `master` for the
production environment.

Then login to the CFEngine policy server of the corresponding
environment (TEST or production one) and pull the changes.

On the CFEngine policy server:

```bash
cd /var/cfengine && git pull
```

Verify that the ppkey of the client is now in 
`/var/cfengine/ppkeys/root-x.x.x.x.pub`

## Starting CFEngine on the clients

We have almost everything configured by now on the client systems, and
we can now start the agents.

The first time you run `cf-agent(8)` it will go to failsafe mode,
because there will be no main CFEngine configuration file found in
`/var/cfengine/inputs`.

When the agent goes to failsafe it will search for the `failsafe.cf`
file and run it.

Since our failsafe configuration we did already is simply doing an
update of the policy files, and we already deploy the failsafe
configuration during jail creations, the first time `cf-agent(8)`
runs it will simply do an update of our configurations.

On the client machines:

```bash
$ sudo cf-agent -v
```

Now you can verify that `/var/cfengine/inputs` contains the
configuration files, which were copied to the client from the
policy server.

The next time you run `cf-agent(8)` it will start parsing the
configuration defined in the main configuration of CFEngine - 
`promises.cf` and the other configuration files we have included as well.

Execute `cf-agent(8)` a couple of times, and verify it works OK.

If you've followed the handbook by this step, you should have 
already a running CFEngine server and clients.

Now that we have the basic setup of CFEngine on our clients and server,
let's do something useful with CFEngine.

In the next chapter you will find various examples of 
CFEngine configurations.

