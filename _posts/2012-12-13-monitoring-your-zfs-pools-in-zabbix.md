---
layout: post
title: Monitoring your ZFS pools in Zabbix
created: 1355427487
tags: zfs monitoring freebsd zabbix
---
In  this post we'll see how we can setup Zabbix in order to
monitor our ZFS pools.

For demonstration purposes I've setup a test virtual machine running
FreeBSD 9.0 with a ZFS pool of two disks in a mirror. We will see how
to monitor the ZFS pool's health in [Zabbix](http://zabbix.com) and
trigger alarms in case we have some issues with our ZFS pools.

Basic understand and knowledge about Zabbix is required as well in
order to better understand the material in this post.

First, lets see our ZFS pool:

```bash
% zpool status                           
  pool: zroot
 state: ONLINE
  scan: none requested
config:

     NAME        STATE     READ WRITE CKSUM
     zroot       ONLINE       0     0     0
       mirror-0  ONLINE       0     0     0
         ada0p2  ONLINE       0     0     0
         ada1p2  ONLINE       0     0     0

errors: No known data errors
```

Okay, our ZFS pool is looking good. Now lets proceed with the Zabbix
setup.

We are going to create a template for our ZFS pools in Zabbix, so
later it would be easy to extend it and attach to existing systems.

First we need to add a new *UserParamater* to our Zabbix Agents, which
will be used for polling information for our ZFS pools.

Create a new file under
*/usr/local/etc/zabbix2/zabbix_agentd.conf.d/zfs.conf* (this assumes
that your *zabbix_agentd.conf* file includes the
*/usr/local/etc/zabbix2/zabbix_agentd.conf.d/* directory).

The contents of the *zfs.conf* file are shown below:

```bash
UserParameter=zpool.health[*],zpool list -H -o health $1
```
	
Restart the Zabbix agent:

```bash
$ sudo service zabbix_agentd restart 
```

Now we can continue with configuring our Zabbix checks. Login to your
Zabbix server and navigate to *Configuration* -> *Templates* and then
click on the *Create template* button.

Name your template *Template App ZFS* and click the save button as
shown in the example screenshot below:

![_config.yml]({{ site.baseurl }}/images/zabbix-zfs-1.jpg)

Next we create a new application in our template, which is called
*ZFS Checks*:

![_config.yml]({{ site.baseurl }}/images/zabbix-zfs-2.jpg)

Now we are going to create a new item that will be checking our ZFS
pools, so go to the *Items* tab of your template and click the *Create
Item* button.

On the screenshot below you can see the item we've created. As the
*key* of our item we use the newly added *UserParameter* which is
called *zpool.health*, the type of information should be set to
*Text*. You may also note that we pass an argument to our key
*zpool.health[zroot]*, which specifies the ZFS pool's name.

![_config.yml]({{ site.baseurl }}/images/zabbix-zfs-3.jpg)

Now, it's time to add a trigger for our item. Triggers are the way you
fire up alarms in Zabbix. Here we are going to create a trigger that
will go into alarm if our ZFS pool is no longer in online state. Now,
navigate to the *Triggers* tab of your template and click on the
*Create* button in order to create a new trigger.

You can see the trigger we've created on the screenshot below:

![_config.yml]({{ site.baseurl }}/images/zabbix-zfs-4.jpg)

The trigger's expression we use in the screenshot above is:

```text
{Template App ZFS:zpool.health[zroot].regexp(ONLINE)}=0
```
	
What it basically does is that it will enable the trigger if our ZFS
pool is no longer in *ONLINE* state, which means there's something
wrong with your pool - a disk failed or whatever.

The last thing you need to do is to attach your ZFS template to your
systems, so that you can start monitoring your ZFS pools'
health. Checking the latest data of our test server we can see that
our ZFS pool is online:

![_config.yml]({{ site.baseurl }}/images/zabbix-zfs-5.jpg)

In order to show you how Zabbix detects problems with our ZFS pools, I
am going to remove physically one of the disks on my test machine. You
can see how Zabbix detected the issue right away from the screenshot
below:

![_config.yml]({{ site.baseurl }}/images/zabbix-zfs-6.jpg)

Checking on the server the status of our ZFS pool we can see that the
*zroot* pool is indeed in degraded state:

```bash
% zpool status
  pool: zroot
 state: DEGRADED
status: One or more devices could not be opened.  Sufficient replicas exist for
        the pool to continue functioning in a degraded state.
action: Attach the missing device and online it using 'zpool online'.
   see: http://www.sun.com/msg/ZFS-8000-2Q
  scan: resilvered 15.9M in 0h0m with 0 errors on Thu Dec 13 13:16:53 2012
config:

    NAME                      STATE     READ WRITE CKSUM
    zroot                     DEGRADED     0     0     0
      mirror-0                DEGRADED     0     0     0
        ada0p2                ONLINE       0     0     0
        17609975337436690025  UNAVAIL      0     0     0  was /dev/ada1p2

errors: No known data errors
```

As you've seen from the screenshot above Zabbix has detected the issue
with our ZFS pool and notified us, so that we can take an action for
fixing the issues.
