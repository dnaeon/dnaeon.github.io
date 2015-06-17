---
layout: post
title: Monitoring FreeBSD pkgng in Zabbix, Part I - Package Statistics
created: 1356132121
tags: monitoring zabbix freebsd
---
In this post I'm going to show you how to do monitoring of your
[FreeBSD pkgng-ready](http://wiki.freebsd.org/pkgng) system in Zabbix.

The purpose will be to show you how you can monitor package statistics
- number of packages & size of disk space usage.

In a separate post I'll show you how to detect issues in your package
database and check for know vulnerabilities.

The target system we use for monitoring here is
[FreeBSD](http://freebsd.org) and the package manager is
[pkgng](http://wiki.freebsd.org/pkgng).

I assume that you are already familiar with *pkgng* and have it
installed and configured on your system. But just in case you are one
of those people that are still stuck in the time and dealing with the
legacy *pkg_\** tools, my advise to you is to go ahead and install
*pkgng* right away! Seriously, you will love it! :)

Okay, lets do some work here, shall we?

First we need to add a new *UserParameter* in Zabbix, which will take
care of getting the package statistics for us. Later those statistics
will be used by Zabbix to create nice graphs of our *pkgng* package
database, because everybody loves graphs, right?

Create a new file under
*/usr/local/etc/zabbix2/zabbix_agentd.conf.d/pkgng.conf* (this assumes
that your *zabbix_agentd.conf* file includes the
*/usr/local/etc/zabbix2/zabbix_agentd.conf.d/* directory). The
contents of the *pkgng.conf* file are shown below:

```text
UserParameter=pkg.stats.number,/usr/local/sbin/pkg stats -l | awk '/Installed/ { print $3 }'
UserParameter=pkg.stats.diskspace,/usr/local/sbin/pkg stats -lb | awk '/Disk/ { print $4 }'
```

Restart the Zabbix agent:

```bash
$ sudo service zabbix_agentd restart 
```
	
We proceed now to the configuration in Zabbix. Login to your Zabbix
server and navigate to *Configuration* -> *Templates*, then click on
the *Create template* button and name your template as *Template App
PKGNG* and save it.

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-1.jpg)

Next we create a new application in our template, which is called
*PKGNG Checks*. Navigate to the *Applications* tab of your template
and create the application as shown in the screenshot below:

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-2.jpg)

Now it is time to add a few *items* in our template, so navigate to
the *Items* tab of your template and click the *Create item*
button. Here we are going to use the newly added *UserParameters*
we've added before.

On the screenshot below you can see the Zabbix item we add for
monitoring the number of packages - as the key we use
*pkg.stats.number*, the type of information is set to *Numeric
(unsigned)* and units are set to *packages*.

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-3.jpg)

And we will also add another item for monitoring the disk space usage occupied by our packages. On the screenshot below you can see the second item we add - as the key we use *pkg.stats.diskspace* and the type of information is *Numeric (unsigned)*, everything else should be left to the defaults.

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-4.jpg)

Lets create some nice graphs for our items now. Navigate to the
*Graphs* tab of your template and click the *Create graph*
button. Then name your graphs and select the corresponding items for
it, you've created previously.

Below you can see a screenshot of the graph we create for the number
of packages. Create the second graph for the disk space usage as well.

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-5.jpg)

The last thing that you need to do is to attach your *PKGNG Template*
to your FreeBSD systems and check your graphs.

I've taken some screenshots of my graphs while I was performing an
installation of a few packages. Below you can see the graph of disk
space usage occupied by our packages:

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-6.jpg)

And here's the graph of the number of packages installed on our
system:

![_config.yml]({{ site.baseurl }}/images/pkgng-zabbix-part1-7.jpg)

And that was all, folks. Hope you liked it!
