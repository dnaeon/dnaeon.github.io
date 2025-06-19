---
layout: post
title: Monitoring FreeBSD pkgng in Zabbix, Part II - Database Audits
created: 1356132672
tags: zabbix monitoring zabbix
---
In the first post about
[monitoring FreeBSD pkgng in Zabbix, Part I](/node/72), we've seen
how to get some nice graphs of the disk space usage and number of
packages installed on our systems. 

In this post we are going to extend our Zabbix template for *pkgng*
and add additional items to it for performing audits of our package
database.

The items we will add will allow Zabbix to detect missing package
dependencies on our *pkgng* systems and also inform us if our packages
are vulnerable while performing a security audit.

If you haven't created your Zabbix pkgng template yet, I'd recommend
you checking out the [Monitoring pkgng in Zabbix, Part I - Package
Statistics](/node/72) post first, which explains how to create the
template, application and items in your Zabbix server.

I'm considering that you already have the *Template App PKGNG*
template setup in your Zabbix server, so now we will proceed in
extending this template with a few more items.

First lets update our
*/usr/local/etc/zabbix2/zabbix_agentd.conf.d/pkgng.conf* file and add
two new *UserParameters*:

```text
UserParameter=pkg.check.deps,/usr/local/sbin/pkg check -da | awk 'END { print NR }'
UserParameter=pkg.audit,/usr/local/sbin/pkg audit | awk '/problem\(s\)/ { print $1 }'
```

Restart the Zabbix agent:

```bash
$ sudo service zabbix_agentd restart 
```

Login to your Zabbix server and navigate to your *Template App PKGNG*
template and click on the *Items* tab. Now create a new item, which is
called *Missing package dependencies*, which will check for.. well,
you know already what it will check for.

Here's a screenshot of our new *pkgng* item in Zabbix. The key we use
here is *pkg.check.deps*:

[![]({{ site.baseurl }}/images/pkgng-zabbix-part2-1.jpg)]({{ site.baseurl }}/images/pkgng-zabbix-part2-1.jpg){:.glightbox}

Lets add another item to our template, which will check our packages
against the vulnerability database for known issues. The key we use
for this item is *pkg.audit* as shown in the screenshot below:

[![]({{ site.baseurl }}/images/pkgng-zabbix-part2-2.jpg)]({{ site.baseurl }}/images/pkgng-zabbix-part2-2.jpg){:.glightbox}

Okay, items added. What's next?

What we are interested in here is that we want to know if our systems
have missing package dependencies or are known to be vulnerable. In
order to do that we are going to create *triggers* in Zabbix for our
items. So, navigate to the *Triggers* tab of your template and create
a new trigger. The first trigger we set is for missing package
dependencies, and you can see on the screenshot below how we configure
it:

[![]({{ site.baseurl }}/images/pkgng-zabbix-part2-3.jpg)]({{ site.baseurl }}/images/pkgng-zabbix-part2-3.jpg){:.glightbox}

For the name of the trigger we use:

```text
There were {ITEM.LASTVALUE} package(s) with missing dependencies detected
```

And the trigger's expression we use is:

```text
{Template App PKGNG:pkg.check.deps.last(0)}#0
```

Now lets add a second trigger for the vulnerability checks. Here's a
screenshot of the trigger:

[![]({{ site.baseurl }}/images/pkgng-zabbix-part2-4.jpg)]({{ site.baseurl }}/images/pkgng-zabbix-part2-4.jpg){:.glightbox}

And here's the trigger's expression we've used:

```text
{Template App PKGNG:pkg.audit.last(0)}#0
```

We are now ready with the Zabbix setup. Lets check if Zabbix has
something to tell us looking at the Dashboard.

[![]({{ site.baseurl }}/images/pkgng-zabbix-part2-5.jpg)]({{ site.baseurl }}/images/pkgng-zabbix-part2-5.jpg){:.glightbox}

As you can see from the above screenshot Zabbix has detected packages
with missing dependencies and it also identified one package that is
known to be vulnerable. But lets check if we really have these issues
on the system.

Lets see if we have any missing dependencies first:

```bash
$ pkg check -da
graphics/ImageMagick has a missing dependency: devel/dbus
devel/dbus-glib has a missing dependency: devel/dbus
devel/eggdbus has a missing dependency: devel/dbus
editors/emacs has a missing dependency: devel/dbus
devel/gconf2 has a missing dependency: devel/dbus
devel/libgsf has a missing dependency: devel/dbus
graphics/librsvg2 has a missing dependency: devel/dbus
sysutils/polkit has a missing dependency: devel/dbus
```

Okay that gives us eight packages with missing dependencies, so Zabbix
actually detected this successfully. Lets see now what is the
vulnerable package that Zabbix detected as well:

```bash
$ pkg audit                              
sudo-1.8.4_1 is vulnerable:
sudo -- netmask vulnerability

WWW: http://portaudit.FreeBSD.org/b3435b68-9ee8-11e1-997c-002354ed89bc.html

1 problem(s) in your installed packages found.
```

There's our vulnerable package. Time to update it packages.
