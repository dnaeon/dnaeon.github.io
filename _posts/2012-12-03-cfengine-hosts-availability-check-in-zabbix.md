---
layout: post
title: CFEngine hosts availability check in Zabbix
created: 1354551405
tags: linux cfengine automation monitoring
---
Knowing that your CFEngine agents are complying to the promises is
nice to have in your monitoring system, if not even required. 

That way you can see if an Agent contains old policy files and does it
comply to the policies at all.

A simple way of how this can be accomplished is to use the CFEngine's
*lastseen* records, which will give you information of when an Agent's
last connection to the master server has been made.

In this post I'm going to show you how to use CFEngine's *lastseen*
records in order to trigger an alarm in Zabbix when an Agent didn't
check-in for the more than 24 hours.

First we need to make sure that our Agents are creating *lastseen*
reports. If you have followed the [CFEngine Handbook](/node/13) you
probably already have this configuration in place, but I'll post it
here as well for quick reference.

Please note that this configuration is not just FreeBSD related as the
handbook's title suggests, but you can also use it under GNU/Linux as
well. And here's the contents of *cf-report.cf* file:

```text
#########################################################################
#                                                                       #
#               cf-report.cf - Cfengine 3 Reports                       #
#                                                                       #
#########################################################################

body reporter control {
	reports           => { "performance", "last_seen", "monitor_history" };
	build_directory   => "$(sys.workdir)/reports";
	report_output     => "text";
}
```

Reports will go to the *reports* directory in the CFEngine's work
directory, which is in */var/cfengine/reports*. In order to generate
the reports, you need to execute *cf-report(8)*. To make sure that
reports are generated on hourly basis I choose to add a cron job for
it in */etc/crontab*, which looks like this:

```bash
5 * * * * root /var/cfengine/bin/cf-report
```

That was for the CFEngine part. Now we need to do some Zabbix
configurations. First we will add a new Zabbix *UserParameter* that
will take care of getting the hours since last connection to the
master policy server was made.

Create a new file under */etc/zabbix/zabbix_agentd.d/cfengine.conf*
(this assumes that your *zabbix_agent.conf* file includes the
*/etc/zabbix/zabbix_agentd.d/* directory). The contents of the
*cfengine.conf* file are shown below:

```text
# CFEngine keys
UserParameter=cfengine.lastseen,sed -e 's/.*(\(.*\)).*/\1/g' /var/cfengine/reports/lastseen.txt
```

Restart the Zabbix Agent:

```bash
$ sudo zabbix-agent restart
```

If access to */var/cfengine/reports* is restricted (recommended) you
should make sure the Zabbix user can get the report file, e.g. by
allowing *zabbix* user to run *sudo(8) sed* on
*/var/cfengine/reports/lastseen.txt* file.

Now it's time to add a new item to our Zabbix server. If you've seen
the [Monitoring CFEngine Services in Zabbix post](/node/70), we've
created a template CFEngine App and added a few items to it for
monitoring the CFEngine services.

Now we are going to extend our template app and add the *CFEngine
LastSeen* item to it. You might want to check the [Monitoring CFEngine
Services in Zabbix post](/node/70) for more details on the template
app we've created.

Now, login to your Zabbix server and navigate to *Configuration* ->
*Templates* and select the *Template App CFEngine* template we've
created previously. Next, go to the *Items* tab and create a new
item. The new item is called *CFEngine LastSeen*, which uses the key
*cfengine.lastseen* and the type of information is set to
*Numeric(float)*. See the screenshot below:

[![]({{ site.baseurl }}/images/zabbix-cfengine-img1.jpg)]({{ site.baseurl }}/images/zabbix-cfengine-img1.jpg){:.glightbox}

Now lets add a trigger for the newly added item. Navigate to the
*Triggers* tab and create a new trigger. On the screenshot below you
can see we are defining a trigger for our *CFEngine LastSeen* item
which will go into alarm if a host hasn't checked-in for more than 24
hours.

[![]({{ site.baseurl }}/images/zabbix-cfengine-img2.jpg)]({{ site.baseurl }}/images/zabbix-cfengine-img2.jpg){:.glightbox}

Save the trigger and that was it. From the example screenshot below
you can see that Zabbix properly catches the hosts that haven't
checked-in for more than 24 hours.

[![]({{ site.baseurl }}/images/zabbix-cfengine-img3.jpg)]({{ site.baseurl }}/images/zabbix-cfengine-img3.jpg){:.glightbox}

And that was it. Next time I will try to add a few more Zabbix posts
which will deal this time with package management and monitoring, so
stay tuned! :)
