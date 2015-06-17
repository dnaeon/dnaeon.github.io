---
layout: post
title: Monitoring CFEngine services with Zabbix
created: 1354537844
tags: linux cfengine automation monitoring
---
Continuing [our CFEngine saga](/tag/cfengine/) in this post I'm going
to show you how to do basic monitoring of the CFEngine's services in
Zabbix. 

In this post I'm going to show you how to do basic monitoring of the
CFEngine's services. For the purposes of this example we are going to
configure and start monitoring three basic CFEngine services -
CFEngine's *Execution Agent*, *Server Agent* and *Monitoring Agent*,
but you could also use the information in this post to configure the
rest of the CFEngine's services if needed.

Basic understanding and knowledge about Zabbix is required as well in
order to understand better the material in this post. To make things
clear I've also added screenshots of the different steps you need to
perform.

Now, lets proceed with out Zabbix setup, shall we?

We are going to create a template for our CFEngine services in Zabbix,
so later it would be easy to extend them and attach to existing
systems.

Now login to your Zabbix server and navigate to *Configuration* ->
*Templates* and then click on the *Create Template* button. Name your
template *Template App CFEngine* and click save as shown in the
example screenshot below:

![_config.yml]({{ site.baseurl }}/images/zabbix-cfengine-services-1.jpg)

Next we create a new application in our template, which is called
*CFEngine Services*:

![_config.yml]({{ site.baseurl }}/images/zabbix-cfengine-services-2.jpg)

Now we are going to create a few *items*, so go to the *Items tab* of
your template and click the *Create Item* button. On the screenshot
below you can see the item we've created for the *CFEngine Server
Agent*. As the key of our item we use *proc.num[cf-serverd]* which
will return the number of running *Server Agent* processes on the
system:

![_config.yml]({{ site.baseurl }}/images/zabbix-cfengine-services-3.jpg)

Add additional items like the one above for the *Execution Agent* and
the *Monitoring Agent*.

Once ready with that we will continue with the creation of *triggers*,
so go to the *Triggers* tab and click the *Create trigger*
button. Below you can see the trigger we are adding for the *Server
Agent* item.

What the trigger basically does is that it checks the result of the
last check and if it is zero, this means that we have no processes
running and our service is down.

![_config.yml]({{ site.baseurl }}/images/zabbix-cfengine-services-4.jpg)

Add triggers for the *Execution Agent* and *Monitoring Agent* as well
similar to the one shown above.

And that was it more or less. The last remaining step is to attach the
*Template App CFEngine* template to a system which is running the
CFEngine services and you are good to go.

On the screenshot below you can see Zabbix detecting the first issues
on our systems:

![_config.yml]({{ site.baseurl }}/images/zabbix-cfengine-services-5.jpg)

Hope that was useful. In a future post I will show you how to check
for hosts availability in CFEngine using Zabbix.
