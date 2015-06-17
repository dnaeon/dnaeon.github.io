---
layout: post
title: Enable KSM during boot-time on Linux
created: 1353671739
tags: linux virtualization kvm ksm
---
[Kernel Samepage Merging](http://www.linux-kvm.org/page/KSM) is a
cool feature of the Linux kernel, which you would really want to
have if you are running a virtualized environment, but you would also
benefit from it on a regular system as well. 

Having KVM hypervisors aroud I wanted to have KSM enabled, and you can
easily do that by echo'ing *1* to */sys/kernel/mm/ksm/run*, but the
downside of this is that it does not work accross reboots.

The other thing is that there is no kernel parameter that you can drop
in */etc/sysctl.conf* in order to enable KSM accross reboots. Looking
at the existing *init.d* scripts on my Debian Wheezy system I didn't
find any KSM related script neither.

So, what are our possibilities here to make this simply work? Looking
around on the Net I've seen a lot of people recommending to use
*/etc/rc.local*, but I'm not a big fan of */etc/rc.local*. In CentOS
there's a little script there in */etc/init.d/ksm* which does the job,
so I've decided to write my own version of it for Debian GNU/Linux.

Below you can find the script that I've used in order to enable KSM on
my Debian GNU/Linux systems upon reboots:

```bash
#!/bin/sh
#
# Author:           Marin Atanasov Nikolov <dnaeon@gmail.com>
#
### BEGIN INIT INFO
# Provides:          ksm
# Required-Start:    
# Required-Stop:
# X-Start-Before:    
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6 
# Short-Description: Enable and disable KVM KSM
# Description:       Enables and disables the KVM  Kernel Samepage Merging
#                    feature of the kernel
### END INIT INFO

set -e
. /lib/lsb/init-functions

do_start() {
	echo 1 > /sys/kernel/mm/ksm/run
	log_success_msg "Enabling Kernel Samepage Merging"
}
 
do_stop() {
	echo 0 > /sys/kernel/mm/ksm/run
	log_success_msg "Disabling Kernel Samepage Merging"
}
		
do_status() {
	local ksm_status
	ksm_status=$( cat /sys/kernel/mm/ksm/run )

	if [ ${ksm_status} -eq 1 ]; then
		log_success_msg "Kernel Samepage Merging is enabled"
	else
		log_success_msg "Kernel Samepage Merging is disabled"
	fi
}

case "${1}" in
	start)
		do_start
		;;
	reset|stop)
		do_stop
		;;
	status)
		do_status
		;;
	reload|restart|force-reload)
		do_stop
		do_start
		;;
	*)
		log_success_msg "usage: ${0} {start|stop|status|reload|restart|force-reload|reset}" >&2
		;;
esac
```

You can also find the latest version of this script at my Github
account on the link below:

* [https://github.com/dnaeon/ksm-init.d-debian](https://github.com/dnaeon/ksm-init.d-debian)

In order to use this script and have KSM enabled during boot-time,
drop the *ksm* script into your */etc/init.d* directory. Then enable
KSM *init.d* script during boot-time, by executing the command below:

```bash
$ sudo update-rc.d ksm defaults
update-rc.d: using dependency based boot sequencing
```

Here's how to enable KSM by using the *init.d* script:

```bash
$ sudo service ksm start
Enabling Kernel Samepage Merging.
```
	
Disabling KSM by using the *init.d* script:

```bash
$ sudo service ksm stop
Disabling Kernel Samepage Merging.
```
	
Getting status info by using the *init.d* script:

```
% sudo service ksm status
Kernel Samepage Merging is disabled.
```
