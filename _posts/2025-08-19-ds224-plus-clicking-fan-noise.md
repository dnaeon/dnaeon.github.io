---
layout: post
title: Fixing the clicking fan noise of Synology DS224+
tags: synology fan ds224+
---
Couple of days ago I got my brand new Synology `DS224+` disk station, which was
meant to replace my current (and old) Synology `DS213j` system.

My old `DS213j` system has served me well for the past ten years, and I have
been pretty happy with it. Synology also did an awesome job by providing
security updates for all these years to old devices such as the `DS213j` I
have. However, it was time for an upgrade.

After some researching I've decided to grab the `DS224+`, which is a great
improvement over my current `DS213j`. It runs with with 4 cores Intel Celeron
J4125 CPU and comes with 2GB of memory, and you have the option to upgrade the
memory to 6GB in total. And it also has dual-port NICs. Sweet.

For comparison my `DS213j` runs with a single core Marvell Kirkwood 1.2GHz and
has 512MB of memory. And it has a single NIC.

Like I've mentioned -- my `DS213j` has served me well for the past years, and I
still keep it around. One of the things that I really like about the `DS213j` is
how quiet it actually is.

So, after getting the `DS224+` and installing latest DSM 7.2.2 one thing
immediately caught my attention -- the noise coming from the fan. And that noise
was simply annoying and clicking. I've [recorded a video of the fan noise]({{ site.baseurl }}/files/ds224-plus-fan-noise.mp4),
coming out of my brand new DS224+, which you can also listen to.

The first I did was to make sure that the fan is configured in `Quiet Mode` and
to my surprise -- that was already the case.

[![]({{ site.baseurl }}/images/ds224plus-info.png)]({{ site.baseurl }}/images/ds224plus-info.png){:.glightbox}

Unfortunately, it turned out that I'm not the only one having such issues with
the DS224+. A number of people have already reported similar issues, which you
can find in the links below.

- [ds224+ fan noise is driving me crazy](https://www.reddit.com/r/synology/comments/1bwq0wi/ds224_fan_noise_is_driving_me_crazy/)
- [Ds224+ disk noise and slowness?](https://www.reddit.com/r/synology/comments/193vsjl/ds224_disk_noise_and_slowness/)
- [DS224+ fan noise at low speed](https://www.reddit.com/r/synology/comments/1bfwxa1/ds224_fan_noise_at_low_speed/)
- [DS224+ constant "clicking" sound that doesn't seem related to the drives](https://www.reddit.com/r/synology/comments/1kgf614/ds224_constant_clicking_sound_that_doesnt_seem/)

Interestingly, the only way you can make that annoying noise disappear is if you
simply switch from `Quiet Mode` to `Full-speed mode` in the `Fan Speed Mode`
settings. However, that's not what I really want to do.

The `DS213j` and `DS224+` while being different in terms of CPUs, architecture,
amount of memory, etc., are identical in one thing -- and that is the fan they
run with.

The [Synology Spare Parts](https://www.synology.com/en-global/products/spare_parts?search_by=category&category=Fan) page confirms that as well.

[![]({{ site.baseurl }}/images/syno-fan-parts.png)]({{ site.baseurl }}/images/syno-fan-parts.png){:.glightbox}

Back in the day I had to replace the fan of my old DS213j and I've used a
`Noctua NF-A9 FLX / 3 pins`, which to this day is still running. So, I thought
about doing the same with my DS224+ as well -- replace the stock fan with a
NF-A9 FLX 3-pin fan, however some people have reported issues when using this
fan in particular with DSM 7.2.x (check the links above for more details).

Another Noctua fan, for which people have reported good results in replacing
their stock fan is the `Noctua NF-B9-redux-1600, 92 x 92 x 25 mm / 3 pins`,
however I didn't have a spare one I could use immediately, and I had to order
one.

And while waiting for the NF-B9-redux-1600 to arrive I had to keep listening to
that annoying, clicking sound coming from the DS224+ stock fan. Needless to say,
I had time to tinker with the new DS224+ and see what can be done to fix this.

After some researching and testing various things I eventually found out about
`/usr/syno/etc.defaults/scemd.xml`, which is the configuration for the fan
curve. This file is parsed by the `scemd` service and is used to configure
various disk and CPU temperature thresholds, which when reached will configure
the fan to run either at low and high speeds, so that the system can cool down.

Comparing this file on my DS224+ with the same file on my old DS213j showed some
differences, which I expected anyways. However, having the exact same fan on
both systems, I would at least expect that the fans behave the same (unless one
of them is actually broken).

You can find the original `scemd.xml` files for DS213j and DS224+, which I've
extracted from my systems here.

- [DS213j scemd.xml]({{ site.baseurl }}/files/scemd-ds213j.xml)
- [DS224+ scemd.xml]({{ site.baseurl }}/files/scemd-ds224plus.xml)

Some time later I've also found a post from a folk at Reddit, who has provided
details about how he/she fixed the same annoying noise by adjusting the fan
curve profile in `/usr/syno/etc.defaults/scemd.xml`. You can find the
[post here](https://www.reddit.com/r/synology/comments/1lbvdp6/solution_for_ds224_fan_noise/).

I've tried the instructions he left and it actually made a difference on my
DS224+, however there was still a minor low frequency noise coming out of that
fan, which I was not happy with.

Then I've decided to simply adapt my existing DS213j config for DS224+. Turned
out it was pretty simple, and I didn't have to deal with any of the conversions
from frequency (`Hz`) to RPM values.

A snippet of changes I did to my `/usr/syno/etc.defaults/scemd.xml` file are
provided below.

``` xml
        <fan_config period="20" threshold="6" type="DUAL_MODE_HIGH" hibernation_speed="UNKNOWN">
		<disk_temperature fan_speed="ULTRA_LOW"  action="NONE">0</disk_temperature>
		<disk_temperature fan_speed="VERY_LOW"   action="NONE">33</disk_temperature>
		<disk_temperature fan_speed="LOW"        action="NONE">41</disk_temperature>
		<disk_temperature fan_speed="VERY_HIGH"  action="NONE">47</disk_temperature>
		<disk_temperature fan_speed="ULTRA_HIGH" action="NONE">53</disk_temperature>
		<disk_temperature fan_speed="ULTRA_HIGH" action="SHUTDOWN">61</disk_temperature>

		<cpu_temperature fan_speed="ULTRA_LOW"   action="NONE">0</cpu_temperature>
		<cpu_temperature fan_speed="LOW"         action="NONE">55</cpu_temperature>
		<cpu_temperature fan_speed="HIGH"        action="NONE">65</cpu_temperature>
		<cpu_temperature fan_speed="ULTRA_HIGH"  action="SHUTDOWN">95</cpu_temperature>
	</fan_config>
	<fan_config period="20" threshold="6" type="DUAL_MODE_LOW" hibernation_speed="UNKNOWN">
		<disk_temperature fan_speed="ULTRA_LOW"  action="NONE">0</disk_temperature>
		<disk_temperature fan_speed="VERY_LOW"   action="NONE">38</disk_temperature>
		<disk_temperature fan_speed="LOW"        action="NONE">46</disk_temperature>
		<disk_temperature fan_speed="HIGH"       action="NONE">52</disk_temperature>
		<disk_temperature fan_speed="VERY_HIGH"  action="NONE">55</disk_temperature>
		<disk_temperature fan_speed="ULTRA_HIGH" action="NONE">58</disk_temperature>
		<disk_temperature fan_speed="ULTRA_HIGH" action="SHUTDOWN">61</disk_temperature>

		<cpu_temperature fan_speed="ULTRA_LOW"   action="NONE">0</cpu_temperature>
		<cpu_temperature fan_speed="LOW"         action="NONE">55</cpu_temperature>
		<cpu_temperature fan_speed="HIGH"        action="NONE">65</cpu_temperature>
		<cpu_temperature fan_speed="ULTRA_HIGH"  action="SHUTDOWN">95</cpu_temperature>
	</fan_config>
```

You can also find the
[full updated scemd.xml file here]({{ site.baseurl }}/files/scemd-ds224plus-updated.xml).

Feel free to adjust the threshold temperatures for disk and CPU to other values,
if needed.

After you make changes to the `/usr/syno/etc.defaults/scemd.xml` file make sure
that you restart the `scemd` service.

``` shell
sudo systemctl restart scemd
```

And with these changes made to `scemd.xml` the clicking noise coming from the
DS224+ fan went away for me, so now I don't even have to replace it.
