---
layout: post
title: vEvents - VMware vSphere Events from the command-line
created: 1401525266
tags: virtualization vmware python events
---
Another day, another Python project:
[vEvents](https://github.com/dnaeon/py-vevents) - an application that
allows you to view and monitor VMware vSphere Events from the
command-line.

`vEvents` uses the
[vConnector](https://github.com/dnaeon/py-vconnector) wrapper module
in order to retrieve and display VMware vSphere Events from the
command-line.

Requirements of `vEvents` are listed below:

* Python 2.7.x
* [docopt](https://github.com/docopt/docopt)
* [pyVmomi](https://github.com/vmware/pyvmomi)
* [vconnector](https://github.com/dnaeon/py-vconnector)

In order to install `vEvents` clone the [vEvents Github
repository](https://github.com/dnaeon/py-vevents) and execute the
command below:

```bash
$ sudo python setup.py install
```

Now, let's see an example of using `vEvents` in order to monitor the
VMware vSphere Events from the command-line.

First we configure our VMware vSphere host with `vConnector`, so that
`vEvents` can connect to our VMware vSphere host.

![_config.yml]({{ site.baseurl }}/images/vconnector-cli.jpg)

For more information about `vConnector` and how to use it, please
refer to the [vConnector Github repository
documentation](https://github.com/dnaeon/py-vconnector).

Having our VMware vSphere host registered in the `vConnector` database
we can now fire up `vEvents` and monitoring our VMware vSphere Events.

![_config.yml]({{ site.baseurl }}/images/vevents-cli.jpg)

For more information and updates of `vEvents`, please refer to the
[vEvents Github repository](https://github.com/dnaeon/py-vevents).
