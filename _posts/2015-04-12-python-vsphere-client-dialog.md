---
layout: post
title: Python vSphere Client with a dialog(1) interface
tags: python programming vmware virtualization
---
PVC is an interactive text-mode VMware vSphere Client with a
[dialog(1)](http://invisible-island.net/dialog/) interface for
GNU/Linux systems built on top of the
[pyVmomi](https://github.com/vmware/pyvmomi) VMware vSphere API
Python bindings.

PVC allows you to quickly navigate in your VMware vSphere
environment and perform common tasks against various VMware vSphere
Managed Entities, e.g. manage Datacenters and Clusters, create
Virtual Machines, launch consoles to Virtual Machines, manage
datastores on ESXi hosts, view and acknowledge alarms and many more.

[![]({{ site.baseurl }}/images/pvc-screenshot.jpg)]({{ site.baseurl }}/images/pvc-screenshot.jpg){:.glightbox}

You can get PVC from the
[PVC Github repository](https://github.com/dnaeon/pvc) and from the
[PVC PyPI page](https://pypi.python.org/pypi/pvc/).

Make sure to also check the
[documentation](http://pvc.readthedocs.org/en/latest/) for more
information about PVC.
