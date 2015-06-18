---
layout: post
title: Run commands inside VMware Virtual Machine using Python
tags: python programming
---
In this post we will see how we can use the
[Python vSphere API bindings](https://github.com/vmware/pyvmomi)
in order to login to a VMware Virtual Machine and run some commands.

There has been a
[request on the pyVmomi issue
tracker](https://github.com/vmware/pyvmomi/issues/22)
for creating an example script that would login to a guest system, so
here it is now.

Any operations related to a guest Operating System, files and
processes are performed via the
[GuestOperationsManager](http://pubs.vmware.com/vsphere-55/index.jsp#com.vmware.wssdk.apiref.doc/vim.vm.guest.GuestOperationsManager.html#field_detail)
managed entity, so we will use that in order to get a list of all
processes running in our Virtual Machine.

And here is the Python script that would perform this task.

```python
from __future__ import print_function

import pyVmomi

from pyVim.connect import SmartConnect


def main():
    si = SmartConnect(
        user='root',
        pwd='password',
        host='vc01'
    )

    # Credentials used to login to the guest system
    creds = pyVmomi.vim.vm.guest.NamePasswordAuthentication(
        username='guest_user',
        password='guest_password'
    )

    # Get a view ref to all VirtualMachines
    view_ref = si.content.viewManager.CreateContainerView(
        container=si.content.rootFolder,
        type=[pyVmomi.vim.VirtualMachine],
        recursive=True
    )

    # Pick up the first VM
    vm = view_ref.view[0]
    print("VM Name: {}".format(vm.name))

    # Get VM processes
    processes = si.content.guestOperationsManager.processManager.ListProcessesInGuest(
        vm=vm,
        auth=creds
    )

    # Print some process info
    print("Process name: {}".format(processes[0].name))
    print("Process owner: {}".format(processes[0].owner))
    print("Process PID: {}".format(processes[0].pid))


if __name__ == '__main__':
    main()
```
