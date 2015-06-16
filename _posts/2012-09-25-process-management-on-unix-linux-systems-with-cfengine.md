---
layout: post
title: Process management on UNIX/Linux systems with CFEngine
created: 1348581279
tags: cfengine automation
---
Another nice feature of CFEngine is that you are able to do process
accounting as well.

The examples below show different cases of process control in
CFEngine 2.

The below snippet shows how to start up SSH on machines, which are
detected not to run the SSH daemon.

```text
processes:

openft_servers::

	"/usr/sbin/sshd$"
     
	 restart '/usr/sbin/sshd -f /etc/ssh/sshd_config'
	 useshell=true
	 owner=root
	 group=root
	 inform=true

SetOptionString "auxw"
```

What the above snippet does is to simply check if the sshd process is
running and:

* If the process is running - nothing is done.
* If the process is NOT running that the command defined by the
  "restart" clause is executed.

The *SetOptionString* defines the argument list to be passed to the
`ps(1)` command when searching for the processes.

Another example for process control, where we only want to restart a
daemon process when the configuration changes is listed below.

```text
editfiles:

   openft_servers::

   { $(sshd_config)

        Inform 'on'

        ReplaceAll '^Subsystem sftp /usr/lib/openssh/sftp-server$' With 'Subsystem sftp internal-sftp'

        BeginGroupIfNoLineMatching '^Match Group sftp$'
            IncrementPointer '1'
            Append 'Match Group sftp'
            Append '   ChrootDirectory /home/chroot'
            Append '   X11Forwarding no'
            Append '   AllowTcpForwarding no'
            Append '   ForceCommand internal-sftp'
        EndGroup

        DefineClasses 'ssh_kill_hup'
   }

processes:

openft_servers.ssh_kill_hup::

      "/usr/sbin/sshd$"
         restart '/usr/sbin/sshd -f /etc/ssh/sshd_config'
         useshell=true
         owner=root
         group=root
         inform=true

SetOptionString "auxw"
```
	
The above snippet does the following:

* Checks the SSH configuration file - `/etc/ssh/sshd_config` if it
  contains the following lines, and if they are missing it adds them:

```text
Match Group sftp
	ChrootDirectory /home/chroot
	X11Forwarding no
	AllowTcpForwarding no
	ForceCommand internal-sftp
```

* If the configuration file of SSH is updated then a SIGHUP signal is
  being sent to the SSH daemon process.
* If the SSH daemon process is not running, then the
  daemon is started up

It is important to pay attention to the process
`class openft_servers.ssh_kill_hup`.

This class ensures that the ssh_kill_hup command will be executed only
on the openft_servers, which is what we wanted actually.

## More Information

You can find more information about the process control in CFEngine 2
in the link below:

* http://cfengine.com/manuals/cf2-Reference.html#processes
