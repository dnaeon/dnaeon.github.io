---
layout: post
title: Disconnecting RDP sessions on a Windows(R) machine
tags: rdp windows
---

## General Information

Sometimes when connecting to a Windows(R) machine you might see an
error messsage saying that the maximum number of remote connections
has exceeded.

This happens when someone for example just closes the RDP session,
instead of properly logging off, in which case the session stays
active and blocks other people from logging in.

Since you cannot connect to the remote machine and disconnect the
stale remote connections, we will see how you can disconnect those
stale remote connections remotely from another Windows(R) machine.

## Requirements

* Administrator account or account with sufficient privileges on the
remote machine, that we want to disconnect all those stale connections.

## Connecting to the remote machine

List the RDP sessions on the remote machine:

```PowerShell
> qwinsta /server:example.com
```

You might have issues listing the already established RDP
sessions, and you might get access denied error.

So for example:

```PowerShell
> qwinsta /server:example.com
Error opening Terminal server example.com

Error [5]:Access is denied.
```

In such a case, connect to the remote machine's IPC channel and then
try again:

```PowerShell
> net use \\example.com\IPC$ /u:username
```

Where ``username`` is the user account that has privileges to
view and disconnect established RDP sessions.

```PowerShell
> net use \\example.com\IPC$ /u:administrator
The password or user name is invalid for \\example.com\IPC$.

Enter the password for 'administrator' to connect to 'example.com':

The command completed successfully.
```

Now you can view the established RDP sessions:

```PowerShell
> qwinsta /server:example.com
SESSIONNAME       USERNAME                 ID  STATE   TYPE        DEVICE
console                                     0  Conn    wdcon
rdp-tcp                                 65536  Listen  rdpwd
rdp-tcp#42        foo                       2  Active  rdpwd
```

Find the session you want to disconnect and then just execute the
following command:

```PowerShell
> rwinsta /server:example.com <session-id>
```

So for example if we want to disconnect user ``foo``'s RDP session
we would execute:

```PowerShell
> rwinsta /server:example.com 2
```

Executing the above command would disconnect user ``foo`` and make
possible for others to connect to the remote machine again.
