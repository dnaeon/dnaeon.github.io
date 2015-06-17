---
layout: post
title: Users and groups management with CFEngine 2
created: 1348582251
tags: cfengine automation
---
CFEngine 2 does not provide interfaces for direct manipulation of
users and groups on a UNIX/Linux system, but it provides the
primitives we need in order to create our own.

In order to manage our users/groups the commands that we will use are
already defined in `cfvariables.conf`

Here's a snippet of the defined commands in `cfvariables.conf`:

```text
control:

# Common variables: any

    # Variables for user and group management
    useradd                 = ( /usr/sbin/useradd )
    usermod                 = ( /usr/sbin/usermod )
    userdel                 = ( /usr/sbin/userdel )
    groupadd                = ( /usr/sbin/groupadd )
    groupmod                = ( /usr/sbin/groupmod )
    groupdel                = ( /usr/sbin/groupdel )
```

## Creating users and groups

In the below example snippet we create one user and one group if the
user/group do not exist on the system already on the systems from the
ftp_servers group:

```text
control:

classes:

# 
# Needed user and group for the FTP servers
#

user_virtual    = ( UserExists(virtual) )
group_sftp      = ( GroupExists(sftp) )

shellcommands:

ftp_servers.!user_virtual::
    
    "$(useradd) -c 'Virtual User for vsFTPd' -d /home/ftpsite -m -s /bin/bash -U virtual"
            
        useshell=false
        timeout=300     
        inform=true
            
ftp_servers.!group_sftp::
            
    "$(groupadd) sftp"
            
        useshell=false
        timeout=300
        inform=true
```
			
In the above snippet we use the UserExists and GroupExists functions
that define a class for us depending on the result of the functions.

In other words if the user virtual exists on the system then for that
system the class user `virtual` is defined.

In the shellcommands section we define the actual command for creating
user and group, which will be executed on the systems from the
ftp_servers group for which the user_virtual class is not defined,
meaning that the user does not exists yet.

## Removing users and groups

The above example for creating users can be modified this way, so that
now the user virtual and group sftp will be removed if they exist.

```text
control:

classes:

# 
# User and group to be removed from the FTP servers
#

user_virtual    = ( UserExists(virtual) )
group_sftp      = ( GroupExists(sftp) )

shellcommands:

ftp_servers.user_virtual::
  
    "$(userdel) virtual"
            
        useshell=false
        timeout=300     
        inform=true
            
ftp_servers.group_sftp::
            
    "$(groupdel) sftp"
            
        useshell=false
        timeout=300
        inform=true
```

In order to remove the user/group we need to have both classes
defined, which means that the user/group exists and then we can try to
remove it.

## Modifying users and groups

Modifying users and groups can be done using the $(usermod) and
$(groupmod) commands, which are already defined in `cfvariables.conf`

In the below snippet we modify the users shell to /bin/false thus not
allowing the user login access to the system.

```text
control:

classes:
# 
# User to modify
#

user_virtual    = ( UserExists(virtual) )

shellcommands:

ftp_servers.user_virtual::
    
    "$(usermod) -s /bin/false virtual"
            
        useshell=false
        timeout=300     
        inform=true
```

## Managing multiple users/groups

The above examples show how to manage a single user/group, but having
more users/groups to manage at once can become tedious to have a
`user_username = ( UserExists(username) )` for each user/group you
need to manage.

Instead we can make use of CFEngine's lists.

The below example snippets show how to create a number of users and
groups. These examples can be extended further to remove, modify,
etc..

```text
control:

classes:

    users_list = ( user1:user2:user3: )
    groups_list = ( group1:group2:group3 )

shellcommands:

	ftp_servers.!UserExists($(users_list))::

		"$(useradd) -s /bin/bash $(users_list)"
			
		useshell=false
		timeout=300
		inform=true

	ftp_servers.!GroupExists($(groups_list))::
			
		"$(groupadd) $(groups_list)"
		useshell=false
		timeout=300
		inform=true
```

The above example snippet will create the three users and groups on
the systems from the ftp_servers group, if these users/groups does not
exist already.

## More Information

More information on the topic can be found in the link below:

* [CFEngine - UserExists](http://cfengine.com/manuals/cf2-Reference.html#UserExists)
* [CFEngine - GroupExists](http://cfengine.com/manuals/cf2-Reference.html#GroupExists)
* [CFEngine - Iteration-over-lists](http://cfengine.com/manuals/cf2-Reference.html#Iteration-over-lists)
