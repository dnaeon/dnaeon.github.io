---
layout: post
title: Installing and configuring vsftpd on FreeBSD
created: 1294916389
tags: freebsd ftp pf
---
In this handbook we will see how to install and configure the
Very Secure FTP Daemon - [vsftpd](http://vsftpd.beasts.org/)

As the official website states, vsftpd is a fast and secure FTP
server and is used by a lot of big projects and companies.

Please read more about it, on the
[official site of vsftpd](http://vsftpd.beasts.org/).

In this documentation we will be creating a vsftpd installation using
virtual users, meaning that we will have one UNIX account named
`virtual`, and all our FTP accounts will be mapped to that UNIX
account, so in fact we won't be creating any local accounts on the
machine the FTP server will be running on.

During the preparation of this handbook I needed a private,
shared, non-anonymous FTP server, which allows
download/upload capabilities to my FTP users from a single location.

If you need to create an anonymous read-only FTP server,
please refer to the
[official site of vsftpd](http://vsftpd.beasts.org/),
which contains a lot of examples of how to create different setups for
your vsftpd installation.

The official website also contains examples on how to create virtual
users setup, which allows different users to have their own
home folders. 

In this document we will cover only how to create a private
shared FTP server, which requires authentication in order to
download/upload files.

This setup has been tested and works fine on FreeBSD 8.1 and
vsftpd version 2.3.2

## Requirements

* root access or sudo rights

## Installation

In order to install `vsftpd`, we will be using the
[FreeBSD Ports Collection](http://www.freebsd.org/doc/handbook/ports-using.html).

To install vsftpd from ports, execute the following command:

```bash
$ cd /usr/ports/ftp/vsftp && sudo make install clean
```

When you execute the above command you should see the following
screen, allowing you to select additional options for vsftpd.

You can safely leave the options as they are and continue with the
installation.

[![]({{ site.baseurl }}/images/vsftpd-config.jpg)]({{ site.baseurl }}/images/vsftpd-config.jpg){:.glightbox}

Once the installation is over you should see something similar:

```text
===> Installing rc.d startup script(s)
===>   Compressing manual pages for vsftpd-2.3.2
===>   Registering installation for vsftpd-2.3.2
===> SECURITY REPORT: 
      This port has installed the following files which may act as network
      servers and may therefore pose a remote security risk to the system.
/usr/local/libexec/vsftpd

      This port has installed the following startup scripts which may cause
      these network services to be started at boot time.
/usr/local/etc/rc.d/vsftpd

      If there are vulnerabilities in these programs there may be a security
      risk to the system. FreeBSD makes no guarantee about the security of
      ports included in the Ports Collection. Please type &#39;make deinstall&#39;
      to deinstall the port if this is a concern.

      For more information, and contact details about the security
      status of this software, see the following webpage: 
http://vsftpd.beasts.org/
===>  Cleaning for vsftpd-2.3.2
```

In FreeBSD in order to be able to authenticate FTP users
properly, we also need the `security/pam_pwdfile` port to be
installed, so let's install it too:

```bash
$ cd /usr/ports/security/pam_pwdfile && sudo make install clean
```

The installation of `pam_pwdfile` is pretty straightforward and
should take a couple of seconds to complete.

Once the installation is over you should see something similar:

```text
===>  Installing for pam_pwdfile-0.99_1
===>   Generating temporary packing list
===>  Checking if security/pam_pwdfile already installed
install -s -o root -g wheel -m 444     pam_pwdfile.so /usr/local/lib
===>   Registering installation for pam_pwdfile-0.99_1
===>  Cleaning for pam_pwdfile-0.99_1
```

We are now ready with the installation, so let's go ahead and
configure vsftpd.

## Configuration of vsftpd

First we will configure vsftpd, so it is able to authenticate
our FTP users - the information about the FTP users will be
stored in the `/usr/local/etc/vsftpd_login.db` file, which we will
later populate with some user accounts.

Now, create the `/etc/pam.d/vsftpd` file, which contains the
following lines:

```text
auth required /usr/local/lib/pam_pwdfile.so pwdfile /usr/local/etc/vsftpd_login.db
account required /usr/lib/pam_permit.so
```

Let's create the `virtual` user for our vsftpd setup:

```bash
$ sudo adduser -v
Username: virtual
Full name: Virtual FTP user
Uid (Leave empty for default):
Login group [virtual]:
Login group is virtual. Invite virtual into other groups? []:
Login class [default]:
Shell (sh csh tcsh bash rbash nologin) [sh]: nologin
Home directory [/home/virtual]:
Use password-based authentication? [yes]:
Use an empty password? (yes/no) [no]:
Use a random password? (yes/no) [no]:
Enter password:
Enter password again:
Lock out the account after creation? [no]:
Username   : virtual
Password   : *****
Full Name  : Virtual FTP user
Uid        : 1007
Class      :
Groups     : virtual
Home       : /home/virtual
Shell      : /usr/sbin/nologin
Locked     : no
OK? (yes/no): yes
adduser: INFO: Successfully added (virtual) to the user database.
Add another user? (yes/no): no
Goodbye!
```

We can now configure vsftpd, which keeps it's configuration
data in the `/usr/local/etc/vsftpd.conf` file.

NOTE: Below is just a sample configuration file that I've used for my
private FTP server. Please refer to the manual pages of
`vsftpd(8)` and `vsftpd.conf(5)` for more information about the
configuration options that you might want to include.

```text
anonymous_enable=NO
anon_upload_enable=YES
anon_mkdir_write_enable=YES
anon_other_write_enable=YES
anon_world_readable_only=NO

listen=YES
background=YES
listen_address=x.x.x.x # change this to the IP address vsftpd will be listening on
listen_port=21 # change this to whatever port you wish

max_clients=200 # change these to whatever you wish
max_per_ip=5

write_enable=NO
local_enable=YES
pam_service_name=vsftpd

pasv_min_port=50000 # change these too if you have a firewall running
pasv_max_port=50999 

xferlog_enable=YES

chroot_local_user=YES
secure_chroot_dir=/usr/local/share/vsftpd/empty/

guest_enable=YES
guest_username=virtual

ls_recurse_enable=NO
ascii_download_enable=NO
ascii_upload_enable=NO
```

## Adding users to vsftpd

In order to create a user for our vsftp setup we will use the
`htpasswd` tool, and we will keep the user details in the
`/usr/local/etc/vsftpd_login.db` file.

In order to create the password database and create a user, use the
following command:

```bash
$ sudo htpasswd -c -b /usr/local/etc/vsftpd_login.db USERNAME PASSWORD
```

Secure the password file:

```bash
$ sudo chmod 0600 /usr/local/etc/vsftpd_login.db
```

In order to add new users, after you've created the password
database, use the following command:

```bash
$ sudo htpasswd -b /usr/local/etc/vsftpd_login.db USERNAME PASSWORD
```

## Starting vsftpd

In order to start vsftpd, execute the following command:

```bash
$ sudo /usr/local/etc/rc.d/vsftpd start
```

If you want to start vsftpd during boot-time, add the
following line to your `/etc/rc.conf` file:

```bash
vsftpd_enable="YES"
```

# Allowing FTP traffic through your firewall

Even though vsftpd is secure enough, just like any other service I
wish to enable on my network, I prefer to isolate it in a jail
environment.

It might not be your case, but in most of the cases your FTP server
will be running on a machine behind a firewall, and if this is the
case you need to enable the FTP traffic through your firewall to the
internal FTP server.

Below you can find sample [PF](http://www.openbsd.org/faq/pf/) rules
that you can use in order to pass traffic to and from your
firewall to the internal FTP server.

```text
# --- redirect ftp traffic to the internal ftp server ---
rdr on $ext_if proto tcp from any to $ext_if port $FTP_PORT -> $FTP_SERVER port $FTP_PORT
rdr on $ext_if proto tcp from any to $ext_if port 50000:50999 -> $FTP_SERVER port 50000:50999

# --- pass incoming ftp traffic ---
pass in quick on $ext_if inet proto tcp from any to $FTP_SERVER port $FTP_PORT keep state
pass in quick on $ext_if inet proto tcp from any to $FTP_SERVER port 50000:50999 keep state
```

While keeping in mind that you need to set in your `/etc/pf.conf`
file the following macros:

* `ext_if` - the external interface
* `FTP_PORT` - the FTP port number your vsftpd server is listening on
* `FTP_SERVER` - the IP address of the ftp server in your internal network<
* `50000:50999` - this is the range of ports for passive FTP connections,
  that you set in your `vsftpd.conf` file
