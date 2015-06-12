---
layout: post
title: Installing and configuring Tinderbox on FreeBSD
tags: freebsd tinderbox
---

Tinderbox is a package building system system, which we will be using
in order to build our packages and then install them to our FreeBSD
hosts and jails.

The information you will find below more or less is part already of the
[Tinderbox README](http://tinderbox.marcuscom.com/README/index.html),
although you will find some additional information here as well.

If you already have Tinderbox installed and configured, you may safely
skip this chapter and proceed with the next one, where we take a look
into CFEngine 3.

Please consider checking the
[documentation of Tinderbox](http://tinderbox.marcuscom.com/README/index.html)
in order to understand how Tinderbox works and what you can do with it.

Assuming you have installed Apache webserver with PHP support already,
as it is listed in the requirements, let's go ahead and
install Tinderbox.

```shell
$ cd /usr/ports/ports-mgmt/tinderbox-devel && sudo make install clean
```

![_config.yml]({{ site.baseurl }}/images/tinderbox-config.jpg)

NOTE: Please, note that `lsof(8)` option requires that you have the
kernel sources to be present on your system.

You can safely leave the options as they are, and continue with the
port build.

Once the installation of Tinderbox is over, you should see
something similar:

```shell
===============================================================================
ports-mgmt/tinderbox is now installed, but it requires some additional setup.

****************************************************
Please do read: /usr/local/tinderbox/scripts/README
****************************************************

=============================================================================

If you installed the port with WITH_TMPFS option:
To enable the usage of FreeBSD's tmpfs implementation you need to add the
tmpfs kernel module to your /boot/loader.conf:
tmpfs_load="YES"

If you installed the port WITH_PARALLEL option:
The parallel patch allows to run multiple tinderd instances at the same time.
You can enable multiple tinderd instances via /etc/rc.conf with:
tinderd_instances="N"

Both patches are experimental and are not official supported by the Tinderbox
distribution.

=============================================================================


The following walkthrough is the webserver setup, if you installed the WebUI:

- In your Apache configuration add the following lines:

  Alias /tb/logs/ "/usr/local/tinderbox/logs/"
  Alias /tb/packages/ "/usr/local/tinderbox/packages/"
  Alias /tb/errors/ "/usr/local/tinderbox/errors/"
  Alias /tb/wrkdirs/ "/usr/local/tinderbox/wrkdirs/"
  Alias /tb/ "/usr/local/tinderbox/scripts/webui/"
  <Directory "/usr/local/tinderbox/">
      Order allow,deny
      Allow from all
  </Directory>

- In your Lighttpd configuration:

Turn on "mod_alias" and add the following lines:

alias.url = (   "/tb/logs/" => "/usr/local/tinderbox/logs/",
                "/tb/packages/" => "/usr/local/tinderbox/packages/",
	        "/tb/errors/" => "/usr/local/tinderbox/errors/",
	        "/tb/wrkdirs/" => "/usr/local/tinderbox/wrkdirs/",
	        "/tb/" => "/usr/local/tinderbox/scripts/webui/" )
dir-listing.activate = "enable"


- In your Hiawatha configuration:

Alias is a built in command, no need to load any special mod

Alias = /tb/logs:/usr/local/tinderbox/logs
Alias = /tb/packages:/usr/local/tinderbox/packages
Alias = /tb/errors:/usr/local/tinderbox/errors
Alias = /tb/wrkdirs:/usr/local/tinderbox/wrkdirs
Alias = /tb/:/usr/local/tinderbox/scripts/webui

  Check your system by going to http://localhost/tb/

=============================================================================

===>   Compressing manual pages for tinderbox-devel-3.4.20110101
===>   Registering installation for tinderbox-devel-3.4.20110101
```

## Configuration of Tinderbox

Assuming you have installed and configured Apache as described in the
[Apache handbook](/node/15), we will now do some updates to the
Apache configuration, so that Tinderbox is our default Apache vhost.

To do that, you need to update the
`/usr/local/etc/apache22/extra/httpd-vhosts.conf` file.
Here is how my configuration file looks like:

```ApacheConf
<VirtualHost *:80>
    ServerAdmin admin@example.org
    ServerName tinderbox.example.org
    ServerAlias tinderbox.example.org

    Alias /tb/logs/ "/usr/local/tinderbox/logs/"
    Alias /tb/packages/ "/usr/local/tinderbox/packages/"
    Alias /tb/errors/ "/usr/local/tinderbox/errors/"
    Alias /tb/wrkdirs/ "/usr/local/tinderbox/wrkdirs/"
    Alias /tb/ "/usr/local/tinderbox/scripts/webui/"
    
    <Directory "/usr/local/tinderbox/">
        Order allow,deny
        Allow from all
    </Directory>

    ErrorLog "/var/log/apache/httpd-tinderbox.example.org-error.log"
    CustomLog "/var/log/apache/httpd-tinderbox.example.org-access.log" common
</VirtualHost>
```

Test the Apache configuration for errors:

```shell
$ sudo apachectl configtest
```

If the output from the above command is `Syntax OK`,
then reload Apache's configuration, otherwise you will need to check
your configuration file again for errors.

```shell
$ sudo apachectl graceful
```

## Database configuration for Tinderbox

In this step we will create the MySQL database for Tinderbox.
	
Assuming you have read the
[handbook for installing and configuring MySQL](/node/2), we will now
create the database for Tinderbox.

Login to the MySQL server:

```SQL
$ mysql -u root -h mysql.example.org -p
Enter password: 
Welcome to the MySQL monitor.  Commands end with ; or \g.
Your MySQL connection id is 55412
Server version: 5.0.91-log FreeBSD port: mysql-server-5.0.91
 
Type 'help;' or '\h' for help. Type '\c' to clear the current input statement.
 
mysql>
```

Now, create the database and user for Tinderbox:

```SQL
mysql> CREATE DATABASE tinderbox DEFAULT CHARSET UTF8;
Query OK, 1 row affected (0.00 sec)

mysql> GRANT SELECT, INSERT, DELETE, UPDATE, INDEX, DROP, CREATE ON tinderbox.* to tinderbox_user@'tinderbox.example.org' IDENTIFIED BY '<password>';
Query OK, 1 row affected (0.00 sec)

mysql> SET PASSWORD FOR tinderbox_user@'tinderbox.example.org' = PASSWORD('<password>');
Query OK, 1 row affected (0.00 sec)

mysql> FLUSH PRIVILEGES;
Query OK, 1 row affected (0.00 sec)
```

We now have the Tinderbox database and user created, and later we will
populate the database with the Tinderbox schema.

## Configuration of Tinderbox itself

Now it is time to configure Tinderbox itself.

As in the
[Tinderbox README](http://tinderbox.marcuscom.com/README/index.html),
we will use the *${pb}* (package build) environmental variable to refer
to the top-level directory of Tinderbox.

By default
[ports-mgmt/tinderbox-devel](http://www.freshports.org/ports-mgmt/tinderbox-devel/)
installs in */usr/local/tinderbox*, which is our *${pb}*.

## Setup of configuration files and database initilialization

First we will setup the configuration files and initialize the
Tinderbox database.

```shell
$ cd ${pb}/scripts && sudo ./tc Setup
```

Press `ENTER` when asked:

```Text
Does this host have access to connect to the Tinderbox database as a database administrator? (y/N)
```

## Web front-end configuration of Tinderbox

Now let's configure the web front-end of Tinderbox:

```shell
$ cd ${pb}/scripts
$ sudo cp webui/inc_ds.php.dist webui/inc_ds.php
```

Open the `webui/inc_ds.php` file for editing and update it to reflect
your actual database settings:

```php
<?php

$DB_DRIVER  = 'mysql';
$DB_HOST    = 'mysql.example.org';
$DB_NAME    = 'tinderbox';
$DB_USER    = 'tinderbox_user';
$DB_PASS    = '<password>';

?>
```

Setup the `ds.ph` configuration file:

```shell
$ cd ${pb}/scripts
$ sudo cp ds.ph.dist ds.ph
```

Open the `ds.ph` file for editing and update it to reflect your
actual database settings:

```perl
$DB_DRIVER       = 'mysql';
$DB_HOST         = 'mysql.example.org';
$DB_NAME         = 'tinderbox';
$DB_USER         = 'tinderbox_user';
$DB_PASS         = '<password>';
$DBI_TYPE        = 'database';

1;
```

## Database schema creation of Tinderbox
	
Now let's create the Tinderbox database schema.

```shell
$ cd ${pb}/scripts/sql
$ ./genschema mysql | mysql -u tinderbox_user -p -h mysql.example.org tinderbox
```

In the above command:

* **tinderbox_user** - is the the username which we created for Tinderbox
* **mysql.example.org** - is the MySQL server name
* **tinderbox** - is the database name

## Setup of tinderbox.ph and inc_tinderbox.php

```shell
$ cd ${pb}/scripts
$ sudo cp tinderbox.ph.dist tinderbox.ph
```

Now, open `tinderbox.ph` for editing, make the needed changes, and
save the file.

Here's mine `tinderbox.ph` file:

```perl
# Configurable options
$TINDERBOX_HOST  = 'http://tinderbox.example.org';
$TINDERBOX_URI   = '/tb';
$SUBJECT         = '[ Tinderbox at example.org ] :';
$SENDER          = 'tinderbox@example.org';
$SMTP_HOST       = 'mail.example.org';

# These should probably be left alone
$LOGS_URI        = $TINDERBOX_URI . '/logs';
$SHOWBUILD_URI   = $TINDERBOX_URI . '/index.php?action=list_buildports&build=';
$SHOWPORT_URI    = $TINDERBOX_URI . '/index.php?action=describe_port&id=';

1;
```

## Further configuration of the web front-end

Further configuration of the web frontend of Tinderbox is done
from the `${pb}/webui/inc_tinderbox.php` file.

```shell
$ cd ${pb}/scripts/webui
$ sudo cp inc_tinderbox.php.dist inc_tinderbox.php
```

Open the `inc_tinderbox.php` for editing and make the
needed changes - page title, page header, theme, etc..

## Initialize the Tinderbox

Now, let's initialize the Tinderbox:

```shell
$ cd ${pb}/scripts && sudo ./tc init
```

## Creating Tinderbox jails

A Tinderbox jails is just a given world in chroot.

Tinderbox can download the FreeBSD sources via `csup(1)` and then
do a `make world` in order to build a complete Tinderbox jail.

Another way of creating the Tinderbox jails is to use binary releases,
in which case Tinderbox will download the binary release sets, and then
install them.

NOTE: In order to use the binary release sets, you will need to have
[ftp/lftp](http://www.freshports.org/ftp/lftp/) installed.

```shell
$ cd ${pb}/scripts && sudo ./tc createJail -j 8.2 -d "FreeBSD 8.2-RELEASE" -t 8.2-RELEASE -u LFTP -H ftp.freebsd.org
```

In order to build a Tinderbox jail from sources, use the following
form of creating the Tinderbox jails:

```shell
$ cd ${pb}/scripts && sudo ./tc createJail -j 8.2 -d "FreeBSD 8.2-RELEASE" -t RELENG_8_2 -u CVSUP
```

The `-t` flag here specifies the CVS tag to use when checking out the
FreeBSD sources, while in the binary method `-t` specifies the
FreeBSD release.

Please check the
[Tinderbox README](http://tinderbox.marcuscom.com/README/index.html)
for more information on creating Tinderbox jails.

## Building i386 packages on amd64 machine

If you are running Tinderbox on a `amd64` machine and want to be able
to build `i386` packages, here is how to do it.

When creating the Tinderbox jail use the `-a` flag to specify the
arch of the Tinderbox jail.

So, for example if you want to create an i386 Tinderbox jail on a
amd64 machine, you will create the Tinderbox jail like this:

Using binary release sets:

```shell
$ cd ${pb}/scripts && sudo ./tc createJail -j 8.2-i386 -d "FreeBSD 8.2-RELEASE" -t 8.2-RELEASE -u LFTP -H ftp.freebsd.org -a i386
```

Using sources:

```shell
$ cd ${pb}/scripts && sudo ./tc createJail -j 8.2-i386 -d "FreeBSD 8.2-RELEASE" -t RELENG_8_2_0_RELEASE -u CVSUP -a i386
```

After creating the Tinderbox jail, we need to set the `$ARCH`
environment variable in the jail to `i386` as well.

To do this create a file that will keep all the environment variables
of the jail and add `ARCH=i386` in it.
The file should be placed in `${pb}/scripts/etc/env/jail.<TinderboxJail>`

So for example, if you've created a jail named `8.2-i386`, then your
`${pb}/scripts/etc/env/jail.8.2-i386` file will look like this

```shell
ARCH=i386
```

If you wish to define other environmental variables for a jail, you
should put them in the above file as well.

## Creating a Ports Tree

The next thing to do is to create a Ports Tree for our Tinderbox jails.

```shell
$ cd ${pb}/scripts && sudo ./tc createPortsTree -p FreeBSD -d "FreeBSD Ports Tree" -w http://www.freebsd.org/cgi/cvsweb.cgi/ports/
```

The above command creates a Ports Tree that tracks the FreeBSD Ports Tree.

## Creating Tinderbox builds

In Tinderbox a build is called the combination of a jail and a ports
tree. This is where packages are being built as well.

Now let's create a build that combines in itself the 8.2 FreeBSD jail
we've created and the full FreeBSD Ports Tree.

```shell
$ cd ${pb}/scripts
$ sudo ./tc createBuild -b 8.2-FreeBSD-i386 -j 8.2-i386 -p FreeBSD -d "8.2-RELEASE-i386 with FreeBSD Ports Collection"
```

In this step you should create the builds for all jails with
Ports Trees you've created in the previous steps.

## Configuration of tinderd for port build scheduling

Tinderbox uses `tinderd` - a daemon that stays in the background and
checks whether or not there are new ports, that need to be built.

This allows us to use the web interface of Tinderbox, so that we can
add/queue ports for building.

First let's create a user that we'll use to login to the web interface.

```shell
$ cd ${pb}/scripts && sudo ./tc addUser -u {USER} -e {EMAIL} -p {PASSWORD} -w
$ cd ${pb}/scripts && sudo ./tc setWwwAdmin -u {USER}
```

In order to enable `tinderd` during boot-time, add the following lines
to your `/etc/rc.conf` file:

```shell
# Enable Tinderbox's tinderd daemon
tinderd_enable="YES"
tinderd_directory="/usr/local/tinderbox/scripts"
tinderd_flags="-nullfs"
```

Now start the `tinderd` daemon:

```shell
$ sudo /usr/local/etc/rc.d/tinderd start
```

## Caching of distfiles

In order to enable the caching of distfiles, execute the commands below:

```shell
$ sudo mkdir /usr/local/tinderbox/distfiles
$ cd ${pb}/scripts && sudo ./tc configDistfile -c /usr/local/tinderbox/distfiles
```

## Using Tinderbox

In order to start building a package and track it in the Tinderbox
database, you can either use the web interface of Tinderbox to
schedule a port build, or you can do this from the command-line.

In order to build a port from the command-line, first add the
port to the Tinderbox database.

```shell
$ cd ${pb}/scripts && sudo ./tc addPort -b {BUILD} -d {PORT DIRECTORY}
```

Now that the port is added to the database, we can build it
using the `tinderBuild` command.

```shell
$ cd ${pb}/scripts && sudo ./tc tinderbuild -nullfs -b {BUILD} {PORT DIRECTORY}
```

Again, you are advised to check the
[official Tinderbox documentation](http://tinderbox.marcuscom.com/README/index.html),
for more information on Tinderbox and other features, which you can
enable like for example ccache, port options, etc.

When a new version of a port is available and we schedule a build for
it in Tinderbox, the new build will actually remake the port, thus
removing the old version and building the new one.

What we would want to have is to keep different versions of the
package available, so that we have the choice of installing a specific
version of a package.

One way to do this is to use the Tinderbox hooks, and run a script
after a port has been successfully built.

Another way is to add a cron job that rsyncs the packages to your
local FTP server. In my case I'm using an rsync script that runs
each hour and syncs the Tnderbox packages to a local FTP server.
