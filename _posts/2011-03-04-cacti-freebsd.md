---
layout: post
title: Installing and configuring Cacti on FreeBSD
tags: freebsd monitoring
---
In this handbook we will have a look at Cacti - The complete RRDTool
based graphing solution.

This handbook will not teach how to work with Cacti, doing so would
only mean to duplicate the great documentation prepared for you by the
Cacti team.

Instead here we will see how to install and configure Cacti on your
FreeBSD machine, so you can use it for monitoring any device you have
on your network.

So what is Cacti? Citing from the [official web site of
Cacti](http://www.cacti.net/):

*Cacti is a complete frontend to RRDTool, it stores all of the
 necessary information to create graphs and populate them with data in
 a MySQL database. The frontend is completely PHP driven. Along with
 being able to maintain Graphs, Data Sources, and Round Robin Archives
 in a database, cacti handles the data gathering. There is also SNMP
 support for those used to creating traffic graphs with MRTG.*

You are advised to check and go through the [Cacti Online
Documentation](http://www.cacti.net/documentation.php) for a
comprehensive explaination of Cacti and the features it provides.

Below you will find FreeBSD specific instructions for installing and
configuring Cacti. So let's go ahead and install Cacti, shall we?

## Requirements

* root access or sudo rights
* Apache
* MySQL

## Installation

In order to install Cacti we are going to use the [FreeBSD Ports
Collection](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports.html)

First make sure that you have updated your Ports tree, and then let's
go ahead and install Cacti.

To install Cacti from the FreeBSD Ports Collection, execute the
following command:

```bash
$ sudo cd /usr/ports/net-mgmt/cacti && sudo make install clean
```

Once the installation is over you should see something similar:

```bash
===>   Returning to build of cacti-0.8.7g
===>   cacti-0.8.7g depends on file: /usr/local/lib/php/20090626/xml.so - found
===>   cacti-0.8.7g depends on shared library: mysqlclient.16 - found
Added group "cacti".
Added user "cacti".
===>   Generating temporary packing list
===>  Checking if net-mgmt/cacti already installed
=======================================================================
Cacti is now installed. If you intall it for the first time,
you may have to follow this steps to make it work correctly:

1. Create the MySQL database:
# mysqladmin --user=root create cacti
2. Create a mysql user/password for cacti:
   (change user and/or password if required)
   # echo "GRANT ALL ON cacti.* TO cactiuser@localhost IDENTIFIED BY 'cactiuser'; FLUSH PRIVILEGES;" | mysql
3. Import the default cacti database:
   # mysql cacti < /usr/local/share/cacti/cacti.sql
4. Edit /usr/local/share/cacti/include/config.php.
5. Add the line to cron jobs with the command:
   # crontab -u cacti -e
   */5 * * * * /usr/local/bin/php /usr/local/share/cacti/poller.php > /dev/null 2>&1
6. Add alias in apache config for the cacti dir:
   Alias /cacti "/usr/local/share/cacti/"
7. Be sure apache gives an access to the directory ('Allow from' keywords).
8. Open a cacti login page in your web browser and login with admin/admin.

If you update cacti, open a login page, an updating process
will start automatically.
=======================================================================
===>   Registering installation for cacti-0.8.7g
===>  Cleaning for cacti-0.8.7g
```

Now let's continue with the configuration.

## Configuring the SNMP daemon on the machines to be monitored

In this step we will configure the SNMP daemon on the FreeBSD machines
to be monitored.

You can skip this step, if your machines or routers already have
configured SNMP daemons.

Here we are going to use the SNMP daemon that comes with the base
FreeBSD system -
[bsnmpd(1)](http://www.freebsd.org/cgi/man.cgi?query=bsnmpd&apropos=0&sektion=0&manpath=FreeBSD+8.1-RELEASE&format=html)

You might also want to install a more extensible SNMP daemon like
[net-mgmt/net-snmp](http://www.freshports.org/net-mgmt/net-snmp/), but
this is not in the scope of the current handbook.

The configuration for *bsnmpd(1)* resides is in the
*/etc/snmpd.config* file.

Please refer to the manual page for bsnmpd(1) for more information
about the different options and explanation of them.

In the configuration we are only going to change a couple of things,
so again, please refer to the manual page of bsnmpd(1) for more
information regarding the rest of the configuration options.

```text
< SNIP > 

#
# Set some common variables
#
location := "example.org"
contact := "admin@example.org"

< SNIP > 

# Change this!
read := "bsnmpd"
```

In the above configuration we set the read community to **bsnmpd**

To enable bsnmpd(1) during boot-time, add the following lines to your
*/etc/rc.conf* file.

```bash
# Enable bsnmpd
bsnmpd_enable="YES"
```

Now let's start the SNMP daemon:

```bash
$ sudo /etc/rc.d/bsnmpd start
```

You will need to configure the SNMP daemon on each machine you want to
monitor. Once you are ready with that, please proceed with the next
step.

## Cacti Database Configuration

Cacti is using a database backend for storing all the information
about the devices it monitores.

In this step we will see how to create the database that will be used
by Cacti later for storing it's data.

Assuming you have read the handbook for [installing and configuring
MySQL server under FreeBSD](/node/2), we will now create the database
for Cacti.

So, first login to the MySQL server:

```bash
# mysql -u root -h mysql.example.org -p
Enter password:
Welcome to the MySQL monitor.  Commands end with ; or \g.
Your MySQL connection id is 55412
Server version: 5.0.91-log FreeBSD port: mysql-server-5.0.91

Type 'help;' or '\h' for help. Type '\c' to clear the current input
statement.

mysql>
```

Now let's create the database and user for Cacti:

```sql
mysql> CREATE DATABASE cacti DEFAULT CHARSET UTF8;
Query OK, 1 row affected (0.02 sec)

mysql> GRANT SELECT, INSERT, DELETE, UPDATE, INDEX, DROP, CREATE ON cacti.* to cacti_user@'monitor.example.org' IDENTIFIED BY 'some-password';
Query OK, 0 rows affected (0.00 sec)

mysql>SET PASSWORD FOR cacti_user@'monitor.example.org' = PASSWORD('some-password');
Query OK, 0 rows affected (0.00 sec)

mysql> FLUSH PRIVILEGES;
Query OK, 0 rows affected (0.00 sec)
```

Keep in mind that in the above SQL statements we define the following:

* **cacti** - The MySQL database Cacti will use
* **cacti_user** - The MySQL user that Cacti will use when connecting
* **to the database
* **some-password** Password for the MySQL account used by Cacti&nbsp;
* **monitor.example.org** - This is the server that Cacti will be
* **running on

Now let's create the Cacti database schema. This step needs to be
executed on the server Cacti will be running on,
e.g. *monitor.example.org*

```sql
mysql> USE cacti;
Database changed

mysql> SOURCE /usr/local/share/cacti/cacti.sql
```

The above commands will create the Cacti database schema, with all
it's tables and columns needed by Cacti.

Now we can configure Cacti and Apache in the next step of the
handbook.

## Configuring Apache and Cacti

Now let's configure Cacti, so that it uses the database we have just
created.

Cacti's default install location is */usr/local/share/cacti* - this is
where the configuration files also reside.

Open */usr/local/share/cacti/include/config.php* file for editing and
enter the database name, username, password and server that will be
used by Cacti:

```perl
/* make sure these values refect your actual database/host/user/password */
$database_type = "mysql";
$database_default = "cacti";
$database_hostname = "mysql.example.org";
$database_username = "cacti_user";
$database_password = "cactipassword";
$database_port = "3306";
```

Now let's configure our Apache vhost for Cacti.

Assuming you have read the handbook for [installing and configuring
Apache under FreeBSD](/node/30), we will now make some minor changes
to the Apache configuration, so that our default Apache vhost points
to Cacti.

```apacheconf
#
# Virtual Hosts
#
# If you want to maintain multiple domains/hostnames on your
# machine you can setup VirtualHost containers for them. Most configurations
# use only name-based virtual hosts so the server doesn't need to worry about
# IP addresses. This is indicated by the asterisks in the directives below.
#
# Please see the documentation at 
# <URL:http://httpd.apache.org/docs/2.2/vhosts/>
# for further details before you try to setup virtual hosts.
#
# You may use the command line option '-S' to verify your virtual host
# configuration.

#
# Use name-based virtual hosting.
#
NameVirtualHost *:80

#
# VirtualHost example:
# Almost any Apache directive may go into a VirtualHost container.
# The first VirtualHost section is used for all requests that do not
# match a ServerName or ServerAlias in any <VirtualHost> block.
#
#

<VirtualHost *:80>
    ServerAdmin admin@example.org
    ServerName monitor.example.org
    ServerAlias monitor.example.org

    DocumentRoot "/usr/local/share/cacti"

        <Directory "/usr/local/share/cacti">
                AllowOverride None
                Order Allow,Deny
                Allow from all
        </Directory>

    ErrorLog "/var/log/apache/httpd-monitor.example.org-error.log"
    CustomLog "/var/log/apache/httpd-monitor.example.org-access.log" common
<VirtualHost>
```

The next thing to do is to add a cron job that polls your machines and
routers each 5 minutes.

To do that add the following lines to your /etc/crontab file:

```text
# Call Cacti poller each 5 minutes
*/5 *   *   *   *       cacti   /usr/local/bin/php /usr/local/share/cacti/poller.php > /dev/null 2>&1
```

Now, let's start Apache and start using Cacti. First we test the
configuration for errors, and then we start Apache.

```bash
$ sudo apachectl configtest
Syntax OK
```

Configuration is OK, so now we can start Apache:

```bash
$ sudo apachectl start
```

Now, please continue reading through the next chapter in order to do
the initial setup of Cacti and start using it.

## Using Cacti

Before we can start using and monitoring devices via Cacti we first
need to do the initial setup for Cacti.

To do that, open a browser and open the Cacti web interface, which
should be on the *http://monitor.example.org/* Apache vhost we've
installed and configured.

Please follow the steps provided by the Cacti installer, so that you
can do the initial configuration of Cacti.

When you are invited to login, use *admin* for the username and
password - right after that you will be advised to change the default
password to one of your choice.

Now you can start adding the machines and routers you want to be
monitored by Cacti.

To do that just go to *Console -> Devices -> Add* and enter the needed
information of the machine to be monitored.

Again, I would recommend that you now go and read the great [Cacti
online documentation](http://www.cacti.net/documentation.php), for
more comprehensive explanation of how to use and configure the
different aspects of Cacti.

## Installing Cacti Spine

This step is optional. It just covers how to install and configure
[Cacti Spine](http://www.cacti.net/spine_info.php) - a fast poller for
Cacti.

Spine is another SNMP poller that can be used by Cacti. It's a good
replacement for the default *cmd.php* poller that comes with Cacti and
is quite fast.

If your environment constist of a lot of machines you might want to
prefer using Cacti Spine just for that reason - performance.

To install Cacti Spine, we are going to use again the FreeBSD Ports
Collection.

```bash
$ sudo cd /usr/ports/net-mgmt/cacti-spine && make install clean
```

Once the installation of net-mgmt/cacti-spine is over, you should see
something similar:

```bash
===>  Installing for spine-0.8.7.g_1
===>   Generating temporary packing list
===>  Checking if net-mgmt/cacti-spine already installed
===>   Registering installation for spine-0.8.7.g_1
===> SECURITY REPORT: 
      This port has installed the following files which may act as network
      servers and may therefore pose a remote security risk to the system.
/usr/local/bin/spine

      If there are vulnerabilities in these programs there may be a security
      risk to the system. FreeBSD makes no guarantee about the security of
      ports included in the Ports Collection. Please type 'make deinstall'
      to deinstall the port if this is a concern.

      For more information, and contact details about the security
      status of this software, see the following webpage: 
http://www.cacti.net/cactid_info.php
===>  Cleaning for spine-0.8.7.g_1
```

Let's configure Spine. Open */usr/local/etc/spine.conf* and enter the
needed information - database server, username, password, database
name and port.

```perl
DB_Host         mysql.example.org
DB_Database     cacti
DB_User         cacti_user
DB_Pass         cactipassword
DB_Port         3306
DB_PreG         0
```

Now we need to tell Cacti to use the Spine poller, instead of the
default cmd.php one.

To do that, login to your Cacti web interface on
*http://monitor.example.org/* and go to *Console -> Settings*.

Then click on the *Paths* tab and somewhere near the bottom you should
see a field to enter the *Cacti Spine location poller*. Enter the
following in the field - */usr/local/bin/spine* and then *Save*.

Then click on the *Poller* tab and choose Spine for the *Poller Type*
option and again *Save*.

Again on the *Poller* tab, you can further configure Spine - how many
threads to use, maximum SNMP OIDs per SNMP Get Request, etc.
