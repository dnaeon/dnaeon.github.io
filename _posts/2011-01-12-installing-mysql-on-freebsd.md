---
layout: post
title: Installing MySQL on FreeBSD
created: 1294852512
tags: freebsd mysql
---
MySQL is an RDBMS database that is freely distributed under the
terms of the GPL license.

More information about MySQL can be found on their
[home page](http://mysql.com) and it's
[Wiki page](http://en.wikipedia.org/wiki/MySQL)

## Requirements

* root access or sudo rights

## Installing MySQL

In this handbook we are going to install `mysql50-server` on a
FreeBSD 8.0 system using the
[FreeBSD Ports Collection](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports-using.html).

With that being said, let's go ahead and install MySQL.

```bash
$ cd /usr/ports/databases/mysql50-server && sudo make install clean
```

Once the installation is over you should see something similar:

```text
************************************************************************

Remember to run mysql_upgrade (with the optional --datadir=<dbdir> flag)
the first time you start the MySQL server after an upgrade from an
earlier version.

************************************************************************
install-info --quiet /usr/local/info/mysql.info /usr/local/info/dir
===> Installing rc.d startup script(s)
===>   Compressing manual pages for mysql-server-5.0.90
===>   Registering installation for mysql-server-5.0.90
===> SECURITY REPORT: 
      This port has installed the following files which may act as network
      servers and may therefore pose a remote security risk to the system.
/usr/local/libexec/mysqld

      This port has installed the following startup scripts which may cause
      these network services to be started at boot time.
/usr/local/etc/rc.d/mysql-server

      If there are vulnerabilities in these programs there may be a security
      risk to the system. FreeBSD makes no guarantee about the security of
      ports included in the Ports Collection. Please type &#39;make deinstall&#39;
      to deinstall the port if this is a concern.

      For more information, and contact details about the security
      status of this software, see the following webpage: 
http://www.mysql.com/
===>  Cleaning for mysql-client-5.0.90
===>  Cleaning for mysql-server-5.0.90
```

## Starting MySQL

Before starting the database daemon we need to initialize the
database directory, so execute the following commands to do so:

```bash
$ sudo mysql_install_db --user=mysql
```

Set proper permission of the MySQL directory:

```bash
$ sudo chown -R mysql:mysql /var/db/mysql
```

Copy the MySQL configuration file. According to the setup and
purpose you will be using the MySQL database copy
`my-{huge, innodb-heavy-4G, large, medium, small}.cnf` configuration
file from `/usr/local/share/mysql` directory.

So for example if your requirements are not that big and you plan to
use MySQL just to experiment with the database you may want to
copy the `mysql-medium.cnf` configuration file

```bash
$ sudo cp /usr/local/share/mysql/mysql-medium.cnf /etc/my.cnf
```

Now we can start the database:

```bash
$ sudo mysqld_safe --user=mysql &
```

## Securing MySQL

The very first thing that we need to do is to define a password for the
MySQL root account:

```bash
$ sudo mysqladmin -u root password <new-password>
```

The next thing we need to do is to secure the MySQL user accounts.

When MySQL was first installed it created some anonymous
password-less accounts.

In the next step we are going to remove those accounts due to
security reasons.

So, login to the database and provide the password you set for the
root account in the previous step:

```bash
$ mysql -u root -p
Enter password: 
Welcome to the MySQL monitor.  Commands end with ; or \g.
Your MySQL connection id is 5268
Server version: 5.0.91-log FreeBSD port: mysql-server-5.0.91

Type 'help' or '\h' for help. Type '\c' to clear the current input statement.

mysql>
```

Once logged in, you should see the MySQL prompt. Now let's secure our
MySQL database.

Setting password for the root user:

```sql
mysql> set password for root@localhost = password ('password');
Query OK, 0 rows affected (0.00 sec)

mysql> set password for root@example.com = password ('password');
Query OK, 0 rows affected (0.00 sec)
```

Remember to replace `example.com` with your actual hostname.

Now we are going to remove the anonymous accounts:

```sql
mysql> drop user ''@localhost;
Query OK, 0 rows affected (0.00 sec)

mysql> drop user ''@hostname;
Query OK, 0 rows affected (0.00 sec)

mysql> flush privileges;
Query OK, 0 rows affected (0.00 sec)
```

Drop the test database:

```sql
mysql> drop database test;
Query OK, 0 rows affected (0.00 sec)
```

## Starting MySQL during boot-time

In order to start the MySQL database during boot-time, add the
following lines to your `/etc/rc.conf` file:

```bash
# Enable MySQL
mysql_enable="YES"
mysql_args="--user=mysql"
```

## Backup and restore

Creating backups and restoring is easy. In order to create a
backup of a database, just execute the following command:

```bash
$ sudo mysqldump -u root -p --databases database --opt --default-character-set=utf8 > mysql-dump.sql
```

In order to restore from a backup you can do the following:

```bash
$ sudo mysql -u root -p pass < mysql-dump.sql
```

Another way to restore your backups is to use the `source` command
while logged in to the database:

```sql
mysql> source mysql-dump.sql
```
