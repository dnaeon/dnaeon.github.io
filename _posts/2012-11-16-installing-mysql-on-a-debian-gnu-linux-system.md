---
layout: post
title: Installing MySQL on a Debian GNU/Linux system
created: 1353085027
tags: linux mysql
---
Login to the system you want to install MySQL and install the *mysql-server* package.

```bash
$ sudo apt-get install mysql-server
```

## Stopping the database

Once the package is installed the database will be started up, so
first stop it before continiuing with it's configuration.

```bash
$ sudo invoke-rc.d mysql stop
```

## Configuring the database

If remote machines will be connecting to the MySQL database, we need
to make sure that we listen not just on localhost.

To allow remote machines connecting to the database in
`/etc/mysql/my.cnf` comment this line out:

```text
bind-address           = 127.0.0.1
```

Any other changes you need to make you should do them in
`/etc/mysql/my.cnf` file.

## Starting up the database:

In order to start the database, execute the command below:

```bash
$ sudo invoke-rc.d mysql start
```

## Securing the database

Set password for the database *root* user:

```bash
/usr/bin/mysqladmin -u root password 'some-good-password-here'
/usr/bin/mysqladmin -u root -h 'mysql.example.org' password 'some-good-password-here'
```

Drop any anonymous users from the database if any. Login to the MySQL
instance using `mysql -u root -p` and then drop the anonymous users:

```sql
mysql> drop user ''@localhost;
mysql> drop user ''@hostname;
mysql> flush privileges;
```

And that should be all!
