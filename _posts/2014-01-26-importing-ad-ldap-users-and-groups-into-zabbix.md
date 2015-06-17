---
layout: post
title: Importing AD/LDAP users and groups into Zabbix
created: 1390750507
tags: programming python monitoring zabbix
---
I've been a Zabbix user for quite some time already, but one thing
that I always wanted and which Zabbix lacks as a feature so far is the
ability to import AD/LDAP users and groups.

Zabbix already supports authentication against an AD/LDAP directory
server, but in order for you to authenticate against the AD/LDAP
directory server you need to have the users imported to the Zabbix
database first.

Now, that could easily become annoying especially when your AD/LDAP
directory server has lots of users and groups and you want to allow
them access to the Zabbix Frontend.

Fortunately for us we can we use the [Zabbix
API](https://www.zabbix.com/documentation/2.2/manual/api) in order to
manage the Zabbix users and groups.

In this post we will see how we can import and update AD/LDAP users
and groups into the Zabbix database.

## Requirements

* Python 2.7.x
* [python-ldap](https://pypi.python.org/pypi/python-ldap/)
* [zabbix_api](https://github.com/gnetsman/zabbix_api)
* [docopt](https://github.com/docopt/docopt)

You also need to have your Zabbix Frontend configured to authenticate
against an AD/LDAP directory server.

Check the official documentation of Zabbix on how to [configure Zabbix
to authenticate against an AD/LDAP directory
server](https://www.zabbix.com/documentation/2.2/manual/web_interface/frontend_sections/administration/authentication).

You will also need the
[zabbix-ldap-sync](https://github.com/dnaeon/zabbix-ldap-sync) script
which takes care of properly importing our AD/LDAP users and groups to
Zabbix.

In order to grab the latest version of
[zabbix-ldap-sync](https://github.com/dnaeon/zabbix-ldap-sync) clone
the Git repository by executing the command below:

```bash
$ git clone https://github.com/dnaeon/zabbix-ldap-sync.git
```

## Configuration

The first thing we need to do is to prepare a configuration file,
which describes the various AD/LDAP and Zabbix related configs
entries.

Here is an example configuration file:

```ini
[ldap]
uri = ldaps://ldap.example.org:636/
base = dc=example,dc=org
groups = sysadmins

[zabbix]
server = http://zabbix.example.org/zabbix/
username = admin
password = adminp4ssw0rd
```

As you can see from the above configuration file we are defining the
various LDAP settings such as the LDAP URI, the LDAP search base and
the LDAP groups we want to import into our Zabbix database.

The configuration file also defines the URL to our Zabbix Frontend and
the username/password credentials with admin access.

## Importing the AD/LDAP users and groups into Zabbix

Once you have the
[zabbix-ldap-sync](https://github.com/dnaeon/zabbix-ldap-sync) script
and the configuration file ready we can import our AD/LDAP users and
groups into the Zabbix database.

Here is an example command which you can use in order to import your
AD/LDAP users and groups to Zabbix:

```bash
$ zabbix-ldap-sync -f /path/to/zabbix-ldap.conf
```

The script reports each user and group that will be imported into
Zabbix. Once the script completes you should check your Zabbix
Frontend to verify that users are successfully imported.

You would generally be running the above script on regular basis, say
each day from `cron(8)` in order to make sure your Zabbix system is in
sync with LDAP.
