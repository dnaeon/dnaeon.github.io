---
layout: post
title: Configuring Postfix on FreeBSD to relay mail through Gmail
created: 1341843164
tags: freebsd postfix relay mail
---
In this handbook we will see how to install and configure a
Postfix server which relays through Gmail.

Recently I had to decommission one of my local mail servers and
setting up Postfix to relay through Gmail seemed to be the
most viable solution.

A lot of sites out there also seem to insist on having a client
certificate when relaying to Gmail, and let me tell
you that this is *not* required as most of them say. 

I do not want to have any certificates created and installed, and
just want my local mail server to simply relay through Gmail.

So to summarize this handbook shows you how to configure Postfix to
relay through Gmail *without* having any certificates.

## Requirements

* root access or sudo rights

## Tested and verified

The setup explained in this handbook has been tested and verified on:

* FreeBSD 9.0 system
* postfix-2.9.3,1
* cyrus-sasl-2.1.25_2

## Installation of Postfix

First lets install Postfix from the
[FreeBSD Ports Tree](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/ports.html).

To install Postfix execute the command below:

```bash
$ sudo make -C /usr/ports/mail/postfix install clean
```

[![]({{ site.baseurl }}/images/postfix-config_0.jpg)]({{ site.baseurl }}/images/postfix-config_0.jpg){:.glightbox}

Make sure to select the `SASL2` and `TLS` options from the
configuration menu.

## Configuration of Postfix

Now we can configure Postfix to relay through Gmail.

Below you will find a working `main.cf` file for Postfix which
relays through Gmail.

Please refert to `smtpd(8)` man page for explanation of the
configuration options used in the example configuration file.

This is how my `main.cf` file looks like:

```text
# General options
queue_directory = /var/spool/postfix
command_directory = /usr/local/sbin
daemon_directory = /usr/local/libexec/postfix
data_directory = /var/db/postfix
mail_owner = postfix
unknown_local_recipient_reject_code = 550
debug_peer_level = 2

# Alias maps
alias_maps = hash:/etc/aliases

# My hostname, domain, origin, networks
myhostname = mail.example.org
mydomain = example.org
myorigin = example.org
inet_interfaces = <mail-server-address>
inet_protocols = ipv4
mynetworks = 127.0.0.0/8, <network>

# SASL options
smtp_sasl_auth_enable = yes
smtp_sasl_password_maps = hash:/usr/local/etc/postfix/sasl_passwd
smtp_sasl_security_options = noanonymous

# TLS options
smtp_use_tls = yes
smtp_tls_security_level = encrypt
tls_random_source = dev:/dev/urandom

# Relay host
relayhost = [smtp.gmail.com]:587
```

The only things you need to edit in the above configuration
file order to match your setup is:

* myhostname
* mydomain
* myorigin
* inet_interfaces
* my_networks

Please edit them and save the configuration file.

## Setting up Postfix permissions

Now we need to setup proper permissions for Postfix.

To do that just execute the command below:

```bash
$ sudo postfix -c /usr/local/etc/postfix set-permissions
```

## Creating the aliases maps 

In order to create the aliases maps, execute the commands below:

```bash
$ sudo newaliases
$ sudo postalias /etc/aliases
```

## Authenticating to Gmail

The next thing we need to do is to be able to authenticate to
Gmail using our username and password.

To do that we need to create the `/usr/local/etc/postfix/sasl_passwd`
file with the below content:

```text
smtp.gmail.com    <username>:<password>
```

Now hash the file, so that Postfix can use it:

```bash
$ sudo postmap /usr/local/etc/postfix/sasl_passwd
```

Secure the password files:

```bash
$ sudo chmod 0600 /usr/local/etc/postfix/sasl_passwd*
$ sudo chown root:wheel /usr/local/etc/postfix/sasl_passwd*
```

## Starting Postfix during boot-time

Now that we are done with the installation and configuration of
Postfix, we would want that it starts during boot-time.

To do that add the following line to your `/etc/rc.conf` file:

```bash
postfix_enable="YES"
```

And now start Postfix:

```bash
$ sudo service postfix start
```

## Testing it out

Now that all is done we can finally test if our setup is working well.

To do that just send a test email and check `/var/log/maillog` that the
mail is being processed correctly.

```bash
$ echo "test mail" | mail -s "test subject" user@gmail.com
```

Check in `/var/log/maillog` that everything is OK.

## Further reading

If you want to have your local relay server be used also from all
machines in your network, you might want to 
take a look at [mail/ssmtp](http://www.freshports.org/mail/ssmtp/).

The FreeBSD Handbook provides a nice article on
[how to setup mail/ssmtp on your FreeBSD system](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/outgoing-only.html),
so you might want to check that as well.
