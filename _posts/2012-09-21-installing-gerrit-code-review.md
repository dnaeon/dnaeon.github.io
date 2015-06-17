---
layout: post
title: Installing and configuring Gerrit Code Review System
tags: programming linux freebsd
---
In this handbook we we will see how to install and do the initial
configuration of Gerrit - a code review and project management system
for Git based project.

So, what is Gerrit?

Quoting from the [official web page of
Gerrit](http://code.google.com/p/gerrit/):

"*Gerrit is a web based code review system, facilitating online code
reviews for projects using the Git version control system.*

*Gerrit makes reviews easier by showing changes in a side-by-side
 display, and allowing inline comments to be added by any reviewer.*

*Gerrit simplifies Git based project maintainership by permitting any
 authorized user to submit changes to the master Git repository,
 rather than requiring all approved changes to be merged in by hand by
 the project maintainer. This functionality enables a more centralized
 usage of Git.*"

In this handbook we will see how to install and configure Gerrit under
[FreeBSD](http://www.freebsd.org) and also [Debian
GNU/Linux](http://www.debian.org)

Currently there are no official Gerrit packages for FreeBSD and Debian
GNU/Linux at the moment of writing this handbook, so I've ported
Gerrit to both systems and the port/package can be found in [my Github
projects](https://github.com/dnaeon).

* [FreeBSD Port for Gerrit](https://github.com/dnaeon/gerrit-freebsd)
* [Debian GNU/Linux package for
* Gerrit](https://github.com/dnaeon/gerrit-debian)

If you are willing to submit these packages as official FreeBSD/Debian
packages, please feel free to do so :)

## Requirements

* root access or sudo rights

## Tested And Verified

* FreeBSD 9.0 system
* Debian GNU/Linux 6.0 system
* Gerrit 2.4.2

## Installing Gerrit on FreeBSD

In order to install Gerrit under FreeBSD we are going to use the
[gerrit-freebsd port from
Github](https://github.com/dnaeon/gerrit-freebsd).

NOTE: There is already a PR for adding Gerrit to the FreeBSD Ports
collection, so until Gerrit goes into the Ports Tree, please use the
instructions provided below. Once Gerrit has been added to the Ports
Tree install it as you would with any other port. See
[ports/163655](http://www.freebsd.org/cgi/query-pr.cgi?pr=ports/163665)

So, first clone the Gerrit Git repository:

```bash
$ git clone https://github.com/dnaeon/gerrit-freebsd.git
```

Before we install Gerrit we also need a dedicated user for Gerrit. In
order to create the Gerrit user, we need to patch our *ports/UIDs* and
*ports/GIDs* first.

NOTE: Once
[ports/163666](http://www.freebsd.org/cgi/query-pr.cgi?pr=163666) gets
commited and Gerrit is added to the Ports Tree, you will not have to
patch the files manually, but this will be already set for you.

Download the patch from
[ports/163666](http://www.freebsd.org/cgi/query-pr.cgi?pr=163666) and
then apply it:

```bash
$ cd /usr/ports && sudo patch < /path/to/patch.txt
```

This should add entries for the Gerrit user and group, which in turn
will be used by the Gerrit Port during installation.

Now, lets build the Gerrit port:

```bash
$ cd gerrit-freebsd && sudo make install clean
```

Once Gerrit is installed lets enable it in */etc/rc.conf*, so that
Gerrit is also started during boot-time. To do that add this line to
your */etc/rc.conf* file:

```bash
gerrit_enable="YES"
```

## Installing Gerrit on Debian GNU/Linux

In order to install Gerrit under Debian GNU/Linux we are going to use
the [gerrit-debian package from
Github](https://github.com/dnaeon/gerrit-debian).

So, first clone the Gerrit repository from Github:

```bash
$ git clone https://github.com/dnaeon/gerrit-debian.git
```

Before building Gerrit make sure you have *build-essential* package
installed already. Now let's build the Gerrit package:

```bash
$ cd gerrit-debian && dpkg-buildpackage -us -uc
```

Once the package is built you will be able to install it just like any
other Debian GNU/Linux package using *dpkg(1)*:

```bash
$ sudo dpkg -i gerrit_2.4.2_all.deb
```

You can also find a pre-compiled package of Gerrit for Debian
GNU/Linux 6.0 (Squeeze) on the link below:

* [Gerrit 2.4.2 package for Debian GNU/Linux 6.0
  (Squeeze)](http://users.unix-heaven.org/~dnaeon/gerrit-debian/)

## Configuration of Gerrit

In this section of the handbook we will see how to start/stop and do
the initial configuration of our Gerrit instance.

In order to start/stop your Gerrit instance under FreeBSD, you should
use the *rc.d* script found in */usr/local/etc/rc.d/gerrit*.

For starting/stopping your Gerrit instance under Debian GNU/Linux, you
should use the *init.d* script found in */etc/init.d/gerrit*.

Now, lets start our Gerrit instance for the first time and perform the
initial configuration.

Starting Gerrit under FreeBSD:

```bash
$ sudo service gerrit start
```

Start Gerrit under Debian GNU/Linux:

```bash
$ sudo /etc/init.d/gerrit start
```

During the first-time run of Gerrit you will need to perform the
initial configuration of Gerrit, which includes:

* Location to the Git repositories
* The SQL database used by Gerrit
* Authentication
* Email server
* The Java runtime environment
* etc, etc.

Below you can see the configuration we've used for setting up Gerrit
during our first-time run under a FreeBSD system.

```bash
$ sudo service gerrit start
No Gerrit site found. Will initialize Gerrit first...

*** Gerrit Code Review 2.4.2
***

Create '/usr/local/gerrit/review_site' [Y/n]?

*** Git Repositories
***

Location of Git repositories   [git]:

*** SQL Database
***

Database server type           [H2/?]:

*** User Authentication
***

Authentication method          [OPENID/?]:

*** Email Delivery
***

SMTP server hostname           [localhost]:
SMTP server port               [(default)]:
SMTP encryption                [NONE/?]:
SMTP username                  :

*** Container Process
***

Run as                         [gerrit]:
Java runtime                   [/usr/local/openjdk6/jre]:
Copy gerrit.war to /usr/local/gerrit/review_site/bin/gerrit.war [Y/n]?
Copying gerrit.war to /usr/local/gerrit/review_site/bin/gerrit.war

*** SSH Daemon
***

Listen on address              [*]:
Listen on port                 [29418]:

Gerrit Code Review is not shipped with Bouncy Castle Crypto v144
If available, Gerrit can take advantage of features
in the library, but will also function without it.
Download and install it now [Y/n]? y
Downloading http://www.bouncycastle.org/download/bcprov-jdk16-144.jar
... OK
Checksum bcprov-jdk16-144.jar OK
Generating SSH host key ... rsa... dsa... done

*** HTTP Daemon
***

Behind reverse proxy           [y/N]?
Use SSL (https://)             [y/N]?
Listen on address              [*]:
Listen on port                 [8080]:
Canonical URL                  [http://localhost:8080/]:

Initialized /usr/local/gerrit/review_site
Executing /usr/local/gerrit/review_site/bin/gerrit.sh start
Starting Gerrit Code Review: OK
Waiting for server to start ... OK
Opening browser ...
Please open a browser and go to
http://localhost:8080/#/admin/projects/
```

The initial configuration of Gerrit under Debian GNU/Linux is
identical to the one in FreeBSD with minor changes in the above
example output like the location to the Java runtime environment and
the location of the review site.

Once you have successfully initialized and started your Gerrit
instance you can access it using your web browser at
http://example.org:8080/#/admin/projects

For further configuration of Gerrit, please refer to the [offcial
documentation of
Gerrit](http://gerrit-documentation.googlecode.com/svn/Documentation/2.4.2/dev-readme.html)
which provides extensive information on how to fully configure your
Gerrit instance.
