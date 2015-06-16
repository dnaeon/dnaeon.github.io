---
layout: post
title: CFEngine 3 and FreeBSD's pkgng
created: 1342099894
tags: freebsd cfengine
---
In a previous [handbook about CFEngine 3 on FreeBSD](/node/13) we
have seen how to install, configure and do configuration management
of our FreBSD systems using CFEngine 3.

In the above mentioned handbook there were also a few examples
included, and one in particular was about
[CFEngine package management](/node/19) using the legacy `pkg_*` tools
under FreeBSD for managing our packages.

Now, in this handbook we will see how we can use
[FreeBSD's pkgng](https://wiki.freebsd.org/pkgng)
together with [CFEngine 3](http://cfengine.com/) for package
management on our FreeBSD systems.

For more information on `pkgng` and how to set it up if you haven't
done so already, please refer to the below links:

* [Wiki page for pkgng](http://wiki.freebsd.org/pkgng/)
* [pkgng: First look at FreeBSD's new package manager](http://www.mebsd.com/make-build-your-freebsd-word/pkgng-first-look-at-freebsds-new-package-manager.html)
* [pkgng at Github](https://github.com/freebsd/pkg)

For setting up CFEngine 3, please refer to the
[CFEngine 3 handbook for FreeBSD users](/node/13).

## Requirements

* root access or sudo rights
* Have a working CFEngine 3 setup already

## Tested and Verified

The setup explained in this handbook has been tested and verified on:

* FreeBSD 9.0 system
* cfengine-3.3.3

## Getting the CFEngine 3 configurations

You can get all the configuration files used in this and the
[previous handbook](/node/13) from the
[Git repository](https://github.com/dnaeon/cfengine3-handbook).

To get all the configurations, just clone the Git repository:

```bash
$ git clone git://github.com/dnaeon/cfengine3-handbook.git
```

## CFEngine 3 package_method for pkgng

In order to get packages managed by `pkgng` first we need to have a
new `package_method` defined in CFEngine.

To do that we are going to update a bit our `library.cf` file and
add the following content to it:

```bash
body package_method freebsd_pkgng
{
package_changes => "bulk";

package_list_command => "/usr/local/sbin/pkg info";

package_list_name_regex    => "([^\s]+)-.*";
package_list_version_regex => "[^\s]+-([^\s]+).*";

package_name_regex    => "([^\s]+)-.*";
package_version_regex => "[^\s]+-([^\s]+).*";

package_installed_regex => ".*";

package_name_convention => "$(name)-$(version)";

package_update_command => "/usr/local/sbin/pkg update";
package_add_command => "/usr/local/sbin/pkg install -L -y";
package_delete_command => "/usr/local/sbin/pkg delete -y";
}
```

It is important to note here that we are now doing bulk package
changes, so for example if we had to install a few packages
they will all be installed using a single call to pkgng and not
multiple times like we did before with the old `pkg_*` tools.

Everything else in the above configuration should be self-explanatory.

## Updating the pkgng repositories using CFEngine 3

First we will start with a simple CFEngine 3 promise that will take
care of deploying `pkgng`'s `pkg.conf` file and updating
the remote repositories.

Below you will find a sample `pkg.conf` that I've used on my
FreeBSD machines with `pkgng`:

```bash
# System-wide configuration file for pkg(1)
# For more information on the file format and 
# options please refer to the pkg.conf(5) man page

# Configuration options
PACKAGESITE         : http://pkgbeta.freebsd.org/freebsd-9-amd64/latest
PKG_DBDIR           : /var/db/pkg
PKG_CACHEDIR        : /var/cache/pkg
PORTSDIR            : /usr/ports
PUBKEY              : /etc/ssl/pkg.conf
HANDLE_RC_SCRIPTS   : NO
PKG_MULTIREPOS      : NO
ASSUME_ALWAYS_YES   : NO
SYSLOG              : YES
SHLIBS              : NO
AUTODEPS            : NO
PORTAUDIT_SITE      : http://portaudit.FreeBSD.org/auditfile.tbz

# Repository definitions
#repos:
#  default : http://example.org/pkgng/
#  repo1 : http://somewhere.org/pkgng/repo1/
#  repo2 : http://somewhere.org/pkgng/repo2/
```

For now on we will use `packages.cf` file for creating the
CFEngine 3 pkgng promises.

And here's the how my first version of `packages.cf` file looks like.

```bash
#####################################################
#                                                   #
# packages.cf - Promises for package installations  #
#                                                   #
#####################################################

bundle agent packages { 

files:
	
	freebsd::
	             
        "/usr/local/etc/pkg.conf"

        comment         => "Copy pkgng configuration file for $(sys.fqhost)",
        perms           => mog("0644", "root", "wheel"),
        copy_from       => remote_copy("$(g.masterfiles)/pkgng/pkg.conf", "$(g.policyhost)"),
        classes         => if_repaired("update_pkgng_repo");
		
commands:

	freebsd.update_pkgng_repo::
        
        "/usr/local/sbin/pkg update -f";
}
```
	
What the above configuration simply does is this:

* Deploys `pkg.conf` file on all FreeBSD nodes connected to CFEngine
* Updates the `pkgng` remote repositories after deploying the new `pkg.conf` file

What we would want to have is updating our repositories each time
`pkg.conf` changes and that's what the above configuration does.

## Installing packages using CFEngine 3 and pkgng

Now, let's start installing packages using `pkgng` and CFEngine 3.

Below is an updated version of `packages.cf` file, which now is
capable of doing package installations using `pkgng`:

```bash
#####################################################
#                                                   #
# packages.cf - Promises for package installations  #
#                                                   #
#####################################################

bundle agent packages { 

vars:

    "freebsd_package_set" slist => { 
                         "git-1.7.10.3",
                         "emacs-23.4_1,2",
                         "tmux-1.6",
                         "sudo-1.8.5.p2",
                         "zsh-4.3.17_3",
                         "cfengine-3.3.3",
                         "apg-2.3.0b_2",
                         "ssmtp-2.64",
                         "rsync-3.0.9"
    };

files:
	
	freebsd::
	             
        "/usr/local/etc/pkg.conf"

        comment         => "Copy pkgng configuration file for $(sys.fqhost)",
        perms           => mog("0644", "root", "wheel"),
        copy_from       => remote_copy("$(g.masterfiles)/pkgng/pkg.conf", "$(g.policyhost)"),
        classes         => if_repaired("update_pkgng_repo");
		
commands:

	freebsd.update_pkgng_repo::
        
        "/usr/local/sbin/pkg update -f";

packages:

    freebsd::

       "$(freebsd_package_set)"

        package_policy  => "add",
        package_method  => freebsd_pkgng;
}
```

Now using the above configuration we are actually able to start
installing packages on our FreeBSD systems using `pkgng`.

As you can see, when we execute `cf-agent(8)` on a FreeBSD node we
will have the following packages installed:

* git-1.7.10.3
* emacs-23.4_1,2
* tmux-1.6
* sudo-1.8.5.p2
* zsh-4.3.17_3
* cfengine-3.3.3
* apg-2.3.0b_2
* ssmtp-2.64
* rsync-3.0.9

Below is just a sample snippet on one of my test FreeBSD systems
where you can see CFEngine 3 and `pkgng` working together:

```bash
cf3>>  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cf3>>    Offering these package-promise suggestions to the managers
cf3>>  >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
cf3>>  -> Deletion schedule...
cf3>>  -> Addition schedule...
cf3>> Execute scheduled package addition
cf3>> Command prefix: /usr/local/sbin/pkg install -L -y
cf3>> Executing /usr/local/sbin/pkg install -L -y rsync-3.0.9 apg-2.3.0b_2 e...
cf3>> Q:pkg install -L -y rs ...:The following packages will be installed:
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:       Installing rsync: 3.0.9
cf3>> Q:pkg install -L -y rs ...:       Installing apg: 2.3.0b_2
cf3>> Q:pkg install -L -y rs ...:       Installing emacs: 23.4_1,2
cf3>> Q:pkg install -L -y rs ...:       Installing git: 1.7.10.3
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:The installation will require 118 MB more space
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:0 B to be downloaded
cf3>> Q:pkg install -L -y rs ...:Checking integrity... done
cf3>> Q:pkg install -L -y rs ...:Installing rsync-3.0.9... done
cf3>> Q:pkg install -L -y rs ...:Installing apg-2.3.0b_2... done
cf3>> Q:pkg install -L -y rs ...:Installing emacs-23.4_1,2... done
cf3>> Q:pkg install -L -y rs ...:Installing git-1.7.10.3...Updating /etc/shells
cf3>> Q:pkg install -L -y rs ...: done
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:------------------------------------------------------------------------
cf3>> Q:pkg install -L -y rs ...:*************************** GITWEB *************************************
cf3>> Q:pkg install -L -y rs ...:If you installed the GITWEB option please follow these instructions:
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:In the directory /usr/local/share/examples/git/gitweb you can find all files to
cf3>> Q:pkg install -L -y rs ...:make gitweb work as a public repository on the web.
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:All you have to do to make gitweb work is:
cf3>> Q:pkg install -L -y rs ...:1) Copy the files /usr/local/share/examples/git/gitweb/* to a directory on
cf3>> Q:pkg install -L -y rs ...:   your web server (e.g. Apache2) in which you are able to execute
cf3>> Q:pkg install -L -y rs ...:   CGI-scripts.
cf3>> Q:pkg install -L -y rs ...:2) In gitweb.cgi, adjust the variable $projectroot to point to
cf3>> Q:pkg install -L -y rs ...:   your git repository (that is where you have your *.git project
cf3>> Q:pkg install -L -y rs ...:   directories).
cf3>> Q:pkg install -L -y rs ...:*************************** GITWEB *************************************
cf3>> Q:pkg install -L -y rs ...:
cf3>> Q:pkg install -L -y rs ...:*************************** CONTRIB ************************************
cf3>> Q:pkg install -L -y rs ...:If you installed the CONTRIB option please note that the scripts are
cf3>> Q:pkg install -L -y rs ...:installed in /usr/local/share/git-core/contrib. Some of them require
cf3>> Q:pkg install -L -y rs ...:other ports to be installed (perl, python, etc), which you may need to
cf3>> Q:pkg install -L -y rs ...:install manually.
cf3>> Q:pkg install -L -y rs ...:*************************** CONTRIB ************************************
cf3>> Q:pkg install -L -y rs ...:------------------------------------------------------------------------
cf3>> Q:pkg install -L -y rs ...:
cf3>>  -> Finished command related to promiser "rsync-3.0.9" -- succeeded
cf3>> Bulk package schedule execution ok for rsync-3.0.9 (outcome cannot be promised by cf-agent)
cf3>> Bulk package schedule execution ok for apg-2.3.0b_2 (outcome cannot be promised by cf-agent)
cf3>> Bulk package schedule execution ok for emacs-23.4_1,2 (outcome cannot be promised by cf-agent)
cf3>> Bulk package schedule execution ok for git-1.7.10.3 (outcome cannot be promised by cf-agent)
cf3>>  -> Update schedule...
cf3>>  -> Patch schedule...
cf3>>  -> Verify schedule...
cf3>> 
cf3>>    =========================================================
cf3>>    vars in bundle packages (2)
cf3>>    =========================================================
cf3>> 
cf3>> 
cf3>>      +  Private classes augmented:
cf3>> 
cf3>>      -  Private classes diminished:
cf3>> 
cf3>> 
cf3>> 
cf3>>    =========================================================
```
