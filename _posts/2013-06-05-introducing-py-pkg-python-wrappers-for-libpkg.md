---
layout: post
title: ! 'Introducing py-pkg: Python wrappers for libpkg'
created: 1370448847
tags: freebsd python programming
---
For the past few weeks I've been working on a Python project and since
then I really enjoy using Python everyday.

Now is the time to actually present the project, so let me introduce
to you **py-pkg** -- *Python wrappers for FreeBSD's libpkg library*.

Here I'll just try to add a few lines about **py-pkg** and what is
interesting about it. Then I'll show you a few examples on how to use
**py-pkg** in order to manage your FreeBSD package database!

## General Information

Q: So, what is **py-pkg**?

The goal of *py-pkg* is to provide Python wrappers for FreeBSD's
libpkg library. The wrappers themself are written in
[Cython](http://cython.org), which gives you the flexibility and power
to use Python with C data structures, very neat! :)

The Python wrappers themself are being compiled as a shared library
and can be imported into Python as any other Python module.

Q: So, what can I use **py-pkg** for?

You can do a lot with it, here are just a few examples:

* Querying information about your packages
* Searching remote repositories for packages
* Install packages
* Delete packages
* Upgrade packages
* etc, etc, etc.

Q: Yeah, I can do that with *pkg(8)* already, what's the point of
*py-pkg*?

The idea of *py-pkg* is to expose *libpkg* functionallity to Python in
a way that *pkg(8)* already uses it.

Think about this for a second: what does it take to write a Gtk/Qt
interface for *libpkg* in C and what does it take to do the same thing
in Python? I'm sure you know the answer already.

So, by providing Python wrappers for *libpkg*, new doors open for
you. Just think about it -- new frontends, new plugins, etc. and all
that you can do in Python! :)

Enough words already, time to go into the real deal.

## Repository and documentation

You can find the source code repository for *py-pkg* on the link
below:

* [py-pkg repository](https://github.com/dnaeon/py-pkg)

And here you can find the *py-pkg* Sphinx documentation:

* [py-pkg Sphinx documentation](http://jenkins.unix-heaven.org/jenkins/job/py-pkg-docs/py-pkg_Sphinx_Documentation)

## Requirements and installation of py-pkg

In order to install *py-pkg* you need to be running the development
version of libpkg (e.g. the one from the *master* branch of *pkgng*).

Then in order to build the wrappers you would need to have
*lang/cython*. Afterwards building and installing is pretty
straight-forward:

```bash
$ python setup.py build
$ sudo python setup.py install
```

Okay, lets go and check some examples now! :)

## Examples

Here, I'll add a few examples of using *py-pkg* on a FreeBSD system in
order to manage the package database.

You can also find these and other examples at the [example's page at
the Github repository](https://github.com/dnaeon/py-pkg).

So, read further, hope you enjoy it.

## Querying the package database

Here is how you could list all installed packages on your FreeBSD
system from Python and *py-pkg*:

```python
#!/usr/bin/env python

import pkg

db = pkg.PkgDb()
pkgs = db.query()

for p in pkgs:
    print p

db.close()
```

Pretty easy, isn't it? :)

## Searching for packages on a remote repository

This is how you would search a remote repository for a package.

```python
#!/usr/bin/env python
	
db = pkg.PkgDb(remotedb=True)

pkgs = db.rquery(pattern='zsh')

for p in pkgs:
    print p

db.close()
```

## Installing packages

This is how you would install a package using Python and *py-pkg*:

First, let's see how this can be done from an interactive Python
session, and later you can find a simple script that you can use for
the same purpose.

Let's first start up our Python interpreter now:

```python
% python
Python 2.7.5 (default, May 30 2013, 14:08:13) 
[GCC 4.2.1 20070831 patched [FreeBSD]] on freebsd9
Type "help", "copyright", "credits" or "license" for more information.
>>> 
```

First we are going to check whether the package we want is not
installed already, and if not -- we are going to install it.

```python
>>> import pkg
>>> db = pkg.PkgDb()
>>> 'security/apg' in db.query()
False
```
	
Okay, that means that *security/apg* is not installed on our system,
lets see if we have this package on our repository.

```python
>>> 'security/apg' in db.rquery()
True
```

So, we've got *security/apg* in our remote repository, lets go ahead
and install it:

```python
>>> jobs = db.install(['apg'])
>>> jobs.apply()
```

Lets see if we have the package installed now:
	
```python
>>> 'security/apg' in db.query()                                                                           
True
```

And we've got our package installed. Below is the short version for
installing a package:

```python
#!/usr/bin/env python

import pkg

db = pkg.PkgDb()
	
jobs = db.install(['apg'])
jobs.apply()
	
db.close()
```

## What is your BSD license coverage?

Okay, here's an interesting example, that will show you all the
packages on your system which are BSD and MIT licensed.

```python
#!/usr/bin/env python
	
import pkg
		
db = pkg.PkgDb()
	
pkgs = db.query()
pkgs.load_licenses()
	
for p in pkgs:
    for license in ['BSD', 'MIT']:
        if license in p.licenses():
            print '%s is %s licensed' % (p.origin(), license)
				
db.close()
```

## What repositories do I have?

Here's how you could list your package repositories.

```python
#!/usr/bin/env python

import pkg

db = pkg.PkgDb()

for repo in db.repositories():
    print '%s: %s [enabled: %s]' % (repo.name(), repo.url(), repo.enabled())

db.close()
```

Here's an example output of the above script:

```text
repo-packagesite: file:///home/dnaeon/PROJECTS/pkg-repo [enabled: True]
```

## Checking for missing package dependencies

Here's a simply Python script that can detect missing dependencies in
your package database.

```python
#!/usr/bin/env python

from pkg import *

def check_deps(pkg, db):
    for dep in pkg.deps():
        if not db.pkg_is_installed(dep.origin()):
            print '%s has a missing dependency: %s' % (pkg.origin(), dep.origin())

def main():
    db = PkgDb()

    pkgs = db.query()
    pkgs.load_deps()
		
    for pkg in pkgs:
        check_deps(pkg, db)

    db.close()

if __name__ == '__main__':
    main()
```
