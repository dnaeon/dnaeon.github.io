---
layout: post
title: py-mod_zipper - Python module for Apache for browsing of Zip archives
created: 1374146212
tags: python programming
---
Recently I had the need of an Apache module, which would allow me to
browse Zip archives on a remote server, without having the need to
download first the whole Zip archive.

So after looking a bit on the web I came across two Apache modules
that initially looked like would do the job. These Apache modules are
*mod_ziplook* and [mod_zipread](http://modzipread.sourceforge.net/).

Unfortunately the problem was that *mod_ziplook* was nowhere to be
found. I had to search through a lot of repositories out there and
even when I thought I've finally found it I was always hitting a no
longer existing links. On the other hand
[mod_zipread](http://modzipread.sourceforge.net/) seems to be
available for download, but there's one big warning on the official
page stating that *mod_zipread* is in very early alpha phase and could
be dangerous to use, so I've decided not even to try it out.

As there were no other alternatives to the problem I was having, I've
decided to write my own solution. Let me introduce you *py-mod_zipper*
- an Apache module written in Python, which allows you to browse Zip
archives through your web browser!

You can grab the *py-mod_zipper* code from the public Github
repository listed below:

* [https://github.com/dnaeon/py-mod_zipper](https://github.com/dnaeon/py-mod_zipper)

You can clone the above repository by executing the command below:

```bash
$ git clone https://github.com/dnaeon/py-mod_zipper.git
```

## Requirements

The *py-mod_zipper* Apache module requires that you have
[mod_python](http://www.modpython.org/) module loaded by your Apache
server.

You will also need to be running Apache version 2.x or above.

## Tested and verified

The *py-mod_zipper* module has been tested and verified to work on the
following system:

* Debian GNU/Linux 7.1, aka Debian Wheezy
* Apache version 2.2.22-13
* mod_python version 3.3.1-9+b3

## Configuration of Apache

Before we can use *py-mod_zipper* we need to install *mod_python* and
enable it:

```bash
$ sudo apt-get install libapache2-mod-python
```

Now, let's create an Apache vhost which will be using *py-mod_zipper*
as the handler for Zip files.

Create a file named *zipper.conf* in */etc/apache2/sites-enabled* with
the contents shown below:

```apacheconf
<Directory /home/username/public_html> 
	Options +Indexes
	AddHandler mod_python .zip
	PythonHandler /path/to/mod_zipper.py
	PythonDebug Off 
</Directory>
```

Make sure to set *PythonHandler* to the correct location of
*mod_zipper.py* on your system.

Restart Apache in order to apply the new configuration changes:

```bash
$ sudo service apache2 restart
```

## Testing the module

Now that we've got Apache configured for use with *py-mod_zipper* we
can test it. Considering that you have configured the Apache directory
to be served in */home/username/public_html* the next thing to do is
to simply put some Zip files in that directory.

Then simply open a browser to your Apache server and you should see
the Zip files there. Clicking on any of the Zip files should result in
calling the *py-mod_zipper* handler which will allow you to browse the
contents of the Zip archive.

## Example screenshots

Below you can see an example screenshot of *py-mod_zipper* in
action. From within the web browser we have clicked on a Zip archive
and as you can see we are able to browse the Zip archive using
*py-mod_zipper*!

![_config.yml]({{ site.baseurl }}/images/py-mod_zipper.png)
