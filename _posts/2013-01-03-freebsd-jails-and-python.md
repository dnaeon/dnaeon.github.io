---
layout: post
title: FreeBSD Jails & Python
created: 1357222071
tags: freebsd python programming
---
This post is about Python and
[FreeBSD
Jails](http://www.freebsd.org/doc/en_US.ISO8859-1/books/handbook/jails.html).

It shows you how you can use Python for managing your FreeBSD jails.

The Python interface for [jail_get() system
call](http://www.freebsd.org/cgi/man.cgi?query=jail_get&apropos=0&sektion=0&manpath=FreeBSD+9.0-RELEASE&arch=default&format=html)
has been added to the existing
[devel/py-freebsd](http://www.freshports.org/devel/py-freebsd/) port
and the purpose of this Python interface is to allow you to get jail's
parameters like the JID, hostname, IP address, etc.

For now the *jail_get()* Python interface is only available from the
Github repository listed below, but hopefully soon enough it will be
added to the FreeBSD Ports Collection as well.

* [Github repository of py-freebsd](https://github.com/dnaeon/py-freebsd)

Okay, lets see what we can do with the new [jail_get() system
call](http://www.freebsd.org/cgi/man.cgi?query=jail_get&apropos=0&sektion=0&manpath=FreeBSD+9.0-RELEASE&arch=default&format=html)
interface for Python, time to fire up your Python interpreter!

```python
dnaeon:(~) % python
Python 2.7.3 (default, Aug  4 2012, 12:47:37) 
[GCC 4.2.1 20070831 patched [FreeBSD]] on freebsd9
Type "help", "copyright", "credits" or "license" for more information.
>>> 
```

First lets load the needed Python modules:

```python
>>> from freebsd import *
>>> from pprint import *
```

The Python interface for *jail_get()* takes one integer argument,
which is the jail's JID and returns a dictionary with the parameters
of the jail.

If you want to get the parameters of all your jails then pass 0 (zero)
as the argument to *jail_get()*. Check the example below:

```python
>>> pprint(jail_get(0))
	
{'3': {'hostname': 'relay.example.org',
	'ip4': '10.11.117.4',
	'jid': 3,
	'path': '/jails/relay'},
'4': {'hostname': 'mysql.example.org',
	'ip4': '10.11.117.5',
	'jid': 4,
	'path': '/jails/mysql'},
'495': {'hostname': 'www.example.org',
	'ip4': '10.11.117.7',
	'jid': 495,
	'path': '/jails/www'},
'496': {'hostname': 'www-proxy.example.org',
	'ip4': '10.11.117.6',
	'jid': 496,
	'path': '/jails/www-proxy'}}
```

You can see from the above output that we've got a nice dictionary
with the parameters of all our jails. Getting the parameters for a
single jail works too, check this example:

```python
>>> jail_get(3)
{'path': '/jails/relay', 'jid': 3, 'hostname': 'relay.example.org', 'name': '3', 'ip4': '10.11.117.4'}
```

Okay, that's all good, but can we do more with it? Sure, we can now
write a simple *jls(8)*-like Python function that displays information
about our FreeBSD jails. The script below works in a similar way to
*jls(8)* and displays information about our jails:

```python
#!/usr/bin/env python

import freebsd

def py_jls():
    jails = freebsd.jail_get(0)

    print '%5s  %-15s  %-35s  %-20s' % ('JID', 'IP Address', 'Hostname', 'Path')
		
    for j in jails:
        print '%5s  %-15s  %-35s  %-20s' % \
            (jails[j]['jid'], jails[j]['ip4'], jails[j]['hostname'], jails[j]['path'])

if __name__ == '__main__':
    py_jls()
```

And here's the output after running the script, which is pretty
much the same as `jls(8)`.

```bash
dnaeon:(~) % python py-jls.py
JID  IP Address         Hostname                   Path                
495  10.11.117.7        www.example.org            /jails/www          
3    10.11.117.4        relay.example.org          /jails/relay        
4    10.11.117.5        mysql.example.org          /jails/mysql        
496  10.11.117.6        www-proxy.example.org      /jails/www-proxy
```
