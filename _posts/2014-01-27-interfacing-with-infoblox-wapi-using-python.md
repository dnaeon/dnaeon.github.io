---
layout: post
title: Interfacing with Infoblox WAPI using Python
created: 1390835262
tags: python programming
---
Infoblox develops some really nice products for automating and
managing your network infrastructure services such as DNS, DHCP,
IP Address Management (IPAM) and others.

In this post we will see how we can interface with the Infoblox WAPI
using Python in order to get, create, update and delete objects from
an Infoblox instance.

Most of the examples on the [Infoblox WAPI documentation
page](https://community.infoblox.com/resource/getting-started-infoblox-web-api-wapi)
are Perl related, so in this post we will see how to use Python in
order to interface with the [Infoblox
WAPI](https://community.infoblox.com/resource/getting-started-infoblox-web-api-wapi).

Fore more information about Infoblox and their products, please refer
to the [official Infoblox web site](http://www.infoblox.com/).

## Requirements

* Python 2.7.x or 3.x
* [requests](http://docs.python-requests.org/en/latest/) library

## Getting started

Most of the examples you will see in this post were executed in Python
interactive mode. At the end of this post you will also find a CLI
application which is able to get, create, delete and update Infoblox
objects. For now let's see how this is done step-by-step.

Now, let's get started, shall we?

```python
$ python
Python 2.7.3 (default, Jan  2 2013, 13:56:14) 
[GCC 4.7.2] on linux2
Type "help", "copyright", "credits" or "license" for more information.
>>>
```

Import the `requests` module:

```python
>>> import requests
```

Now, let's create a `requests.Session` object which we will use for
all our requests to the [Infoblox
WAPI](https://community.infoblox.com/resource/getting-started-infoblox-web-api-wapi).

```python
>>> session = requests.Session()
>>> session.auth = ('admin', 'password)
>>> session.verify = False
```

If your Infoblox instance is using a self-signed SSL certificate you
would want to set `session.verify` to `False`, otherwise your requests
will fail. If your Infoblox instance is using a signed certificate
from an official CA you can skip setting the `session.verify`
attribute.

Let's also set the URL to the Infoblox WAPI, which is where our
requests will be send to:

```python
>>> url = 'https://infoblox.example.org/wapi/v1.1/'
```

Okay, now we are ready to start sending our WAPI requests to the
Infoblox instance. In the next sections of this post we will see how
to get, create, update and delete objects from Infoblox.

## Getting objects from Infoblox

In order to get Infoblox objects we need to use the HTTP `GET`
method. This is how we could get all Infoblox `network` objects.

```python
>>> r = session.get(url + 'network')
```

Let's check if our request was successful or not:

```python
>>> r.status_code
200
```

The `HTTP code 200` means that our request was successful, so we could
now see what the result was as well:

```python
>>> print r.content
[
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTAuMjMuOTYuMC8yNC8w:192.168.0.0/24/default", 
        "network": "192.168.0.0/24", 
        "network_view": "default"
    }, 
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTAuMjMuMTA4LjAvMjQvMA:192.168.1.0/24/default", 
        "network": "192.168.1.0/24", 
        "network_view": "default"
    }
]
```python

From the output above you can see the networks we have in Infoblox,
the network view and also the object reference for each `network`
object. The `object reference` in the output above is what we need to
pass when we need to update or delete an Infoblox object. Later in
this post we will see how to do that as well.

## Creating objects in Infoblox

In order to create a new Infoblox object you need to use the HTTP
`POST` method. This is how we could create a new Infoblox `network`:

```python
>>> data = { 'network': '192.168.2.0/24', 'comment': 'A new Infoblox network' }
>>> r = session.post(url + 'network', data=data)
```

Let's check if the request was successful.

```python
>>> r.status_code
201
```

The `HTTP code 201` means that we have successfully created a new
Infoblox object.

Let's check what was the result from our call as well:

```python
>>> print r.content
"network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default"
```

Upon a successful object creation in Infoblox the result we will
receive is the new object's reference. In the above output you can see
the object reference of our new network `192.168.2.0/24`. Let's verify
that it actually exists in Infoblox.

```python
>>> r = session.get(url + 'network')
>>> print r.content
[
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTAuMjMuOTYuMC8yNC8w:192.168.0.0/24/default", 
        "network": "192.168.0.0/24", 
        "network_view": "default"
    }, 
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTAuMjMuMTA4LjAvMjQvMA:192.168.1.0/24/default", 
        "network": "192.168.1.0/24", 
        "network_view": "default"
    }, 
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default", 
        "comment": "A new Infoblox network", 
        "network": "192.168.2.0/24", 
        "network_view": "default"
    }
]
```

As we can see from the output above we can now also see our newly
created Infoblox network.

## Updating objects in Infoblox

In order to update an object in Infoblox we need to use the HTTP `PUT`
method. When updating an object in Infoblox we need to pass the object
reference as well.

This is how we could update the example `network` object we recently
created in the previous section:

```python
>>> ref = "network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default"
>>> data = { 'comment': 'This is a new comment' }
>>> r = session.put(url + ref, data=data)
```

Let's check if the request was successful:

```python
>>> r.status_code
200
```

The `HTTP code 200` means that we have successfully updated the object
in Infoblox.

If we check the result message from our request we would see that the
Infoblox WAPI returns to us the object reference we have just updated:

```python
>>> r.content
'"network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default"'
```

Let's also check if our network has been successfully updated:

```python
>>> print session.get(url + 'network').content
[
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTAuMjMuOTYuMC8yNC8w:192.168.0.0/24/default", 
        "network": "192.168.0.0/24", 
        "network_view": "default"
    }, 
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTAuMjMuMTA4LjAvMjQvMA:192.168.1.0/24/default", 
        "network": "192.168.1.0/24", 
        "network_view": "default"
    }, 
    {
        "_ref": "network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default", 
        "comment": "This is a new comment", 
        "network": "192.168.2.0/24", 
        "network_view": "default"
    }
]
```

As you can see from the output above we have successfully updated our
`network` object.

## Removing objects from Infoblox

In order to remove an object from Infoblox we need to use the HTTP
`DELETE` method. When removing an object from Infoblox we need to pass
the object reference as well.

This is how we could remove the example `network` object we recently
created in the previous section:

```python
>>> ref = "network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default"
>>> r = session.delete(url + ref)
```

Let's check if the request was successful:

```python
>>> r.status_code
200
```

The `HTTP code 200` means that we have successfully removed the object
from Infoblox.

If we check the result message from our request we would see that the
Infoblox WAPI returns to us the object reference we have just removed:

```python
>>> r.content
'"network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default"'
```

## Wrapping it up

By now you should know how to get, create, update and delete objects
from an Infoblox instance. Running commands in the Python interactive
mode is useful when testing, learning something new or just debugging
an issue, but generally you would want all of the above to be handled
by some tool.

For that purpose I've made a Python script which is able to get,
create, update and remove objects from Infoblox server, which you can
find at the Github repository listed below.

* [Github repository of py-infoblox](https://github.com/dnaeon/py-infoblox)

Grab the repository from Github by executing the command below:

```bash
$ git clone https://github.com/dnaeon/py-infoblox.git
```

Install the module and the CLI application:

```bash
$ python setup.py install
```

Create a config file describing the various Infoblox config
entries. Below is an example configuration file:

```ini
[Default]
wapi      = https://infoblox.example.org/wapi/v1.1/
username  = admin
password  = password
sslverify = False
```

Now you can use the `infoblox-cli` application in order to get,
create, update and remove objects from an Infoblox instance.

Example command to get the Infoblox `network` objects:

```bash
$ infoblox-cli -f infoblox.conf -t network get
```

Example command to create a new Infoblox object:

```bash
$ infoblox-cli -f infoblox.conf -t network -d '{ "network": "192.168.2.0/24", "comment": "Test network" }' create
```

Example command to update an Infoblox object:

```bash
$ infoblox-cli -f infoblox.conf -r network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default -d '{ "comment": "The new test network" }' update
```

Example command to remove an Infoblox object:

```bash
$ infoblox-cli -f infoblox.conf -r network/ZG5zLm5ldHdvcmskMTkyLjE2OC4yLjAvMjQvMA:192.168.2.0/24/default remove
```
