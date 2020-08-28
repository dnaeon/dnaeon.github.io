---
layout: post
title: ECDSA keys support in cl-ssh-keys for Common Lisp
tags: encoding decoding ssh programming rfc4251 rfc4253 rfc5656 common-lisp lisp
---
Support for [ECDSA private and public keys in
ironclad](https://github.com/sharplispers/ironclad/issues/33) has been
implemented.

Soon after that I've added support for ECDSA keys in
[cl-ssh-keys](https://github.com/dnaeon/cl-ssh-keys), which now allows
you to encode, decode and generate new ECDSA keys in OpenSSH binary
format.

Make sure to check this [post about
cl-ssh-keys](http://dnaeon.github.io/parsing-and-generating-openssh-keys-with-cl/),
if you haven't seen it before, which talks about the `cl-ssh-keys`
system in more details.

Generating new OpenSSH ECDSA keys with `cl-ssh-keys` is easy as this.

``` common-lisp
CL-USER> (ssh-keys:generate-key-pair :ecdsa-nistp256)
CL-USER> (ssh-keys:generate-key-pair :ecdsa-nistp384)
CL-USER> (ssh-keys:generate-key-pair :ecdsa-nistp521)
```

You can use the generated ECDSA keys for public-key authentication on
your remote servers.

For more examples and additional documentation, please refer to the
[cl-ssh-keys](https://github.com/dnaeon/cl-ssh-keys) repo.
