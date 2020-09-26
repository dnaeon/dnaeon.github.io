---
layout: post
title: Support for encrypted private keys with cl-ssh-keys in Common Lisp
tags: lisp common-lisp ssh ssh-keys private-key encrypted-private-key
---
This is a follow up of my previous post about [parsing and generating
OpenSSH keys in Common
Lisp](http://dnaeon.github.io/parsing-and-generating-openssh-keys-with-cl/).

One of the features that were missing until today in the
[cl-ssh-keys][cl-ssh-keys] system is support for encrypted private
keys, which I'm happy to share that this one is now implemented.

Here are a few examples about using `cl-ssh-keys` system with
encrypted private keys.

First, lets load the system.

``` common-lisp
CL-USER> (ql:quickload :cl-ssh-keys)
```

In order to parse an encrypted private key we need to (obviously)
provide a passphrase, e.g.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa" :passphrase "my-secret-password")
           (ssh-keys:key-cipher-name key))
"aes256-ctr"
```

The passphrase for an encrypted private key can be changed by setting
a new value for the passphrase using the `SSH-KEYS:KEY-PASSPHRASE`
accessor.

This example changes the passphrase for a given key and saves it on
the filesystem.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa" :passphrase "OLD-PASSPHRASE")
           (setf (ssh-keys:key-passphrase key) "MY-NEW-PASSPHRASE")
           (ssh-keys:write-key-to-path key #P"~/.id_rsa-new-passphrase"))
```

We can also encrypt an existing un-encrypted key. In order to do that
we simply set a passphrase using the `SSH-KEYS:KEY-PASSPHRASE`
accessor, e.g.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa")
           (setf (ssh-keys:key-passphrase key) "my-secret-password")
           (ssh-keys:write-key-to-path key #P"~/.id_rsa-encrypted"))
```

This will encrypt the key using the default cipher (`aes256-ctr` as of
today), using `16` iteration rounds for the KDF function in order to
derive an encryption key.

A passphrase of a private can also be removed by simply setting the
passphrase to `nil`, e.g.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa" :passphrase "PASSPHRASE")
           (setf (ssh-keys:key-passphrase key) nil)
           (ssh-keys:write-key-to-path key #P"~/.id_rsa-unencrypted"))
```

If we need to change the cipher for a private key we can do that as well,
which is useful when you have a private key, which has been encrypted using
an older or deprecated cipher.

The cipher to be used for encryption of a private key can be set by
using the `SSH-KEYS:KEY-CIPHER-NAME` accessor. The value should be one
of the known and supported ciphers as returned by
`SSH-KEYS:GET-ALL-CIPHER-NAMES`.

First, list the known cipher names.

``` common-lisp
CL-USER> (ssh-keys:get-all-cipher-names)
("3des-cbc" "aes128-cbc" "aes192-cbc" "aes256-cbc" "aes128-ctr" "aes192-ctr" "aes256-ctr" "none")
```

Then set a new cipher.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa" :passphrase "PASSPHRASE")
           (setf (ssh-keys:key-cipher-name key) "3des-cbc")
           (ssh-keys:write-key-to-path key #P"~/.id_rsa-3des-cbc"))
```

And finally, we can change the number of iterations which is used by
the KDF function in order to derive the encryption key.

By default `ssh-keygen(1)` and `cl-ssh-keys` will use `16` rounds of
iterations in order to produce an encryption key. You can set this to
a higher value, if needed, which would help against brute-force
attacks.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa" :passphrase "PASSPHRASE")
           (setf (ssh-keys:key-kdf-rounds key) 32)
           (ssh-keys:write-key-to-path key #P"~/.id_rsa-stronger"))
```

For more information and additional examples, please refer to the
[cl-ssh-keys][cl-ssh-keys] repo.

[cl-ssh-keys]: https://github.com/dnaeon/cl-ssh-keys
