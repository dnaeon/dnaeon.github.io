---
layout: post
title: Parsing and generating OpenSSH keys with Common Lisp
tags: encoding decoding programming rfc4251 rfc4253 common-lisp lisp ssh
---
In previous posts I've discussed how you can
[decode](http://dnaeon.github.io/parsing-rfc4251-encoded-data-in-common-lisp/)
and
[encode](http://dnaeon.github.io/encoding-rfc4251-data-in-common-lisp/)
data in [RFC 4251][RFC 4251] binary format using Common Lisp.

This is a follow-up to these posts and shows some of the features for
a new Common Lisp system I've been working on recently -
[cl-ssh-keys][cl-ssh-keys].

The [cl-ssh-keys][cl-ssh-keys] system is built around my previous
Common Lisp project [cl-rfc4251][cl-rfc4251], which provides the
fundamental blocks for encoding and decoding OpenSSH binary data.

Currently with `cl-ssh-keys` you can decode, encode and generate new
OpenSSH private and public key pairs of the following key types.

* RSA
* DSA
* ED25519

The generated keys can be dumped in its text representation, so you
can use these for public-key authentication on remote system, just
like you would when using keys generated with `ssh-keygen(1)`.

The system is not yet in [Quicklisp][Quicklisp], so if you intend on
using it, you should simply clone the [cl-ssh-keys][cl-ssh-keys] to
your [Quicklisp local projects directory][Quicklisp local projects
directory].

Also, for the actual key generation `cl-ssh-keys` uses
[ironclad][ironclad], and there has been one new [MR to
ironclad](https://github.com/sharplispers/ironclad/pull/32), which is
required, so make sure that you have the latest version of ironclad as
well, which has the above MR already merged.

ECDSA keys are not supported at this moment, but may eventually be
added to `cl-ssh-keys`, depending on whether [support for
ECDSA](https://github.com/sharplispers/ironclad/issues/33) lands in
ironclad.

With that said, lets see what we can do with `cl-ssh-keys`.

``` common-lisp
CL-USER> (ql:quickload :cl-ssh-keys)
```

### Public keys

We can load a public key from a given path using the
`SSH-KEYS:PARSE-PUBLIC-KEY-FILE` function, e.g.

``` common-lisp
CL-USER> (defparameter *public-key*
           (ssh-keys:parse-public-key-file #P"~/.ssh/id_rsa.pub"))
*PUBLIC-KEY*
```

You can retrieve the comment associated with a public key by using the
`SSH-KEYS:KEY-COMMENT` accessor.

``` common-lisp
CL-USER> (ssh-keys:key-comment *public-key*)
"john.doe@localhost"
```

The key kind can be retrieved using `SSH-KEYS:KEY-KIND`.

``` common-lisp
CL-USER> (ssh-keys:key-kind *public-key*)
(:NAME "ssh-rsa" :PLAIN-NAME "ssh-rsa" :SHORT-NAME "RSA" :ID :SSH-RSA :IS-CERT NIL)
```

The number of bits for a key can be retrieved using the
`SSH-KEYS:KEY-BITS` generic function, e.g.

``` common-lisp
CL-USER> (ssh-keys:key-bits *public-key*)
3072
```

`SSH-KEYS:WITH-PUBLIC-KEY` and `SSH-KEYS:WITH-PUBLIC-KEY-FILE`
are convenient macros when working with public keys, e.g.

``` common-lisp
CL-USER> (ssh-keys:with-public-key-file (key #P"~/.ssh/id_rsa.pub")
           (format t "Comment: ~a~%" (ssh-keys:key-comment key))
           (format t "MD5 fingerprint: ~a~%" (ssh-keys:fingerprint :md5 key))
           (format t "Number of bits: ~a~%" (ssh-keys:key-bits key)))
Comment: john.doe@localhost
MD5 fingerprint: 04:02:4b:b2:43:39:a4:8e:89:47:49:6f:30:78:94:1e
Number of bits: 3072
NIL
```

### Private keys

A private keys can be parsed using the `SSH-KEYS:PARSE-PRIVATE-KEY`
function, which takes a string representing a private key in [OpenSSH
private key format][PROTOCOL.key], or you can use the
`SSH-KEYS:PARSE-PRIVATE-KEY-FILE` function, e.g.

``` common-lisp
CL-USER> (defparameter *private-key*
           (ssh-keys:parse-private-key-file #P"~/.ssh/id_rsa"))
*PRIVATE-KEY*
```

Key kind, comment and number of bits can be retrieved using
`SSH-KEYS:KEY-KIND`, `SSH-KEYS:KEY-COMMENT` and `SSH-KEYS:KEY-BITS`,
similarly to the way you would for public keys, e.g.

``` common-lisp
CL-USER> (ssh-keys:key-kind *private-key*)
(:NAME "ssh-rsa" :PLAIN-NAME "ssh-rsa" :SHORT-NAME "RSA" :ID :SSH-RSA :IS-CERT NIL)
CL-USER> (ssh-keys:key-comment *private-key*)
"john.doe@localhost"
CL-USER> (ssh-keys:key-bits *private-key*)
3072
```

OpenSSH private keys embed the public key within the binary blob of
the private key. From a private key you can get the embedded public
key using `SSH-KEYS:EMBEDDED-PUBLIC-KEY`, e.g.

``` common-lisp
CL-USER> (ssh-keys:embedded-public-key *private-key*)
#<CL-SSH-KEYS:RSA-PUBLIC-KEY {100619EAB3}>
```

You can also use the `SSH-KEYS:WITH-PRIVATE-KEY` and
`SSH-KEYS:WITH-PRIVATE-KEY-FILE` macros when working with private
keys.

``` common-lisp
CL-USER> (ssh-keys:with-private-key-file (key #P"~/.ssh/id_rsa")
           (format t "Comment: ~a~%" (ssh-keys:key-comment key))
           (format t "MD5 fingerprint: ~a~%" (ssh-keys:fingerprint :md5 key)))
Comment: john.doe@localhost
MD5 fingerprint: 04:02:4b:b2:43:39:a4:8e:89:47:49:6f:30:78:94:1e
```

### Fingerprints

[Public key fingerprint][Public key fingerprint] can be computed using
the `SSH-KEYS:FINGERPRINT` generic function.

The following examples show how to compute the SHA-256, SHA-1 and MD5
fingerprints of a given public key.

``` common-lisp
CL-USER> (ssh-keys:fingerprint :sha256 *public-key*)
"VmYpd+5gvA5Cj57ZZcI8lnFMNNic6jpnnBd0WoNG1F8"
CL-USER> (ssh-keys:fingerprint :sha1 *public-key*)
"RnLPLG93GrABjOqc6xOvVFpQXsc"
CL-USER> (ssh-keys:fingerprint :md5 *public-key*)
"04:02:4b:b2:43:39:a4:8e:89:47:49:6f:30:78:94:1e"
```

Fingerprints of private keys are computed against the embedded public
key.

### Writing keys

A public and private key can be dumped in its text representations
using the `SSH-KEYS:WRITE-KEY` generic function.

``` common-lisp
CL-USER> (ssh-keys:write-key *public-key*)
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCsngzCcay+lQ+34qUeUSH2m1ZYW9B0a2rxpMmvYFcOyL/hRPJwv8XO89T0+HQIZRC+xlM3BSqdFGs+B58MYXPvo3H+p00CJN8tUjvC3VD74kiXSNxIyhBpKCY1s58RxnWS/6bPQIYfnCVBiQZnkNe1T3isxND1Y71TnbSz5QN2xBkAtiGPH0dPM89yWbZpTjTCaIOfyZn2fBBsmp0zUgEJ7o9W9Lrxs1f0Pn+bZ4PqFSEUzlub7mAQ+RpwgGeLeWIFz+o6KQJPFiuRgzQU6ZsY+wjorVefzgeqpRiWGw/bEyUDck09B4B0IWoTtIiKRzd635nOo7Lz/1XgaMZ60WZD9T/labEWcKmtp4Y7NoCkep0DyYyoAgWrco4FD1r0g4WcVbsJQt8HzRy9UaHlh6YPY/xkk0bSiljpygEiT48FxniqE+6HY+7SbC1wz5QThY+UsIiDgFcg3BljskfT8Il3hateXI2wEXqww4+a+DxcHzypclYorbQKUzdzNLZRBNk= john.doe@localhost
NIL
```

Another example, this time using a private key.

``` common-lisp
CL-USER> (ssh-keys:write-key *private-key*)
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAABlwAAAAdzc2gtcn
NhAAAAAwEAAQAAAYEArJ4MwnGsvpUPt+KlHlEh9ptWWFvQdGtq8aTJr2BXDsi/4UTycL/F
zvPU9Ph0CGUQvsZTNwUqnRRrPgefDGFz76Nx/qdNAiTfLVI7wt1Q++JIl0jcSMoQaSgmNb
OfEcZ1kv+mz0CGH5wlQYkGZ5DXtU94rMTQ9WO9U520s+UDdsQZALYhjx9HTzPPclm2aU40
wmiDn8mZ9nwQbJqdM1IBCe6PVvS68bNX9D5/m2eD6hUhFM5bm+5gEPkacIBni3liBc/qOi
kCTxYrkYM0FOmbGPsI6K1Xn84HqqUYlhsP2xMlA3JNPQeAdCFqE7SIikc3et+ZzqOy8/9V
4GjGetFmQ/U/5WmxFnCpraeGOzaApHqdA8mMqAIFq3KOBQ9a9IOFnFW7CULfB80cvVGh5Y
emD2P8ZJNG0opY6coBIk+PBcZ4qhPuh2Pu0mwtcM+UE4WPlLCIg4BXINwZY7JH0/CJd4Wr
XlyNsBF6sMOPmvg8XB88qXJWKK20ClM3czS2UQTZAAAFkJkcYpSZHGKUAAAAB3NzaC1yc2
EAAAGBAKyeDMJxrL6VD7fipR5RIfabVlhb0HRravGkya9gVw7Iv+FE8nC/xc7z1PT4dAhl
EL7GUzcFKp0Uaz4Hnwxhc++jcf6nTQIk3y1SO8LdUPviSJdI3EjKEGkoJjWznxHGdZL/ps
9Ahh+cJUGJBmeQ17VPeKzE0PVjvVOdtLPlA3bEGQC2IY8fR08zz3JZtmlONMJog5/JmfZ8
EGyanTNSAQnuj1b0uvGzV/Q+f5tng+oVIRTOW5vuYBD5GnCAZ4t5YgXP6jopAk8WK5GDNB
Tpmxj7COitV5/OB6qlGJYbD9sTJQNyTT0HgHQhahO0iIpHN3rfmc6jsvP/VeBoxnrRZkP1
P+VpsRZwqa2nhjs2gKR6nQPJjKgCBatyjgUPWvSDhZxVuwlC3wfNHL1RoeWHpg9j/GSTRt
KKWOnKASJPjwXGeKoT7odj7tJsLXDPlBOFj5SwiIOAVyDcGWOyR9PwiXeFq15cjbARerDD
j5r4PFwfPKlyViittApTN3M0tlEE2QAAAAMBAAEAAAGBAJT3DFHdYdNSti7d09sW7zVvlp
NIINvnO3Jv4HGNtXOXwSd5pbOxe9Z+TEBgDVqVRV8trfCkb8MBNQ9h6lr32uJqbdzyqh14
jnUBK3ueHN5SyIxuH1RdtM3bDSZ47YScfSivoVfn+hdbXDdzNei4cb8RZzXJ3/505ZU8Ww
6IS3X6Aw2/H7TwrExojNTFIQs9p4BCS5zgkRLKvC3NPG5mjWjxzBehuZcOS5AHQ35sVcX0
GAlpkFs/2v2qy6tc1H7j703RsrlJtXvLQ2fUGVXdZflMSlX1te+T+KM5T1unUS5fPFWfLj
U+bQK7KkY48ILVQkrFLGg+8Wj77MTS3AGmQ2MnHzaK0+Cd+HAqUfRIDZZgG/5/T8nIsra/
9AG2ZIvOTSZsLqht4TkfZnp6hJm+MKmpJ9F40NnzGtYNso6GD/aqkDxubKf4uoOEW9cbOO
s5i5bvvZSgxQ1sNees0/nBBYsRhLfYkC41EcCRlhQIcvHA1IFRj5Un0gowA8vtCGyRJQAA
AMEAuPkxyvsmPYIi0SbVNVMdEpaJ3UHTJOLL6b8QDPYsiuYG0DZfHgL1MSbgIrxUKI4Xi1
oEROgfGHnhnUd7mGbwUF/K0KnYJUMlV0W8Jfz94E7+cQiqgvvWD2JZcuvXP5Dg89whsFFy
pinpkrWe8gDmqo/LKzAEBIFAuNVarD7/cIKTpW+pdo7WfnYsXqTgyZ5NO8IwkTXho6NTRI
s/Z7o7UCXX2XnUcQxWOv+L5aw7w4dBdNZpN7XBQCOfOo32SDpQAAAAwQDYmJZrTrb5w5N+
o/j9nhcrY1ZbJNUbpx1lrV/r1GCGX0f3l2ztjjzyttP+WEggPypMB5BC+S6d67PEJeI988
OanzMx/r37tfFbMMtE5YNx1BwyL1Z1x/KYugReibWclHBAa+b+TCFSfJyf1I5NABsgjQ2h
4uVy1pRWcly4Cfu0NWRJo23waTzvODPWjUz1EFIcytpKvYxwbcvYOVEY5ie9+oXhVxNm6U
ZQTLMtPWNUZGHt3xOrGhrf4M7EJRLUBe8AAADBAMwFRHMyDsyjzlFZA1gL42xO4gCGwjJq
IZu+X6h1PV71IYyyY2XV9p6Ir9UZFeFs73wvO7I+OWW6POIKMKVOjjWTU5KD3+kSI2THWq
j/Cf8gr/aLqHOKa6X63meJCPSKC5CtHFchvAPvcUhfLLv7MfHJfwFU4vrBJh5w4h0TXKCU
8hIzudC5tinyYsDgv0i0keWxWAmKMxSxsfIQkqYtqMHc4E9EZ1baUsvAj8VolJcKn0Ocj9
tvLra3KkT8SoqptwAAABJqb2huLmRvZUBsb2NhbGhvc3QBAgMEBQYH
-----END OPENSSH PRIVATE KEY-----
NIL
```

The `SSH-KEYS:WRITE-KEY` generic function takes an optional stream
parameter, so you can write your keys to a given stream, if needed.

``` common-lisp
CL-USER> (with-open-file (out #P"my-rsa-public-key" :direction :output)
           (ssh-keys:write-key *public-key* out))
NIL
```

### Generating keys

With `cl-ssh-keys` we can generate new private/public key pairs using
the `SSH-KEYS:GENERATE-KEY-PAIR` generic function.

The generated keys are identical with what `ssh-keygen(1)` would
produce and you can use them to authenticate to remote systems.

The following example creates an RSA private/public key pair, and
saves the keys on the file system.

``` common-lisp
CL-USER> (multiple-value-bind (priv-key pub-key) (ssh-keys:generate-key-pair :rsa)
           (with-open-file (out #P"~/.ssh/my-priv-rsa-key" :direction :output)
             (ssh-keys:write-key priv-key out))
           (with-open-file (out #P"~/.ssh/my-pub-rsa-key.pub" :direction :output)
             (ssh-keys:write-key pub-key out)))
NIL
```

The following example generates DSA private/public key pairs.

``` common-lisp
CL-USER> (ssh-keys:generate-key-pair :dsa)
```

This example shows how to generate Ed25519 private/public key pairs.

``` common-lisp
CL-USER> (ssh-keys:generate-key-pair :ed25519)
```

### Next steps

There are a few additional things that I think would be useful to
have in `cl-ssh-keys`.

Having [ECDSA keys
support](https://github.com/sharplispers/ironclad/issues/33) is one of
them, as mentioned in the beggining of this post.

Another thing is having support for encrypted private keys. This one
is a bit tricky, because OpenSSH uses [bcrypt_pbkdf][bcrypt_pbkdf] as
the [KDF][KDF], which in turn is used to encrypt the key using a
supported cipher. Currently there is no implementation of
[bcrypt_pbkdf][bcrypt_pbkdf] KDF function in the popular cryptography
library for Common Lisp [ironclad][ironclad]. There is however a
[request for having
bcrypt_pbkdf](https://github.com/sharplispers/ironclad/issues/34)
implemented, but not sure when that will (or will ever) be
implemented.

There is also another possibility here, which is to use
[CFFI](https://common-lisp.net/project/cffi/) along with
[libssh2](https://www.libssh2.org), and simply call out the
foreign-function `bcrypt_pbkdf` function in order to generate an
encryption/decryption key, which can later be used with `ironclad` for
the actual encryption and decryption of the private key.

Depending on my spare time I may just go that route, but will leave
the details for another post.

[PROTOCOL.key]: https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.key?annotate=HEAD
[KDF]: https://en.wikipedia.org/wiki/Key_derivation_function
[bcrypt_pbkdf]: https://github.com/openssh/openssh-portable/blob/master/openbsd-compat/bcrypt_pbkdf.c
[Public key fingerprint]: https://en.wikipedia.org/wiki/Public_key_fingerprint
[Quicklisp]: https://www.quicklisp.org/beta/
[Quicklisp local projects directory]: https://www.quicklisp.org/beta/faq.html
[RFC 4251]: https://tools.ietf.org/html/rfc4251
[RFC 4253]: https://tools.ietf.org/html/rfc4253
[cl-rfc4251]: https://github.com/dnaeon/cl-rfc4251
[cl-ssh-keys]: https://github.com/dnaeon/cl-ssh-keys
[ironclad]: https://github.com/sharplispers/ironclad
[test suite]: https://github.com/dnaeon/cl-ssh-keys/blob/master/t/test-suite.lisp
