---
layout: post
title: bcrypt password hashing in Common Lisp
tags: bcrypt password hashing common-lisp lisp
---
Native support for [bcrypt][bcrypt] password hashes was somewhat missing
in the Common Lisp ecosystem, unless you count the various CFFI wrappers.

In recent releases of [ironclad][ironclad] you can now find native
Common Lisp implementations for `bcrypt` and `bcrypt-pbkdf`.  Both of
these have been implemented as part of this
[request](https://github.com/sharplispers/ironclad/issues/34).

Given the recently added support for `bcrypt` in `ironclad` I've
packaged things up in a new Common Lisp system, which makes it easy to
work with `bcrypt` password hashes in Common Lisp -
[cl-bcrypt][cl-bcrypt].

With [cl-bcrypt][cl-bcrypt] we can now generate, parse and verify
bcrypt password hashes.

First, lets load the `cl-bcrypt` system.

``` common-lisp
CL-USER> (ql:quickload :cl-bcrypt)
```

The supported hash algorithm identifiers by `cl-bcrypt` are `2a` and
`2b`.

In order to create a new bcrypt password you need to use the
`BCRYPT:MAKE-PASSWORD` function, e.g.

``` common-lisp
CL-USER> (defparameter *password*
           (bcrypt:make-password "my-secret-password"))
*PASSWORD*
```

`BCRYPT:MAKE-PASSWORD` accepts keyword parameters, which allow you to
specify a different salt (e.g. obtained by `BCRYPT:GENERATE-SALT`),
different cost factor than the default, and a different algorithm
identifier than the default (e.g. `2a`).

If you don't specify explicitely a salt, a random one will be
generated for you by the `BCRYPT:GENERATE-SALT` function.

This example specifies a cost factor of `16` and a hash algorithm
identifier `2a`.

``` common-lisp
CL-USER> (defparameter *password*
           (bcrypt:make-password "my-secret-password" :cost 16 :identifier "2a"))
*PASSWORD*
```

You can use the `BCRYPT:ALGORITHM-IDENTIFIER`, `BCRYPT:COST-FACTOR`,
`BCRYPT:SALT` and `BCRYPT:PASSWORD-HASH` readers to inspect the
returned `BCRYPT:PASSWORD` instance from the `BCRYPT:MAKE-PASSWORD`
function, e.g.

``` common-lisp
CL-USER> (bcrypt:algorithm-identifier *password*)
"2a"
CL-USER> (bcrypt:cost-factor *password*)
16
CL-USER> (bcrypt:salt *password*)
#(18 117 245 59 29 97 63 72 199 11 254 164 52 87 213 169)
CL-USER> (bcrypt:password-hash *password*)
#(94 0 171 116 90 235 30 220 57 45 147 214 210 77 244 223 63 14 153 13 140 213 183)
```

The `BCRYPT:SALT` and `BCRYPT:PASSWORD-HASH` readers return the raw
bytes of the salt and the password hash respectively.

In order to encode a `BCRYPT:PASSWORD` instance into its text
representation you need to use the `BCRYPT:ENCODE` function.

``` common-lisp
CL-USER> (bcrypt:encode *password*)
"$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa"
```

A bcrypt password hash can be decoded using the `BCRYPT:DECODE` function,
which will return a new instance of `BCRYPT:PASSWORD`, e.g.

``` common-lisp
CL-USER> (bcrypt:decode "$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa")
#<CL-BCRYPT:PASSWORD {1002207AD3}>
```

If you encode back the returned instance you should get the same hash
string as the one that was decoded.

The `BCRYPT:PARSE-HASH` function returns a property list of the
parts that comprise the bcrypt hash string.

``` common-lisp
CL-USER> (bcrypt:parse-hash "$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa")
(:ALGORITHM-IDENTIFIER "2a"
 :COST-FACTOR "16"
 :SALT "ClVzMvzfNyhFA94iLDdToO"
 :PASSWORD-HASH "VeApbDppFru3JXNUyi1y1x6MkO0KzZa")
```

When you need to test whether a given bcrypt hash matches a given
password you can use the `BCRYPT:PASSWORD=` predicate, e.g.

``` common-lisp
CL-USER> (bcrypt:password= "my-secret-password"
                           "$2a$16$ClVzMvzfNyhFA94iLDdToOVeApbDppFru3JXNUyi1y1x6MkO0KzZa")
T
```

For more information, please refer to the [cl-bcrypt][cl-bcrypt] repo.

[cl-bcrypt]: https://github.com/dnaeon/cl-bcrypt
[ironclad]: https://github.com/sharplispers/ironclad
[bcrypt]: https://en.wikipedia.org/wiki/Bcrypt
