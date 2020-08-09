---
layout: post
title: Encoding data in RFC 4251 compliant format in Common Lisp
tags: encoding lisp programming rfc4251 binary data
---
In a previous post I've discussed how to [decode RFC 4251 binary data
in Common
Lisp](http://dnaeon.github.io/parsing-rfc4251-encoded-data-in-common-lisp/).
In this one we are going to see how to encode data in [RFC
4251](https://tools.ietf.org/html/rfc4251) compliant format.

Being able to encode data in RFC 4251 format would enable us to do
some interesting things, e.g. generation of private/public key pairs
and storing them on-disk in the proper format, just like you would do
when using `ssh-keygen(1)`. Another possibility would be to generate
[OpenSSH certificate
keys](https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.certkeys?annotate=HEAD)
and leasing these to clients when requested. You could also build
something completely different and new, but actually base your
protocol around the data types defined in RFC 4251.

Support for encoding data has already been merged in the
[cl-rfc4251](https://github.com/dnaeon/cl-rfc4251) repo, and the [test
suite](https://github.com/dnaeon/cl-rfc4251/tree/master/t) contains
test cases for all data types defined in the RFC document.

The `RFC4251:ENCODE` generic function is the one that is responsible
for encoding data into a binary stream. Also, there is now an
implementation of a binary output stream, based on [Gray
streams](https://www.cliki.net/Gray%20streams), which can be
created using the `RFC4251:MAKE-BINARY-OUTPUT-STREAM`.

Here are a few examples of encoding data using the `cl-rfc4251`
system. First, we need to load the system.

``` shell
CL-USER> (ql:quickload :cl-rfc4251)
```

The following example shows how to encode an unsigned 32-bit integer.

``` common-lisp
CL-USER> (defparameter *s* (rfc4251:make-binary-output-stream))
*S*
CL-USER> (rfc4251:encode :uint32 42 *s*)
4
CL-USER> (rfc4251:binary-output-stream-data *s*)
#(0 0 0 42)
```

Note the result of the second expression above -- the `RFC4251:ENCODE`
generic function returns the number of bytes written to the stream.

Above example, can be written this way as well, if you only care about
the actual encoded bytes.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :uint32 42 s)          ;; <- Encode the value into the stream
           (rfc4251:binary-output-stream-data s)) ;; <- Return the contents of the stream
#(0 0 0 42)
```

The following example encodes a string value.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :string "Hello, World!" s) ;; <- Encode the value into the stream
           (rfc4251:binary-output-stream-data s))     ;; <- Return the contents of the stream
#(0 0 0 13 72 101 108 108 111 44 32 87 111 114 108 100 33)
```

The following examples show how to encode `mpint` values according to
RFC 4251.

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x00 s)         ;; <- Encode the zero value
           (rfc4251:binary-output-stream-data s)) ;; <- Get the encoded data
#(0 0 0 0)
```

Here are a few more examples taken directly from the examples section
described in [RFC 4251][RFC 4251]

``` common-lisp
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x-DEADBEEF s)
           (rfc4251:binary-output-stream-data s))
#(0 0 0 5 255 33 82 65 17)
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x80 s)
           (rfc4251:binary-output-stream-data s))
#(0 0 0 2 0 128)
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x9A378F9B2E332A7 s)
           (rfc4251:binary-output-stream-data s))
#(0 0 0 8 9 163 120 249 178 227 50 167)
CL-USER> (let ((s (rfc4251:make-binary-output-stream)))
           (rfc4251:encode :mpint #x-1234 s)
           (rfc4251:binary-output-stream-data s))
#(0 0 0 2 237 204)
```

For additional examples, make sure to check out the included [test
suite](https://github.com/dnaeon/cl-rfc4251/tree/master/t).
