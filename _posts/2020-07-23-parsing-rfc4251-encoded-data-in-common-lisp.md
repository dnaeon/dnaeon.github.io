---
layout: post
title: Decoding RFC 4251 binary data in Common Lisp
tags: parsing decoding lisp programming rfc4251 binary data
---
[RFC 4251](https://tools.ietf.org/html/rfc4251) describes the Secure
Shell (SSH) Protocol Architecture, which also contains a section of
the various [Data Type Representations Used in the SSH
Protocols](https://tools.ietf.org/html/rfc4251#section-5).

Section 5 of the RFC document defines the data types, which are also
used to describe an [OpenSSH certificate
key](https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.certkeys?annotate=HEAD),
and my goal is to be able to decode an OpenSSH certificate using
Common Lisp by following the specification defined in the RFC
document.

In this post I will show you how to decode different types of data
from a binary stream, which have been encoded using the RFC 4251,
section 5 specification.

In order to parse RFC 4251 encoded data we will use the
[cl-rfc4251](https://github.com/dnaeon/cl-rfc4251) system.

``` shell
CL-USER> (ql:quickload :cl-rfc4251)
```

The table below summarizes the supported data types, that can be
decoded by the `cl-rfc4251` system. The `RFC 4251` and `cl-rfc4251
type` columns specify the mapping between the RFC defined data type
name and the keywords used to decode a given value in Common Lisp.

| RFC 4251    | cl-rfc4251 type | Description                                      |
|-------------|-----------------|--------------------------------------------------|
| `byte`      | `:byte`         | An arbitrary 8-bit value (octet)                 |
| `boolean`   | `:boolean`      | A boolean value, either `T` or `NIL`             |
| `uint16`    | `:uint16`       | Unsigned 16-bit integer in big-endian byte order |
| `uint32`    | `:uint32`       | Unsigned 32-bit integer in big-endian byte order |
| `uint64`    | `:uint64`       | Unsigned 64-bit integer in big-endian byte order |
| `string`    | `:string`       | Arbitrary length string                          |
| `mpint`     | `:mpint`        | Multiple precision integer                       |
| `name-list` | `:name-list`    | A list of string names                           |

In addition to the above data types, the following ones are also
supported, which are not directly specified in RFC 4251, but are also
useful on their own, depending on your use case.

| cl-rfc4251 type | Description                                         |
|-----------------|-----------------------------------------------------|
| `:raw-bytes`    | Read a sequence of raw bytes up to a given length   |
| `:uint16-le`    | Unsigned 16-bit integer in little-endian byte order |
| `:uint32-le`    | Unsigned 32-bit integer in little-endian byte order |
| `:uint64-le`    | Unsigned 64-bit integer in little-endian byte order |

The `cl-rfc4251` system exports the generic function `DECODE` via the
`CL-RFC4251` (also available via its nickname `RFC4251`) package.

The `RFC4251:DECODE` function takes a *type* and a *binary stream*
from which to decode. Some types also take additional keyword
parameters (e.g. `:raw-bytes`), which allow to specify the number of
bytes to be decoded.

In all of the examples that follow below `s` represents a binary
stream. You can also use the `RFC4251:MAKE-BINARY-INPUT-STREAM`
function to create a binary stream, which uses a vector for the
underlying data.

Decode raw bytes with a given length from the binary stream `s`.

``` common-lisp
CL-USER> (rfc4251:decode :raw-bytes s :length 2)
```

Decode a 16-bit unsigned integer represented in [big-endian byte
order](https://en.wikipedia.org/wiki/Endianness) from a given binary
stream `s`.

``` common-lisp
CL-USER> (rfc4251:decode :uint16 s)
```

Decode a multiple precision integer represented in [two's
complement format](https://en.wikipedia.org/wiki/Two%27s_complement) from the
binary stream `s`.

``` common-lisp
CL-USER> (rfc4251:decode :mpint s)
```

You can find other examples in the included [test
suite](https://github.com/dnaeon/cl-rfc4251/tree/master/t).

As part of the next post I will show you how we can use the
[cl-rfc4251](https://github.com/dnaeon/cl-rfc4251) system in order to
decode [OpenSSH certificate
keys](https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.certkeys?annotate=HEAD).
