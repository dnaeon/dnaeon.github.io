---
layout: post
title: Computing fingerprints of OpenSSH public keys in Common Lisp
tags: encoding decoding programming rfc4251 rfc4253 rfc4716 binary data openssh fingerprint public-key
---
[Public key
fingerprint](https://en.wikipedia.org/wiki/Public_key_fingerprint)
represents the identity of a public key, which you get after applying
a hashing function to the binary blob that makes up a public key.

They are easier for humans to process, remember and recognize than the
larger binary blob, which represents the actual public key. You've
seen fingerprints being used when logging in to a remote server via
SSH, e.g.

``` shell
$ ssh openbsd.org
The authenticity of host 'openbsd.org (129.128.5.194)' can't be established.
ECDSA key fingerprint is SHA256:gQfDbjZSy3MsNSNwdzAhsdz12JmCf7FORsohBmBLFDc.
Are you sure you want to continue connecting (yes/no/[fingerprint])?
```

You can get the fingerprint of a public key using the `ssh-keygen(1)`
command.

``` shell
$ ssh-keygen -lf ~/.ssh/id_rsa.pub
3072 SHA256:/c7RbZ0MqbDQi/VT30AOi8UoiJVe7Q3vS5VKE+JDS2g john.doe@localhost (RSA)
```

By default `ssh-keygen(1)` will print the SHA-256 fingerprint of a public key,
but you can also get the SHA-1 or MD5 fingerprint as well, e.g.

``` shell
$ ssh-keygen -E md5 -lf ~/.ssh/id_rsa.pub
3072 MD5:3b:47:fa:b3:04:39:25:86:99:f4:62:58:32:7f:22:e2 john.doe@localhost (RSA)
```

So, how does OpenSSH compute the fingerprint of a public key?

The fingerprint is computed against the binary blob, which makes up
the actual public key. [RFC 4253][RFC 4253] defines the following format
for RSA public keys.

``` text
The "ssh-rsa" key format has the following specific encoding:

    string    "ssh-rsa"
    mpint     e
    mpint     n
```

And DSA keys have the following format.

``` text
The "ssh-dss" key format has the following specific encoding:

    string    "ssh-dss"
    mpint     p
    mpint     q
    mpint     g
    mpint     y
```

The data types like `string` and `mpint` from above definitions are
the ones defined in [RFC 4251][RFC 4251], and [RFC 4176][RFC 4716],
section 4 also discusses public key fingerprints.

In the rest of this post I will show you how you can use Common Lisp
in order to compute the fingerprint of an OpenSSH public key.

This is the example public key which we will use.

``` text
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQClthZEF66P7Rd5G7WMhuN/FRIn3Qpqf/rwLcy9ojkdNSZq/rFByjAr2s30N+6RpvCunLDFlspX0/83rYg58F1OcjXZFnn6+xXnQzmht8NuvBmpyrktWlICl++wB9bO1Ouof1DDZyqDrgTzuR5gmPtwbOKd/X31scNpuIEG9hdrxnyFUwVItyAnd9axAeBCJgb4EIg/OpUIv/lhNaHPvZrXAyQPQvY5wTOn5LAbXG/cwTH6kBLpVlqbofnu6b3xmyQSyvq057tFpG8BSOIBNa89azQlXLPO8szXWLn9NAZdDpjfi0vZj+MxcOJjVv7AN58SAlKhWnpLz0KIJhF/68vE8ziMT63ii02g/oe2T28AJM7VLOr/7jekIQwLDPuLlZxxyrzkYb8m1m97emFgzl9wFcm9C2yiJEo9M9B8e8EDu8W+xUDlxa6H/IfKrL2/jgfxzLNVPObx0cZyN+OSKfIkyfatg1/+RpJ85q6Wv7G7OwTOe3h0AFyVydciEFfz1Vc= john.doe@localhost
```

OpenSSH public keys when represented in text format have three
sections -- key type name (e.g. "ssh-rsa"), base64 encoded section
representing the actual public key, and a comment
(e.g. "john.doe@localhost" from above example).

In order to compute the fingerprint of above public key we need to get
the base64 encoded section, then decode it, then apply a hashing
function (e.g. SHA-256), and finally base64 encode the result.

Lets start up our Lisp REPL and load a few systems, which we will
need.

``` common-lisp
CL-USER> (ql:quickload :binascii)
CL-USER> (ql:quickload :ironclad)
```

We will also introduce a helper function, which returns the various parts
of a public key file.

``` common-lisp
(defun public-key-parts (path)
  "Returns the various parts of a public key file"
  (with-open-file (in path)
    (uiop:split-string (read-line in) :separator '(#\Space))))
```

Now we can define a function, which computes the SHA-256 fingerprint
of a given public key file.

``` common-lisp
(defun fingerprint-sha256 (path)
  "Computes the SHA-256 fingerprint of a public key"
  (let* ((parts (public-key-parts path))                              ;; Split the public key parts
         (key-type (first parts))                                     ;; Key type (e.g. "ssh-rsa")
         (encoded-blob (second parts))                                ;; The base64 encoded public key
         (decoded-blob (binascii:decode-base64 encoded-blob))         ;; Decode the private key
         (digest (ironclad:digest-sequence :sha256 decoded-blob))     ;; Compute the hash of the public key
         (encoded-digest (binascii:encode-base64 digest))             ;; base64 encode the hash
         (trim-position (position #\= encoded-digest :test #'char=))) ;; Trim padding characters at this position
    (list :key-type key-type
          :fingerprint (subseq encoded-digest 0 trim-position))))
```

The thing that you should notice in above function is that we also
need to trim the `=` padding characters.

The MD5 fingerprint is computed in a slightly different way.

``` common-lisp
(defun fingerprint-md5 (path)
  "Computes the MD5 fingerprint of a public key"
  (let* ((parts (public-key-parts path))                            ;; Split the public key parts
         (key-type (first parts))                                   ;; Key type (e.g. "ssh-rsa")
         (encoded-blob (second parts))                              ;; The base64 encoded public key
         (decoded-blob (binascii:decode-base64 encoded-blob))       ;; Decode the private key
         (digest (ironclad:digest-sequence :md5 decoded-blob)))     ;; Compute the hash of the public key
    (list :key-type key-type
          :fingerprint (format nil "~{~(~2,'0x~)~^:~}" (coerce digest 'list)))))
```

Lets try them out. We should see the same fingerprints as reported by `ssh-keygen(1)`.

``` common-lisp
CL-USER> (fingerprint-sha256 "~/.ssh/id_rsa.pub")
(:KEY-TYPE "ssh-rsa" :FINGERPRINT "/c7RbZ0MqbDQi/VT30AOi8UoiJVe7Q3vS5VKE+JDS2g")
CL-USER> (fingerprint-md5 #P"~/.ssh/id_rsa.pub")
(:KEY-TYPE "ssh-rsa" :FINGERPRINT "3b:47:fa:b3:04:39:25:86:99:f4:62:58:32:7f:22:e2")
```

`ssh-keygen(1)` can also report the fingerprint of a private key. What
actually happens when you print the fingerprint of a private key is
that OpenSSH will in fact compute the fingerprint of the public key,
which is embedded within the private key. You can find out more about
the OpenSSH private key format in [The OpenSSH private key binary
format](http://dnaeon.github.io/openssh-private-key-binary-format/)
post.

[RFC 4251]: https://tools.ietf.org/html/rfc4251
[RFC 4253]: https://tools.ietf.org/html/rfc4253
[RFC 4716]: https://tools.ietf.org/html/rfc4716
