---
layout: post
title: PKCS #1 RSASSA-PKCS1-v1_5 Signature Scheme with Appendix in Common Lisp
tags: lisp common-lisp ssh ssh-keys private-key rfc8017 rfc4251
---
[RSASSA-PKCS1-v1_5][RFC 8017] signature scheme with Appendix is
defined as part of [RFC 8017][RFC 8017].

Currently [ironclad][ironclad] cannot verify RSASSA-PKCS1-v1_5
signatures, so if you have a message and a signature you want to
verify you have to do it on your own. See [this
issue](https://github.com/sharplispers/ironclad/issues/41) in the
[ironclad][ironclad] issue tracker for more details.

Fortunately, [RFC 8017][RFC 8017] provides a good amount of details,
and the majority of the primitive operations from the RFC document are
already part of `ironclad`, so we can implement the missing bits on
our own.

My use case for RSASSA-PKCS1-v1_5 signatures is related to OpenSSH
certificate keys as described in
[PROTOCOL.certkey][PROTOCOL.certkey]. I need to parse an OpenSSH
Certificate Key and in order to confirm it's authenticity and
integrity, I also need to verify the digital signature that is
embedded within in. This is related to my [cl-ssh-keys][cl-ssh-keys]
project.

Make sure that you have Quicklisp and Ironclad loaded into your image.

``` common-lisp
CL-USER> (ql:quickload :ironclad)
```

Below you can find code which implements `RSASSA-PKCS1-v1_5-VERIFY`
and `RSASSA-PKCS1-v1_5-SIGN` functions from `RFC 8017`.

``` common-lisp
;;;;
;;;; Data Conversion Primitives - RFC 8017, section 4
;;;;

(defun i2osp (n &key n-bits)
  "Integer-to-Octet-String primitive. See RFC 8017, section 4.1"
  (declare (type integer n))
  (let ((n-bits (or n-bits (integer-length n))))
    (ironclad:integer-to-octets n :n-bits n-bits)))

(defun os2ip (octet-vec)
  "Octet-String-to-Integer primitive. See RFC 8017, section 4.2"
  (ironclad:octets-to-integer octet-vec))

;;;;
;;;; Signature and verification primitives - RFC 8017, section 5.2
;;;;

(defun rsasp1 (priv-key message)
  "RSA signature primitive. See RFC 8017, section 5.2.1"
  (declare (type integer message))
  (let ((n (ironclad:rsa-key-modulus priv-key))
        (d (ironclad:rsa-key-exponent priv-key)))
    (unless (<= 0 message (1- n))
      (error "message representative out of range"))
    (ironclad:expt-mod message d n)))

(defun rsavp1 (public-key signature)
  "RSA verification primitive. See RFC 8017, section 5.2.2"
  (declare (type integer signature))
  (let ((n (ironclad:rsa-key-modulus public-key))
        (e (ironclad:rsa-key-exponent public-key)))
    (unless (<= 0 signature (1- n))
      (error "signature representative out of range"))
    (ironclad:expt-mod signature e n)))

;;;;
;;;; Encoding Methods for Signatures with Appendix
;;;;

(defparameter *emsa-pkcs1-v1_5-digest-info*
  '(:md2    #(#x30 #x20 #x30 #x0c #x06 #x08 #x2a #x86 #x48 #x86 #xf7 #x0d #x02 #x02 #x05 #x00 #x04 #x10)
    :md5    #(#x30 #x20 #x30 #x0c #x06 #x08 #x2a #x86 #x48 #x86 #xf7 #x0d #x02 #x05 #x05 #x00 #x04 #x10)
    :sha1   #(#x30 #x21 #x30 #x09 #x06 #x05 #x2b #x0e #x03 #x02 #x1a #x05 #x00 #x04 #x14)
    :sha256 #(#x30 #x31 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x01 #x05 #x00 #x04 #x20)
    :sha384 #(#x30 #x41 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x02 #x05 #x00 #x04 #x30)
    :sha512 #(#x30 #x51 #x30 #x0d #x06 #x09 #x60 #x86 #x48 #x01 #x65 #x03 #x04 #x02 #x03 #x05 #x00 #x04 #x40))
  "DigestInfo DER encoding of the known hash functions. See RFC 8017, section 9.2, notes 1.")

(defun emsa-pkcs1-v1_5-encode (digest-spec message em-len)
  "EMSA-PKCS1-v1_5 encoding method. See RFC 8017, section 9.2"
  (unless (member digest-spec *emsa-pkcs1-v1_5-digest-info*)
    (error "unsupported digest spec"))
  (let* ((algorithm-identifier (getf *emsa-pkcs1-v1_5-digest-info* digest-spec))
         (h (ironclad:digest-sequence digest-spec message)) ;; Step 1
         (tt (concatenate '(vector (unsigned-byte 8) *)     ;; Step 2
                          algorithm-identifier
                          h))
         (tt-len (length tt)))
    (when (< em-len (+ tt-len 11)) ;; Step 3
      (error "intended encoded message length too short"))
    (let* ((ps (make-array (- em-len tt-len 3)  ;; Step 4
                           :element-type '(unsigned-byte 8)
                           :initial-element #xff)))
      (when (< (length ps) 8)
        (error "PS length should be at least 8 octets"))
      ;; Step 5 and 6
      (concatenate '(vector (unsigned-byte 8) *)
                   #(#x00 #x01) ps #(#x00) tt))))

;;;;
;;;; Signature Scheme with Appendix - RFC 8017, section 8
;;;;

(defun rsassa-pkcs1-v1_5-sign (priv-key message digest-spec)
  "RSASSA-PKCS1-v1_5 signature generation. See RFC 8017, section 8.2.1"
  (let* ((n (ironclad:rsa-key-modulus priv-key))
         (k (ceiling (integer-length n) 8))
         (em (emsa-pkcs1-v1_5-encode digest-spec message k))  ;; Step 1
         (m (os2ip em))  ;; Step 2a
         (s (rsasp1 priv-key m)))  ;; Step 2b
    ;; Step 2c and 3
    (i2osp s :n-bits (* 8 k))))

(defun rsassa-pkcs1-v1_5-verify (public-key message signature digest-spec)
  "RSASSA-PKCS1-v1_5 signature verification. See RFC 8017, section 8.2.2"
  (let* ((n (ironclad:rsa-key-modulus public-key))
         (k (ceiling (integer-length n) 8)))
    ;; Step 1
    (unless (= k (length signature))
      (error "invalid signature"))
    (let* ((s (os2ip signature))                                       ;; Step 2a
           (m (rsavp1 public-key s))                                   ;; Step 2b
           (em (i2osp m :n-bits (* 8 k)))                              ;; Step 2c
           (em-prime (emsa-pkcs1-v1_5-encode digest-spec message k)))  ;; Step 3
      ;; Step 4
      (equalp em em-prime))))
```

You should now be able to sign and verify messages with
`RSASSA-PKCS1-v1_5` signatures using the `rsassa-pkcs1-v1_5-sign` and
`rsassa-pkcs1-v1_5-verify` functions. These functions expect an RSA
private and public key respectively, which you can either generate
using Ironclad, or by loading existing keys from the file system using
`cl-ssh-keys`.

Here's an example where we first generate a new private/public key-pair,
then sign a new message and verify it's signature.

``` common-lisp
CL-USER> (multiple-value-bind (priv pub) (ironclad:generate-key-pair :rsa :num-bits 3072)
	   (let* ((m (ironclad:ascii-string-to-byte-array "Hello, World!"))
                  (s (rsassa-pkcs1-v1_5-sign priv m :sha512)))
	     (format t "Message: ~a~%" m)
	     (format t "Signature: ~a~%" s)
	     (format t "Signature verified: ~a~%" (rsassa-pkcs1-v1_5-verify pub m s :sha512))))
Message: #(72 101 108 108 111 44 32 87 111 114 108 100 33)
Signature: #(125 61 95 232 108 191 26 204 56 50 127 183 113 35 75 240 156 243
             52 246 100 65 1 143 186 87 211 60 1 167 103 40 162 60 168 12 160
             38 184 62 135 42 128 48 28 107 24 49 18 60 156 94 141 81 91 57 124
             91 157 186 244 204 223 213 248 209 101 207 212 108 172 252 208 41
             231 94 13 128 250 124 95 9 185 250 207 163 191 95 121 219 162 103
             65 142 108 112 183 134 8 236 40 244 220 224 88 7 64 205 102 150 97
             50 30 162 182 136 181 3 139 127 136 202 18 112 145 87 0 37 210 170
             235 59 199 243 135 70 145 91 58 131 148 181 205 76 132 102 115 245
             15 173 45 54 59 163 38 164 133 158 100 248 207 214 54 105 61 194
             29 238 27 197 133 201 250 10 44 113 55 20 63 107 229 247 154 240
             126 90 88 248 91 155 102 255 28 25 15 187 1 27 234 152 153 246 49
             244 192 121 187 172 105 180 59 81 4 135 152 170 192 196 77 143 220
             254 2 141 121 76 211 33 36 31 173 117 72 144 99 73 124 236 116 128
             57 242 35 124 85 200 10 124 80 226 28 222 153 8 27 172 254 213 159
             147 32 117 28 104 199 137 118 91 231 25 9 113 108 85 187 244 164
             79 188 100 11 53 215 94 67 29 60 57 116 196 12 8 125 38 66 233 160
             187 111 244 159 11 65 99 61 130 9 233 32 67 181 176 38 49 29 241
             13 219 79 174 49 251 107 38 58 194 204 25 200 136 100 184 229 121
             190 31 98 58 26 116 238 33 27 206 95 29 71 32 152 242 6 207 192 22
             79 218 59 4 45 174 171 22 79 56 99 160 191 105 55 243 137 161 221
             179 209 123 66 53 67 209 255 8 190 78)
Signature verified: T
NIL
```

[cl-ssh-keys]: https://github.com/dnaeon/cl-ssh-keys
[RFC 8017]: https://tools.ietf.org/html/rfc8017
[PROTOCOL.certkeys]: https://github.com/openssh/openssh-portable/blob/master/PROTOCOL.certkeys
[ironclad]: https://github.com/sharplispers/ironclad
