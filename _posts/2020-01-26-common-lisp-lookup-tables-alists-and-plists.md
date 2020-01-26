---
layout: post
title: Lookup tables in Common Lisp - alists and plists
tags: lisp programming
---
Common Lisp provides a variety of data structures for mapping key/value pairs.

Two of the basic lookup tables supported by Common Lisp and
implemented in terms of [cons
cells](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_c.htm#cons)
are the [association
lists](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_a.htm#association_list)
(also known as *alists*) and [property
lists](http://clhs.lisp.se/Body/26_glo_p.htm#property_list) (also
known as *plists*). In addition to *alists* and *plists* we can also
use [hash tables](http://cl-cookbook.sourceforge.net/hashes.html).

Using *alists* and *plists* are good enough for small number of
key/value pairs and can perform better than hash tables in some
situations, but in general if you need to work with a large enough
lookup table it's better to stick with hash tables instead.

Using *alists* and *plists* with large number of key/value pairs
can be slow, mainly because testing for presence of a key is
determined by the size of the items stored in them.

An *alist* is made up of *cons cells*, where each key/value pair is
represented as a [dotted
pair](http://www.lispworks.com/documentation/HyperSpec/Body/26_glo_d.htm#dotted_pair),
unless the value associated with the key is a list.

Here's an example of an *alist* with three key/value pairs.

``` common-lisp
CL-USER> (defparameter *alist* (list (cons 'a 1)
                                     (cons 'b 2)
                                     (cons 'c 3)))
*ALIST*
```

We can retrieve a key/value pair from an alist using the [ASSOC,
ASSOC-IF and ASSOC-IF-NOT](http://clhs.lisp.se/Body/f_assocc.htm)
functions, e.g.

``` common-lisp
CL-USER> (assoc 'a *alist*)
(A . 1)
```

If we are interested in the value part of the mapping we just need to
pass the result of `ASSOC` to `CDR` in order to extract it, e.g.

``` common-lisp
CL-USER> (cdr (assoc 'a *alist*))
1
```

Association lists also support reverse querying, which allows us to
find the key/value mapping based on the value of the pair. For that we
need to use the [RASSOC, RASSOC-IF and
RASSOC-IF-NOT](http://www.lispworks.com/documentation/HyperSpec/Body/f_rassoc.htm)
family of functions, e.g.

``` common-lisp
CL-USER> (rassoc 1 *alist*)
(A . 1)
```

Pass the result to `CAR`, if you are interested in only the key part
of the mapping, e.g.

``` common-lisp
CL-USER> (car (rassoc 1 *alist*))
A
```

Adding new mappings can be done either by
[PUSH](http://clhs.lisp.se/Body/m_push.htm)ing a new pair or simply
[CONS](http://www.lispworks.com/documentation/HyperSpec/Body/f_cons.htm)ing
onto it.

``` common-lisp
CL-USER> (cons '(d . 4) *alist*)
((D . 4) (A . 1) (B . 2) (C . 3))

CL-USER> *alist*
((A . 1) (B . 2) (C . 3))
```

Note that `CONS`ing onto the alist does not modify it, which basically
returns a new list instead. If you need to persist the new mapping
in the alist you can `SETF` the original place with the value
returned by above expression.

A convinience function when working with alists is
[ACONS](http://www.lispworks.com/documentation/HyperSpec/Body/f_acons.htm),
which creates a new pair and CONSes it onto the given place.

Previous example would be better written like this instead.

``` common-lisp
CL-USER> (acons 'd 4 *alist*)
((D . 4) (A . 1) (B . 2) (C . 3))
```

Using `PUSH` on other hand will modify the original alist, e.g.

``` common-lisp
CL-USER> (push (cons 'D 4) *alist*)
((D . 4) (A . 1) (B . 2) (C . 3))

CL-USER> *alist*
((D . 4) (A . 1) (B . 2) (C . 3))
```

Note that when using alists where the keys are strings we need to
be aware to use the correct test function such as `STRING=`.

Consider the following example.

``` common-lisp
CL-USER> (assoc "foo" '(("foo" . 1) ("bar" . 2) ("qux" . 3)))
NIL
```

The result is `NIL` because `ASSOC` by default will use `EQL`
as test predicate, and two strings with same values are not
neccessary `EQL`. In order to fix above example we would
use this expression instead.

``` common-lisp
CL-USER> (assoc "foo" '(("foo" . 1) ("bar" . 2) ("qux" . 3)) :test #'string=)
("foo" . 1)
```

Another useful function for creating alists is the
[PAIRLIS](http://www.lispworks.com/documentation/HyperSpec/Body/f_pairli.htm),
which given an even number of sequences creates an alist with the
respective keys and values, e.g.

``` common-lisp
CL-USER> (pairlis '(a b c) '(1 2 3))
((C . 3) (B . 2) (A . 1))
```

In order to remove a pair from an alist we can use the [REMOVE,
REMOVE-IF, REMOVE-IF-NOT, DELETE, DELETE-IF,
DELETE-IF-NOT](http://clhs.lisp.se/Body/f_rm_rm.htm) family of
functions, e.g.

``` common-lisp
CL-USER> (remove 'a *alist* :key #'car)
((B . 2) (C . 3))
```

In above example we are specifying that `CAR` function should be used
to extract the key from the pairs when searching for the key.

An interesting thing about alists is that when searching for a given
key using `ASSOC` or `RASSOC` the first matching pair will be
returned, which in some situations might be useful, e.g.
we can *shadow* a previous pair by having another one
with different value before it.

``` common-lisp
CL-USER> (assoc 'A '((A . 100) (A . 1) (B . 2) (C . 3)))
(A . 100)
```

If you are using using
[CL-JSON](https://common-lisp.net/project/cl-json/cl-json.html) for
parsing and writing JSON objects the result from parsing a JSON object
is an alist. Consider the following example JSON object.

``` json
{
  "a": 1,
  "b": 2,
  "c": 3,
  "foo": {
    "bar": {
      "baz": {
        "qux": 42
      }
    }
  }
}
```

We can parse this JSON object using `cl-json:decode-from-source` function, e.g.

``` common-lisp
CL-USER> (ql:quickload :cl-json)
To load "cl-json":
  Load 1 ASDF system:
    cl-json
; Loading "cl-json"

(:CL-JSON)
```

Once we've loaded the `cl-json` system we can parse the object.

``` common-lisp
CL-USER> (cl-json:decode-json-from-source #P"/path/to/some/file.json")
((:A . 1) (:B . 2) (:C . 3) (:FOO (:BAR (:BAZ (:QUX . 42)))))
```

The result is an alist. Also note that the JSON object contains nested
hash tables. Suppose that we want to get the value associated with the `:QUX`
key from above JSON. We can do that using the `ASSOC` functions like this.

First, lets save the parsed object so we can refer to it.

``` common-lisp
CL-USER> (defparameter *json* (cl-json:decode-json-from-source #P"/tmp/test.json"))
*JSON*
```

One way to extract the `:foo :bar :baz :qux` key would be to use `ASSOC` along with
`CDR`, e.g.

``` common-lisp
CL-USER> (cdr (assoc :qux
                     (cdr (assoc :baz
                                 (cdr (assoc :bar
                                             (cdr (assoc :foo *json*))))))))
42
```

But this looks really ugly to be honest. The
[arrows](https://quickref.common-lisp.net/arrows.html) library can
help a bit here instead.

``` common-lisp
CL-USER> (ql:quickload :arrows)
To load "arrows":
  Load 1 ASDF system:
    arrows
; Loading "arrows"

(:ARROWS)
CL-USER> (use-package :arrows)
T
CL-USER> (->> *json*
              (assoc :foo)
              cdr
              (assoc :bar)
              cdr
              (assoc :baz)
              cdr
              (assoc :qux)
              cdr)
42
```

But still extracting the value of a nested key does not look as nice
as I'd like to. Instead, we can write our own function to retrieve
the value of a nested key in an alist. Here's one possible
implementation.

``` common-lisp
(defun assoc-path (alist path &key (key #'identity) (test #'eql) (default nil))
  "Retrieve the value in the given ALIST represented by the given PATH"
  (or (reduce (lambda (alist k)
                (cdr (assoc k alist :key key :test test)))
              path
              :initial-value alist)
      default))
```

Retrieving the nested value is now an easy thing, e.g.

``` common-lisp
CL-USER> (assoc-path *json* '(:foo :bar :baz :qux))
42
```

This looks much better. The `ASSOC-PATH` function we've implemented
takes the usual `:test` and `:key` keyword parameters just like
`ASSOC` does, and also adds support for specifying a default value via
the `:default` keyword parameter, if no such path exists.

Property lists are another form of lookup tables, which are made up of
cons cells. They are a bit more primitive than alists, but they do
have their purpose as well. A property list is a regular list with
even number of items representing the keys and values of the
mappings. This is an example of a plist.

``` common-lisp
CL-USER> (list :a 1 :b 2 :c 3)
(:A 1 :B 2 :C 3)
```

In order to retrieve a value associated with a given key in a plist we
can use [GETF](http://clhs.lisp.se/Body/f_getf.htm).

``` common-lisp
CL-USER> (defparameter *plist* (list :a 1 :b 2 :c 3))
*PLIST*
CL-USER> (getf *plist* :a)
1
```

You can also supply a default value to be returned, if the key is not
fonud in the plist, e.g.

``` common-lisp
CL-USER> (getf *plist* :x 'default-value)
DEFAULT-VALUE
```

Adding a new mapping to a plist is done via `SETF`.

``` common-lisp
CL-USER> (setf (getf *plist* :new-key) 'new-value)
NEW-VALUE
CL-USER> *plist*
(:NEW-KEY NEW-VALUE :A 1 :B 2 :C 3)
```

In order to remove a key/value pair from a plist we need to use the
[REMF](http://www.lispworks.com/documentation/HyperSpec/Body/m_remf.htm)
macro.

``` common-lisp
CL-USER> (remf *plist* :new-key)
T
CL-USER> *plist*
(:A 1 :B 2 :C 3)
```

If the key exists and is removed the result from `REMF` will be true,
and `NIL` otherwise.

An interesting thing about plists is how they relate to symbols. Each
symbol in Common Lisp contains a plist associated with it, which can
be used to attach metadata to a symbol. In order to access the plist
associated with a symbol we can use
[SYMBOL-PLIST](http://www.lispworks.com/documentation/HyperSpec/Body/f_symb_4.htm).

``` common-lisp
CL-USER> (defparameter *my-list* (list 1 2 3))
*MY-LIST*
CL-USER> (symbol-plist '*my-list*)
NIL
```

Adding a key/value pair to a symbol's plist can be done in the
following way.

``` common-lisp
CL-USER> (setf (getf (symbol-plist '*my-list*) :some-key) :some-value)
:SOME-VALUE
CL-USER> (symbol-plist '*my-list*)
(:SOME-KEY :SOME-VALUE)
```

A convenient function we can use when looking up a symbol's plist is
[GET](http://clhs.lisp.se/Body/f_get.htm). Above example can be
shortened a bit like this.

``` common-lisp
CL-USER> (setf (get '*my-list* :some-key) :some-value)
:SOME-VALUE
CL-USER> (get '*my-list* :some-key)
:SOME-VALUE
```

For removing mappings from a plist we can still use `REMF`, or the more convenient
[REMPROP](http://clhs.lisp.se/Body/f_rempro.htm) function.

``` common-lisp
CL-USER> (remprop '*my-list* :some-key)
(:SOME-KEY :SOME-VALUE)
CL-USER> (symbol-plist '*my-list*)
NIL
```

For more information about lookup tables and other uses of cons cells,
make sure to check the [Practical Common Lisp
book](http://www.gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html).
