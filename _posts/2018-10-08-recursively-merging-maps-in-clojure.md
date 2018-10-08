---
layout: post
title: Recursively merging maps in Clojure
tags: functional programming clojure map deep merge recursive merge
---
In [Clojure](https://clojure.org) if we need to combine maps
together we can use the [merge](https://clojuredocs.org/clojure.core/merge)
function.

```clojure
user> (merge {:foo "foo"} {:bar "foo"})
{:foo "foo", :bar "foo"}
```

The `merge` function also *overrides* values when we have multiple maps
defining the same key. The last map to define the duplicate key is the one,
which overrides the value.

```clojure
user> (merge {:foo "foo"} {:foo "fubar"})
{:foo "fubar"}
```

The `merge` function however does not combine the maps in recursive way.

```clojure
user> (merge {:foo "foo" :bar {:baz "baz"}} {:foo "another-foo" :bar {:qux "qux"}})
{:foo "another-foo", :bar {:qux "qux"}}
```

Note how the `:baz` key is not part of the result, even though we are not
*overriding* it anywhere in the second map.

If you search for recursively merging maps in Clojure you will find that
there was an issue reported for this one already with a proposed
patch, which unfortunately was not accepted by Rich Hickey. You can read
more about the proposed feature in [CLJ-1468](https://dev.clojure.org/jira/browse/CLJ-1468).

Not having this feature as part of the core language results in people often
copying the same solution over and over again, since at some point
someone needs a merge function, which can recursively traverse maps and
combine them.

I was one of those people, and in this post I'm going to show you yet
another way to merge maps in Clojure recursively. Hopefully one day
we will have this feature as part of the core language instead.

```clojure
(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (last xs)))]
    (reduce m maps)))
```

A few words about the `deep-merge` function. Just like the core `merge` function,
the `deep-merge` function we've defined will prefer the last value when multiple maps
define the same key.

Another important thing to note here is that we do not traverse into
[records](https://clojuredocs.org/clojure.core/defrecord). This is because
records in Clojure are just plain old maps under the hood, and we want to
treat them as key values during the recursive merge, and not as actual
maps.

Time to test things out.

```clojure
user> (deep-merge {:foo "foo" :bar {:baz "baz"}} {:foo "another-foo" :bar {:qux "qux"}})
{:foo "another-foo", :bar {:baz "baz", :qux "qux"}}
```

This time things work as expected.

Overriding record values works fine too.

```clojure
user> (defrecord Foo [x])
user.Foo
user> (defrecord Bar [x])
user.Bar
user> (def x {:foo (->Foo :this-is-foo) :bar (->Bar :this-is-bar) :qux "and-this-is-qux"})
#'user/x
user> (def y {:foo (->Foo :this-is-another-foo) :bar (->Bar :this-is-another-bar)})
#'user/y
user> (deep-merge x y)
{:foo #user.Foo{:x :this-is-another-foo}, :bar #user.Bar{:x :this-is-another-bar}, :qux "and-this-is-qux"}
user>
```

The `deep-merge` function also behaves just like the core `merge` function, e.g.

```clojure
user> (deep-merge)
nil
user> (deep-merge nil)
nil
user> (deep-merge {} nil)
{}
```

The `deep-merge` function we've created prefers the last value when combining maps
recursively, but what if we wanted to use the first value instead?

Similar to the core `merge-with` function we can define our own
`deep-merge-with` function as well, which will give us more control over the
merge behaviour.

```clojure
(defn deep-merge-with
  "Recursively merges maps. Applies function f when we have duplicate keys."
  [f & maps]
  (letfn [(m [& xs]
            (if (some #(and (map? %) (not (record? %))) xs)
              (apply merge-with m xs)
              (apply f xs)))]
    (reduce m maps)))
```

And here are a few examples using `deep-merge-with`.

```clojure
user> (deep-merge-with first {:foo "foo" :bar {:baz "baz"}} {:foo "another-foo" :bar {:qux "qux"}})
{:foo "foo", :bar {:baz "baz", :qux "qux"}}
user> (deep-merge-with last {:foo "foo" :bar {:baz "baz"}} {:foo "another-foo" :bar {:qux "qux"}})
{:foo "another-foo", :bar {:baz "baz", :qux "qux"}}
```

Things you would usually use `merge-with` work as expected with `deep-merge-with`.

```clojure
user> (def x {:a 2
              :b {:c 3 :d {:e 90}}})
#'user/x
user> (def y {:a 40
              :b {:c 5}
              :x 99})
#'user/y
user> (deep-merge-with + x y)
{:a 42, :b {:c 8, :d {:e 90}}, :x 99}
```
