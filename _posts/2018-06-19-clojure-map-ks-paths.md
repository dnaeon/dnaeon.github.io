---
layout: post
title: Finding all key sequence paths in a nested Clojure map
tags: functional programming clojure map dfs bfs tree graph traversal
---
In [Clojure](https://clojure.org) when you need to work with maps
you get the option to choose from a number of builtin core functions such as
[get](https://clojuredocs.org/clojure.core/get),
[assoc](https://clojuredocs.org/clojure.core/assoc),
[dissoc](https://clojuredocs.org/clojure.core/dissoc),
[merge](https://clojuredocs.org/clojure.core/merge),
[select-keys](https://clojuredocs.org/clojure.core/select-keys),
[zipmap](https://clojuredocs.org/clojure.core/zipmap),
[keys](https://clojuredocs.org/clojure.core/keys),
[vals](https://clojuredocs.org/clojure.core/vals) and many others.

Getting the value associated with a given key is as easy as doing.

```clojure
user=> (get {:a 1 :b 2 :c 3} :c)
3
```

Or if you need to have a default value when a given key does not exists you can
do this instead.

```clojure
user=> (get {:a 1 :b 2 :c 3} :not-found 42)
42
```

Associating a key/value pair or removing a key from a map can be done using the
`assoc` and `dissoc` functions.

```clojure
user=> (assoc {:a 1 :b 2} :c 3)
{:a 1, :b 2, :c 3}
user=> (dissoc {:a 1 :b 2 :c 3} :c)
{:a 1, :b 2}
```

The `get`, `assoc` and `dissoc` functions work only with top-level map keys,
so if you need to retrieve a value from a nested map you need to use the
[get-in](https://clojuredocs.org/clojure.core/get-in) function instead.

So for example if we need to retrieve the innermost key's value we can do it
like this.

```clojure
user=> (get-in {:a {:b {:c 1}}} [:a :b :c])
1
```

In order to associate a key/value pair in a nested map you would use the
[assoc-in](https://clojuredocs.org/clojure.core/assoc-in) function.

Another useful function is the [keys](https://clojuredocs.org/clojure.core/keys)
function which returns a sequence of the keys in a given map.

```clojure
user=> (keys {:a 1 :b 2 :c 3})
(:a :b :c)
```

But notice what happens if we use `keys` with a nested map instead.

```clojure
user=> (keys {:a {:b {:c 1}}})
(:a)
```

When we apply `keys` function to a nested a map we only get the top-level
keys of the map and not all nested keys as well.

Your first guess here would probably be that there is a `keys-in` function
similar to how we have the `get-in`, `assoc-in` and `update-in` functions which
work with nested maps. Unfortunately these is no builtin function like that.

However, we can build one ourselves. First, we will create a sample nested map
that we can work with.

```clojure
user=> (def m {:a 1 :b 2 :c 3 :d {:e 4 :f {:g {:h 5}}}})
#'user/m
```

Given the above example nested map we want to create a function which will
return all key sequence paths. For the above example map the key sequences
that we have are.

```clojure
([:a] [:b] [:c] [:d] [:d :e] [:d :f] [:d :f :g] [:d :f :g :h])
```

This task can be solved in a number of ways. Possible solutions can be
achieved using [DFS](https://en.wikipedia.org/wiki/Depth-first_search) or
[BFS](https://en.wikipedia.org/wiki/Breadth-first_search) traversal,
another solution would be to use a [zipper](https://clojuredocs.org/clojure.zip/zipper).

In this post we will see how to create a function, which returns all
key sequence paths using depth-first search traversal.

In order to do that we will use the builtin
[tree-seq](https://clojuredocs.org/clojure.core/tree-seq) function,
which returns a lazy sequence of the nodes in a tree via a
depth-first search traversal.

```clojure
user=> (doc tree-seq)
-------------------------
clojure.core/tree-seq
([branch? children root])
  Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree.
nil
```

Now, let us create our own implementation of the `keys-in` function.

```clojure
(defn keys-in
  "Returns a sequence of all key paths in a given map using DFS walk."
  [m]
  (letfn [(children [node]
            (let [v (get-in m node)]
              (if (map? v)
                (map (fn [x] (conj node x)) (keys v))
                [])))
          (branch? [node] (-> (children node) seq boolean))]
    (->> (keys m)
         (map vector)
         (mapcat #(tree-seq branch? children %)))))
```

The `keys-in` function works in the following way. We have defined
two functions in a [letfn](https://clojuredocs.org/clojure.core/letfn)
binding form.

The `children` function gets the value of the key we are currently
exploring and if it is a map it returns a sequence of the inner map's
keys by prepending the path of currently explored node.
This gives us the full path to the keys of the nested map.

The `branch?` function simply uses `children` to return either `true` or
`false` depending on whether the node has any children or not.

Finally we get all top-level keys of the map, then transform each key to a
vector, so that we represent them using their full path and we apply each
path to the `tree-seq` function. The results of traversing each
top-level key are concatenated and returned.

And this is what we get when we use our `keys-in` function with the nested map
we have created previously.

```clojure
user=> (keys-in m)
([:a] [:b] [:c] [:d] [:d :e] [:d :f] [:d :f :g] [:d :f :g :h])
```

We can see that the result contains not just top-level key sequence paths,
but also it includes the paths of any nested maps as well.
