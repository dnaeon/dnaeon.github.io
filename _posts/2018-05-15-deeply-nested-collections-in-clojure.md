---
layout: post
title: Transforming flat sequences to deeply nested collections in Clojure
tags: functional programming clojure dag graph tree zipper dependency graph
---
One of the API services that I have developed recently for internal
project returns a flat sequence of items, where each item in the sequence
may or may not be related to other items with a parent-child relationship.

The API service itself is written in [Clojure](https://clojure.org/) where
endpoints are exposed as [Ring](https://github.com/ring-clojure/ring) handlers.

Below is an example sequence of items as returned by the API.

```clojure
({:id 0, :parent nil, :name "item-0"}
 {:id 1, :parent 0, :name "item-1"}
 {:id 2, :parent 1, :name "item-2"}
 {:id 3, :parent 2, :name "item-3"}
 {:id 4, :parent 3, :name "item-4"})
```

Above example data represents a hierarchy of items.

These items could represent for example a set of directories
being the parents and their children being files or other directories, or
it can be anything else that needs to be represented as a hierarchy of items.

While working with the data returned by the API service I was thinking
about the following - can we transform this flat sequence of data into a
deeply nested collection?

So, as an exercise I've decided to look into this one and see how we can
solve this efficiently in Clojure.

In this post we are going to look into the following problem - how can we
transform the above example sequence of items into a deeply nested
collection which has the following form.

```clojure
({:id 0,
  :parent nil,
  :name "item-0",
  :children
  ({:id 1,
    :parent 0,
    :name "item-1",
    :children
    ({:id 2,
      :parent 1,
      :name "item-2",
      :children
      ({:id 3,
        :parent 2,
        :name "item-3",
        :children ({:id 4, :parent 3, :name "item-4"})})})})})
```

Using above data as a reference we can see that some items have a `parent`,
while others do not. These items without a `parent` we call them the `root` nodes.

The idea of above data set is to represent a tree where relationship between
items is based on the `parent` field.

The actual API service has more than a single root node in contrast to the example
sequence of items shown in this post, but in order to keep things simple and focused
we will stick to a collection of items with only a single root node.

First, we will define a few helper functions to help us create sequence of items
easily.

```clojure
(defn make-items [n]
  (concat [{:id 0 :parent nil :name "item-0"}]
        (for [x (range 1 n)] {:id x :parent (dec x) :name (format "item-%d" x)})))
```

The `make-items` functions creates a sequence of `n` items where each item
is a child of the previous one.

Our single root item can be identified by the `:id 0` key, or by
filtering the collection for items with `:parent` being `nil`.

Another helper function that we will use is shown below. The `root-node`
function will return the first root node in a given collection of items.

```clojure
(defn root-node [coll]
  (-> (filter #(= (:parent %) nil) coll) first))
```

Finally, let's create a function that returns the sequence of children for a given
item.

```clojure
(defn find-children
  "Given a root node and a collection of items returns the children of the root"
  [root coll]
  (filter #(= (:id root) (:parent %)) coll))
```

We can now create a few items to play with by using the `make-items` function
that we have defined previously.

```clojure
user=> (require '[clojure.pprint :refer [pprint]])
nil
user=> (def items (make-items 5))
#'user/items
user=> (pprint items)
({:id 0, :parent nil, :name "item-0"}
 {:id 1, :parent 0, :name "item-1"}
 {:id 2, :parent 1, :name "item-2"}
 {:id 3, :parent 2, :name "item-3"}
 {:id 4, :parent 3, :name "item-4"})
nil
```

Having some data to play with let's see how we can tranform this sequence into a
deeply nested collection. The most natural way that comes to mind to solve this
is to go for a [recursive](https://en.wikipedia.org/wiki/Recursion#Recursive_humor)
algorithm.

And here is the first version of a function that we can use to tranform our
flat sequence into a deeply nested collection.

```clojure
(defn flat-seq->deeply-nested-v1
  "Transforms a flat sequence to a deeply nested collection - version 1"
  [root coll]
  (if-let [children (-> (find-children root coll) seq)]
    (map #(assoc root :children (flat-seq->deeply-nested-v1 % coll)) children)
    (list root)))
```

Let's try it out.

```clojure
user=> (def items (make-items 5))
#'user/items
user=> (def root (root-node items))
#'user/root
user=> (pprint (flat-seq->deeply-nested-v1 root items))
({:id 0,
  :parent nil,
  :name "item-0",
  :children
  ({:id 1,
    :parent 0,
    :name "item-1",
    :children
    ({:id 2,
      :parent 1,
      :name "item-2",
      :children
      ({:id 3,
        :parent 2,
        :name "item-3",
        :children {:id 4, :parent 3, :name "item-4"}})})})})
nil
```

Seems to produce the desired output.

A slightly modified version of the above code, but this time using
`reduce` instead of `map` can be found below as well.

```clojure
(defn flat-seq->deeply-nested-v2
  "Transforms a flat sequence to a deeply nested collection - version 2"
  [root coll]
  (if-let [children (-> (find-children root coll) seq)]
    (reduce (fn [result x] (conj result (assoc root :children (flat-seq->deeply-nested-v2 x coll)))) [] children)
    (vector root)))
```

Another nice solution posted by a fellow Clojurian on Reddit, which uses a
zipper to solve this can be seen below.

```clojure
(require [clojure.zip :as zip])

(defn flat-seq->deeply-nested-zipper [root coll]
  (let [by-parent (group-by :parent coll)]
    (loop [z (zip/zipper some? #(by-parent (:id %)) #(assoc %1 :children %2) (first (by-parent nil)))]
      (if (zip/end? z)
        (zip/root z)
        (recur (zip/next (zip/edit z identity)))))))
```

 Make sure to check the
[zipper examples](https://clojuredocs.org/clojure.zip/zipper) as well as the
[Functional Pearl - The Zipper](https://www.st.cs.uni-saarland.de/edu/seminare/2005/advanced-fp/docs/huet-zipper.pdf)
document on which Clojure zippers are based on.

Before we wrap things up, I wanted to mention a few more things here as well.

While these solutions do in fact meet our initial requirements they have it's
faults.

The way we are building up the nested collection is based on recursion,
and Clojure does not do automatically
[tail-call optimizations](https://en.wikipedia.org/wiki/Tail_call) for us.

That means that our recursive solution will be good enough for small data sets, but
not for really larger ones, as each recursive calls would consume stack space
and eventually will overflow the stack.

The provided solutions so far work well with a small set of items, but they fail
when we use them with a larger sequence of items. Let's see an example with a
larger sequence of items, for example a 100_000 items sequence.

```clojure
user=> (def items (make-items 100000))
#'user/items
user=> (def root (root-node items))
#'user/root
user=> (flat-seq->deeply-nested-zipper root items)
StackOverflowError   clojure.lang.PersistentHashMap$BitmapIndexedNode.index (PersistentHashMap.java:677)
```

We've managed to overflow the stack. Above error tells us something - sooner or later,
depending on the size of the data we are working on we would overflow the stack when using
plain old recursion since our recursive functions consume stack space.

It should also be noted that the initial representation of the deeply
nested collection is probably not the most appropriate one, as in reallity we
would probably never need more than a few levels of nesting.

Slightly better results can be achieved with really large sequences of items
if we provide larger heap size to the JVM.

An alternative approach to this problem could be to use
[trampoline](https://clojuredocs.org/clojure.core/trampoline), but I haven't gotten to
play with it yet.

It would also be interesting to see a solution on
[Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language)), which is another
dialect of Lisp which has proper tail recursion.

Another thing to know when dealing with data with such relationship between
items is to consider using a [dependency graph](https://en.wikipedia.org/wiki/Dependency_graph).
With a dependency graph we can easily find the root nodes and all immediate or transient
children of a node as well. The dependency graph would be more appropriate in such
situations and also would help us find circular dependencies in our data if we have such.

Finally, make sure to check this
[reddit post](https://www.reddit.com/r/Clojure/comments/8hvilo/making_a_deeply_nested_map_with_parentchild/),
which talks about the same problem and contains lots of useful information about it.
