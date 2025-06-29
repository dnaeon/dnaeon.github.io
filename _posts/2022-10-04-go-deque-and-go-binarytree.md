---
layout: post
title: Double-ended queue and Binary Tree implementations in Go
tags: golang go deque queue double-ended-queue algorithms tree binary-tree
---
I've been checking out some old code of mine, which I've written years
ago. Some of it dates back almost 20 years ago, and contains my first
ever programming projects, which consist of some games written in C
using the [Allegro
library](https://en.wikipedia.org/wiki/Allegro_(software_library)), a
program for locking binaries by doing a simple
[xor](https://en.wikipedia.org/wiki/Exclusive_or), and some
implementations of various algorithms I've read about back then.

This old code has been recovered from some really old CD-ROM drives,
and was quite interesting for me to go back in time and look at some
of my first code. In a future post I'll probably share more about my
early programming projects, which I managed to recover from these
CD-ROM drives.

At some point I thought about brushing up my algorithms and decided to
implement some of them from scratch. It was a fun weekend exercise for
me to go back to the basics and do a bit of linked lists, binary
trees, queues, stacks, graphs, searching algorithms, sorting, etc.
All in all, a pretty refreshing experience I would say.

Some of the code I managed to clean up and document, so I thought
about publishing it as well on Github, in case this turns out useful
to anyone else eventually.

The first library I've published is an implementation of [double-ended
queues](https://en.wikipedia.org/wiki/Double-ended_queue) in
Go. Pretty simple and straightforward. Here's how to install it.

``` shell
go get -v gopkg.in/dnaeon/go-deque.v1
```

You can find the code of the deque implementation in the
[dnaeon/go-deque](https://github.com/dnaeon/go-deque) repo.

The library supports generics, so you can use it with any type. Here's
an example of a deque, which stores integers.

``` go
package main

import (
	"fmt"
	"os"

	deque "gopkg.in/dnaeon/go-deque.v1"
)

func main() {
	d := deque.New[int]()

	// Insert a few items to work with
	// The deque looks like this: 5 4 1 2 3
	d.PushBack(1)
	d.PushBack(2)
	d.PushBack(3)
	d.PushFront(4)
	d.PushFront(5)

	fmt.Printf("deque len: %d\n", d.Length())

	// Consume all items - front to back
	for !d.IsEmpty() {
		item, err := d.PopFront()
		if err != nil {
			fmt.Println(err)
			os.Exit(1)
		}
		fmt.Printf("got item: %d\n", item)
	}

	fmt.Printf("deque len: %d\n", d.Length())
}
```

Running above code produces the following output.

``` shell
deque len: 5
got item: 5
got item: 4
got item: 1
got item: 2
got item: 3
deque len: 0
```

The other library I've published is a simple, and generic
implementation of [binary
trees](https://en.wikipedia.org/wiki/Binary_tree) in Go.

It implements various algorithms for binary trees, such as calculating
height and size of the tree, walking the tree in _in-_, _pre-_,
_post-_ and _level-order_, and comes with a number of test predicates
to check whether a tree is _Perfect_, _Complete_, _Binary Search
Tree_, etc.

You can also generate a [Dot
representation](https://en.wikipedia.org/wiki/DOT_(graph_description_language))
of your tree and render it using [graphviz](https://graphviz.org/).

You can find the full code in the
[dnaeon/go-binarytree](https://github.com/dnaeon/go-binarytree) repo.
Here's an example usage of the library.

``` go
package main

import (
	"fmt"

	binarytree "gopkg.in/dnaeon/go-binarytree.v1"
)

func main() {
	root := binarytree.NewNode(10)
	five := root.InsertLeft(5)
	twenty := root.InsertRight(20)
	five.InsertLeft(9)
	five.InsertRight(18)
	twenty.InsertLeft(3)
	twenty.InsertRight(7)

	fmt.Printf("height of tree: %d\n", root.Height())
	fmt.Printf("size of the tree: %d\n", root.Size())
	fmt.Printf("tree is balanced: %t\n", root.IsBalancedTree())
	fmt.Printf("tree is complete: %t\n", root.IsCompleteTree())
	fmt.Printf("tree is perfect: %t\n", root.IsPerfectTree())

	// Function to be called while walking the tree, which simply
	// prints the values of each visited node
	walkFunc := func(n *binarytree.Node[int]) error {
		fmt.Printf("%d ", n.Value)
		return nil
	}

	fmt.Printf("in-order values: ")
	root.WalkInOrder(walkFunc)
	fmt.Println()

	fmt.Printf("pre-order values: ")
	root.WalkPreOrder(walkFunc)
	fmt.Println()

	fmt.Printf("post-orer values: ")
	root.WalkPostOrder(walkFunc)
	fmt.Println()

	fmt.Printf("level-order values: ")
	root.WalkLevelOrder(walkFunc)
	fmt.Println()
}
```

Running above code produces the following output.

``` shell
height of tree: 2
size of the tree: 7
tree is balanced: true
tree is complete: true
tree is perfect: true
in-order values: 9 5 18 10 3 20 7 
pre-order values: 10 5 9 18 20 3 7 
post-orer values: 9 18 5 3 7 20 10 
level-order values: 10 5 20 9 18 3 7 
```

We can generate the Dot representation of a tree using this code.

``` go
package main

import (
	"os"

	"gopkg.in/dnaeon/go-binarytree.v1"
)

func main() {
	root := binarytree.NewNode(10)
	five := root.InsertLeft(5)
	twenty := root.InsertRight(20)
	five.InsertLeft(9)
	five.InsertRight(18)
	twenty.InsertLeft(3)
	twenty.InsertRight(7)

	root.WriteDot(os.Stdout)
}
```

Running above example produces an output similar to this one.

``` shell
digraph {
        node [color=lightblue fillcolor=lightblue fontcolor=black shape=record style="filled, rounded"]
        824634441792 [label="<l>|<v> 10|<r>" ]
        824634441792:l -> 824634441856:v
        824634441792:r -> 824634441920:v
        824634441856 [label="<l>|<v> 5|<r>" ]
        824634441856:l -> 824634441984:v
        824634441856:r -> 824634442048:v
        824634441984 [label="<l>|<v> 9|<r>" ]
        824634442048 [label="<l>|<v> 18|<r>" ]
        824634441920 [label="<l>|<v> 20|<r>" ]
        824634441920:l -> 824634442112:v
        824634441920:r -> 824634442176:v
        824634442112 [label="<l>|<v> 3|<r>" ]
        824634442176 [label="<l>|<v> 7|<r>" ]
}
```

Finally, we can render the binarytree using _graphviz_:

``` shell
dot -Tsvg /path/to/file.dot -o /tmp/to/file.svg
```

And here's how our tree looks like.

[![]({{ site.baseurl }}/images/go-binarytree.svg)]({{ site.baseurl }}/images/go-binarytree.svg){:.glightbox}
