---
layout: post
title: Membership test in Go
tags: golang programming slice membership
---
The [membership test operators in Python](https://docs.python.org/3/reference/expressions.html#membership-test-operations)
make things easy when we need to test whether a given item is
contained within a given container or mapping object instance.

Using the membership operators is easy and also seems natural.

```python
>>> if x in some_list:
>>>		print('yay!')
```

Having done quite a lot of Python coding I've always missed this
kind of tests when I do similar things, but in Go.

In Go there is no such an operator like the membership test
operators in Python. However, implementing such a behaviour in Go is
easy.

It is also worth mentioning that in Python the membership test
operator is not doing some kind of magic in order to determine
whether an item is contained within a given container or mapping
instance.

In fact, Python does iterate over the given collection and tries to
determine whether the item is a member of that collection.

So, that is what we are about to implement in Go as well.

First, let's create our Go collection type.

```go
// List type represents a slice of strings
type List []string
```

The `List` type is nothing more than a slice of strings, and you
might be wondering why we need to create a completely new type,
instead of just using `[]string`.

The reason why we create a new type is because in Go we cannot
implement new methods on the core language types.

If we tried that the compiler would complain and return an error.

In order to overcome this, we need to create an alias type,
and this is what we did in the above snippet.

It is also a good practice to provide a factory function for each
exported type in Go, so that's what we do next.

```go
// NewList creates a new list with the given items
func NewList(s ...string) List {
	l := make(List, len(s))
	for _, v := range s {
		l = append(l, v)
	}

	return l
}
```

Next thing we do is to implement the `Contains` method on our
`List` type, which will perform the actual membership test.

```go
// Contains returns a boolean indicating whether the list
// contains the given string.
func (l List) Contains(x string) bool {
	for _, v := range l {
		if v == x {
			return true
		}
	}

	return false
}
```

And here's our `main` function, which uses the code we've written so
far.

```go
package main

import "fmt"

func main() {
	list := NewList("foo", "bar", "qux")

	fmt.Printf("list contains 'foo': %t\n", list.Contains("foo"))
}
```

Checking for a membership now happens by using a method on the
`List` type, but here's something else that might be useful to add
to our code.

```go
// String type represents a string
type String struct {
	str string
}

// NewString creates a new string
func NewString(s string) String {
	return String{
		str: s,
	}
}

// IsInList returns a boolean indicating whether the string is
// contained within a given list
func (s String) IsInList(l List) bool {
	return l.Contains(s.str)
}
```

By introducing the `String` type we can now test for membership
this way as well.

```go
package main

import "fmt"

func main() {
	s := NewString("foo")
	fmt.Printf("'foo' is a member of the list: %t\n", s.IsInList(list))
}
```

It is also worth mentioning that this code is probably okay for
small sets of data that we perform the membership test against.

For larger sets it is recommended that we use binary search on the
sorted slice of strings.

For more information about binary search, make sure to check
[sort.Search](https://golang.org/pkg/sort/#Search).

You can also find the code used in this post in the
[utils package](https://github.com/dnaeon/gru/tree/master/utils) of the
[Gru project](https://github.com/dnaeon/gru).
