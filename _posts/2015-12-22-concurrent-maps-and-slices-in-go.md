---
layout: post
title: Concurrent map and slice types in Go
tags: golang programming concurrent map slice
---
[Go](https://golang.org/) is a language well known for it's
concurrency primitives.

While Go has some really nice features making it so easy for
developers to create concurrent applications, not all of the
[types in Go](https://golang.org/ref/spec#Types) are safe for
concurrent use.

For instance two of the most commonly used types in Go -
[slice](https://golang.org/ref/spec#Slice_types) and
[map](https://golang.org/ref/spec#Map_types) - cannot be used safely
from multiple goroutines without the risk of having a
[race condition](https://en.wikipedia.org/wiki/Race_condition#Software).

On other hand using the
[basic synchronization primitives](https://golang.org/pkg/sync/) which
Go provides to us we can create our own
[thread safe](https://en.wikipedia.org/wiki/Thread_safety) types.

In this post we will see how to create our own
[slice](https://golang.org/ref/spec#Slice_types) and
[map](https://golang.org/ref/spec#Map_types) types which can be
safely shared between multiple goroutines.

In order to do that we need to ensure that access to the shared
data is protected by a [mutex](https://en.wikipedia.org/wiki/Mutual_exclusion).

What a mutex does is basically to acquire a lock when it needs to
access our concurrent slice or map. While holding the lock a
goroutine can read and/or write to the shared data protected by the
mutex.

A mutex lock can be acquired by a single goroutine at a time, and if
other goroutine needs access to the same shared data it waits
until the lock has been released by the goroutine holding the lock.

Forgetting to release the lock or improper use of it often
leads to [deadlocks](https://en.wikipedia.org/wiki/Deadlock). That is
why we need to make sure we always release the mutex lock once we are
done with it.

Let's start now by creating our concurrent types. We will start
with creating the type for concurrent slice and then make our
way to the concurrent map as well.


```go
// Slice type that can be safely shared between goroutines
type ConcurrentSlice struct {
	sync.RWMutex
	items []interface{}
}

// Concurrent slice item
type ConcurrentSliceItem struct {
	Index int
	Value interface{}
}
```

In order to protect access to the data in our slice we have
used a [sync.RWMutex](https://golang.org/pkg/sync/#RWMutex) using
composition.

The internal slice `ConcurrentSlice.items` that we use for
storing slice items is of type
[interface{}](https://golang.org/ref/spec#Interface_types) which
allows us to use this concurrent slice to store any data into it.

We have also created a `ConcurrentSliceItem` type which we will
use later on when we need to return an item from our concurrent slice
while iterating over it using the
[builtin keyword](https://golang.org/ref/spec#Keywords) `range`.

Now, let's implement a method which allows us to add new items to the
slice.

```go
// Appends an item to the concurrent slice
func (cs *ConcurrentSlice) Append(item interface{}) {
	cs.Lock()
	defer cs.Unlock()

	cs.items = append(cs.items, item)
}
```

What this method does is to acquire the mutex lock and append the item
we have passed to it and after that it releases the lock. Simple enough.

Now let's implement a method that iterates over our slice items.

```go
// Iterates over the items in the concurrent slice
// Each item is sent over a channel, so that
// we can iterate over the slice using the builin range keyword
func (cs *ConcurrentSlice) Iter() <-chan ConcurrentSliceItem {
	c := make(chan ConcurrentSliceItem)

	f := func() {
		cs.Lock()
		defer cs.Lock()
		for index, value := range cs.items {
			c <- ConcurrentSliceItem{index, value}
		}
		close(c)
	}
	go f()

	return c
}
```

The `ConcurrentSlice.Iter()` method returns a
[channel](https://golang.org/ref/spec#Channel_types) over which
`ConcurrentSliceItem` items are being sent to.

This allows us to use the builtin `range` keyword and iterate
over the items in our concurrent slice.

It is also worth noting that since our slice items are stored as
`interface{}` in order to retrieve the underlying values we
need to use [type assertions](https://golang.org/ref/spec#Type_assertions).

Let's create now the concurrent map type.

```go
// Map type that can be safely shared between
// goroutines that require read/write access to a map
type ConcurrentMap struct {
	sync.RWMutex
	items map[string]interface{}
}

// Concurrent map item
type ConcurrentMapItem struct {
	Key   string
	Value interface{}
}
```

Similar to the `ConcurrentSlice` type above we are using
composition to embed the
[sync.RWMutex](https://golang.org/pkg/sync/#RWMutex) in our
`ConcurrentMap` type.

When we retrieve items from the map we will be returning a
`ConcurrentMapItem` type which contains the map item key and value.

Let's implement a method for setting a new key in our concurrent map.

```go
// Sets a key in a concurrent map
func (cm *ConcurrentMap) Set(key string, value interface{}) {
	cm.Lock()
	defer cm.Unlock()

	cm.items[key] = value
}
```

And now we implement the method for retrieving a key from the map:

```go
// Gets a key from a concurrent map
func (cm *ConcurrentMap) Get(key string) (interface{}, bool) {
	cm.Lock()
	defer cm.Unlock()

	value, ok := cm.items[key]

	return value, ok
}
```

The result of `ConcurrentMap.Get()` method would return the
value of requested key and a boolean indicating whether the
key is present or not in the map.

And finally let's implement a method to iterate over all items in the
concurrent map.

```go
// Iterates over the items in a concurrent map
// Each item is sent over a channel, so that
// we can iterate over the map using the builtin range keyword
func (cm *ConcurrentMap) Iter() <-chan ConcurrentMapItem {
	c := make(chan ConcurrentMapItem)

	f := func() {
		cm.Lock()
		defer cm.Unlock()

		for k, v := range cm.items {
			c <- ConcurrentMapItem{k, v}
		}
		close(c)
	}
	go f()

	return c
}
```

Above method returns a channel over which `ConcurrentMapItem`
items are being sent to and we can use the builtin `range` keyword to
iterate over all items in the map.

Similar to the concurrent slices and the fact that we have used
`interface{}` to store the map values we need to use
[type assertions](https://golang.org/ref/spec#Type_assertions)
in order to retrieve the underlying value.

You can also find the code used in this post in the
[gru.utils package in Github](https://github.com/dnaeon/gru/tree/master/utils).
