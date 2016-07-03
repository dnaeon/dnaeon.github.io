---
layout: post
title: Extending Lua with Go types
tags: golang lua programming
---
[Gopher Lua](https://github.com/yuin/gopher-lua) is a 
[Lua](https://www.lua.org/) 5.1 VM and compiler written in Go.

With Gopher Lua you can easily embed a scripting language into 
your Go host programs, making it suitable for things such as
handling the configuration part of your program or even use Lua as
the [DSL](https://en.wikipedia.org/wiki/Domain-specific_language)
language for your project.

Extending Gopher Lua with new types is easy. In this post we will
see how we can extend Lua with new user-defined types written in Go.

First, let's begin with a simple Go type, which later on we will
extend upon.

```go
// Person type represents a single person
type Person struct {
	Name string
}
```

It is a common practice to have a Go factory functions for each type
you define, so next thing we do is to define our factory function,
which creates a new `Person` instance.

```go
// NewPerson creates a new person with the given name
func NewPerson(name string) *Person {
	return &Person{
		Name: name,
	}
}
```

We will also implement a simple method on our type, so we can
call it later when needed.

```go
func (p *Person) Hello() string {
	return fmt.Sprintf("Hello %s!", p.Name)
}
```

Nothing Lua specific in this code yet, so let's now register our new
type, so that we can use it from Lua code as well.

Registering new Go types in Lua can be summarized as a three 
step process - first we define our Go types, then we wrap our 
types in
[lua.LUserData](https://godoc.org/github.com/yuin/gopher-lua#LUserData),
and finally we register our wrapped type in Lua using a
[lua.LState.SetGlobal](https://godoc.org/github.com/yuin/gopher-lua#LState.SetGlobal)
call.

```go
// The type name we use from Lua
const luaPersonTypeName = "person"

// LuaRegisterPersonType registers Person type in Lua
func LuaRegisterPersonType(L *lua.LState) {
	// Create a new metatable for our type
	mt := L.NewTypeMetatable(luaPersonTypeName)

	// Register the global name for our type
	L.SetGlobal(luaPersonTypeName, mt)

	// Use a constructor when creating new persons
	L.SetField(mt, "new", L.NewFunction(NewLuaPerson))
}

// NewLuaPerson creates a new Person from Lua
func NewLuaPerson(L *lua.LState) int {
	// Ensure the first argument we got is a Lua string
	name := L.CheckString(1)

	// Create the person and wrap it in lua.LUserData
	person := NewPerson(name)
	ud := L.NewUserData()
	ud.Value = person
	L.SetMetatable(ud, L.GetTypeMetatable(luaPersonTypeName))

	// Return the value to Lua
	L.Push(ud)

	// The number of values we return to Lua
	return 1
}
```

At this point we should be able to create new persons from Lua,
so let us now define our `main()` function, and run some Lua code.

```go
func main() {
	// Create a new Lua state and register our type
	L := lua.NewState()
	defer L.Close()
	LuaRegisterPersonType(L)

	// Create some persons from Lua
	code := `
	kevin = person.new("Kevin")
	bob = person.new("Bob")
	`

	if err := L.DoString(code); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
```

Running the code we've got so far should build and execute just fine,
but it doesn't allow us to do something useful with our new persons
from Lua. We will change that and implement some methods we can call
from Lua.

Our Go `Person` type has a method `Hello()`, which we will now make
available to Lua as well.

First we need to update our `LuaRegisterFunction()`, so that we also
register some methods we can call from Lua.

```go
// LuaRegisterPersonType registers Person type in Lua
func LuaRegisterPersonType(L *lua.LState) {
	// Create a new metatable for our type
	mt := L.NewTypeMetatable(luaPersonTypeName)

	// Register the global name for our type
	L.SetGlobal(luaPersonTypeName, mt)

	// Use a constructor when creating new persons
	L.SetField(mt, "new", L.NewFunction(NewLuaPerson))

	// Methods we can call from Lua
	methods := map[string]lua.LGFunction{
		"hello": luaPersonHello,
	}

	// Set __index metamethod for our type
	L.SetField(mt, "__index", L.SetFuncs(L.NewTable(), methods))
}
```

What we have done is to set the
[__index metamethod](https://www.lua.org/pil/13.4.1.html) for our
type, which in Lua is a special method which will be called if a
field we access for a type is absent. We also need to define our
`luaPersonHello` function.

```go
// luaPersonHello calls (*Person).Hello and returns the result to Lua
func luaPersonHello(L *lua.LState) int {
	// Get the person from Lua and unwrap it
	ud := L.CheckUserData(1)
	person := ud.Value.(*Person)

	result := person.Hello()

	// Return the value to Lua
	L.Push(lua.LString(result))

	// The number of values we return to Lua
	return 1
}
```

And now let's change our `main()` to actually call these methods from
Lua.

```go
func main() {
	// Create a new Lua state and register our type
	L := lua.NewState()
	defer L.Close()
	LuaRegisterPersonType(L)

	// Create some persons from Lua
	code := `
	kevin = person.new("Kevin")
	bob = person.new("Bob")

	print(kevin:hello())
	print(bob:hello())
	`

	if err := L.DoString(code); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
```

And this is how you usually interface between Go and Lua.

It is easy, although as we've seen in this example this means 
that we need to write some glue code, so we can properly
interface between Go and Lua. For a simple example as the one we've
seen here it is not a big deal, but for bigger projects and more
complex types this means we need to write a lot of glue code.

Imagine we now add another field to our Go `Person` type, e.g. another
field called `Age` which gives us the age of our `Person`. 
Now we need write the respective Lua glue code, so that we can
properly get and set the age of our `Person` from Lua. This can become
quite tedious as we continue to refactor our Go type as we introduce
new fields and methods.

Wouldn't it be nice if we can directly map Go types and methods to Lua?

Fortunately for us, there is such a Go package already that does
this for us and that is
[layeh/gopher-luar](https://github.com/layeh/gopher-luar).

[layeh/gopher-luar](https://github.com/layeh/gopher-luar) is a Go
package that provides custom type reflection for
[yuin/gopher-lua](https://github.com/yuin/gopher-lua).

What that means for us is that we can write pure Go without the
Lua glue code we've used so far and simply register our Go types and
functions using `layeh/gopher-luar`. And yes, methods of Go types
can be called as well directly from Lua. Now, isn't that just sweet?

The
[layeh/gopher-luar API](https://godoc.org/github.com/layeh/gopher-luar)
is very simple, but also very powerful. It comprises of a just a few
exported functions, but most of the time what you will use is
`luar.New` and `luar.NewType` calls in your code.

Considering the example code we've written so far, let's see what
it would look like if we've used `layeh/gopher-luar`.

Below is the full code, which contains our original `Person` type
and it's methods. What you should notice though is the lack of any
glue code when interfacing with Lua. 

```go
package main

import (
	"fmt"
	"os"

	"github.com/layeh/gopher-luar"
	"github.com/yuin/gopher-lua"
)

// The type name we use from Lua
const luaPersonTypeName = "person"

// Person type represents a single person
type Person struct {
	Name string
}

// NewPerson creates a new person with the given name
func NewPerson(name string) *Person {
	return &Person{
		Name: name,
	}
}

func (p *Person) Hello() string {
	return fmt.Sprintf("Hello %s!\n", p.Name)
}

func main() {
	// Create a new Lua state and register our type
	L := lua.NewState()
	defer L.Close()

	// Create a table with constructor for our Person type
	tbl := L.NewTable()
	tbl.RawSetH(lua.LString("new"), luar.New(L, NewPerson))
	L.SetGlobal(luaPersonTypeName, tbl)

	// Create some persons from Lua
	code := `
	kevin = person.new("Kevin")
	bob = person.new("Bob")

	print(kevin:hello())
	print(bob:hello())
	`

	if err := L.DoString(code); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
}
```

Notice that we don't have any glue code here, and yet Lua is able to
call our `(*Person).Hello` method. 

Of course such a convenience comes at the cost of using
[reflection](https://golang.org/pkg/reflect/), which might be 
something that drives people away, who are looking for more performance.

At the same time if you want to focus on developing and delivering new
features quickly and performance is not such a deal breaker for you,
then `layeh/gopher-luar` might be a good fit for you, otherwise just
stick to `yuin/gopher-lua` and make sure to write your glue code.
