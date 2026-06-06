---
layout: post
title: Discoverying and navigating through Go linkname compiler directives
tags: compiler golang go linkname
---

Go supports a number of
[directives](https://pkg.go.dev/cmd/compile#hdr-Compiler_Directives) which can
influence the behaviour of the compiler.

These compiler directives are in the form of _magic comments_, probably because
of some historical reasons, combined with the fact that the Go developers don't
really like extending further the syntax of the language, in order to keep it
small and simple.

One interesting compiler directive is [linkname](https://pkg.go.dev/cmd/compile#hdr-Linkname_Directive),
which allows us to _declare_ a symbol in one package, which happens to be an
alias for a symbol in another package. The symbol to which we link can even be an
unexported symbol, allowing us to break the encapsulation rules as a whole and
make a package-internal symbol exported via another symbol in a different package.

The following example shows how we can use the `linkname` directive in order to
expose an unexported symbol from one package via another package.

Lets create a new Go module and the directories for our example packages.

```shell
mkdir linkname-example
cd linkname-example
go mod init linkname-example
mkdir -p pkg/{foo,bar}
```

Our example project layout looks like this.

```shell
[linkname-example] > tree .
.
├── go.mod
└── pkg
    ├── bar
    └── foo

4 directories, 1 file
```

A few words about the project structure -- the `foo` package will contain an
unexported symbol, which _usually_ we cannot access from another
package. However, we will expose this unexported symbol from `foo` package via
the `bar` package using the `linkname` compiler directive.

Lets start with the `foo` package first. This is what our `pkg/foo/foo.go` file
looks like.

```go
// pkg/foo/foo.go

package foo

import (
        "fmt"
        _ "unsafe"
)

// Use go:linkname directive here to make things clearer to the reader
// that this particular symbol will be referenced by other symbols.
//
// Using go:linkname violates the encapsulation of symbols, and allows
// other packages to refer even to un-exported symbols.
//
// For that reason importing "unsafe" is a requirement.

//go:linkname foo
func foo() {
        fmt.Println("foo() was called")
}
```

In the code above we can see a single `foo` function, which is unexported, and
it also contains the `linkname` directive. This is the function to which we will
be creating an alias.

And this is what our `pkg/bar/bar.go` file looks like, from which we will expose
the `foo.foo` symbol.

```go
// pkg/bar/bar.go

package bar

import (
        "fmt"
        _ "linkname-example/pkg/foo" // We need to import `foo' here as well
        _ "unsafe"                   // Importing unsafe is required
)

// f() here is an alias for [foo.foo]
//
//go:linkname f linkname-example/pkg/foo.foo
func f()

func Bar() {
        fmt.Println("Bar() was called")
        f()
}
```

A few words about the code above. We have one exported symbol - the `Bar`
function, which simply prints `Bar() was called`, and then it calls the
unexported `f()` function from the same package.

The `f()` function does not look like a regular Go function, since it lacks a
function body. It is a function declaration only. It also uses the `linkname`
directive in order to _link_ the `bar.f` symbol to the `foo.foo` symbol.

Using the `linkname` directive also requires that we import the `unsafe`
package, since Go requires it, as documented in the code above.

Additionally, we had to import the `linkname-example/pkg/foo` package, so that
it is part of the import graph, in order to find the respective `pkg/foo.foo`
symbol. If `pkg/foo` was already part of the imports because of a transitive
dependency, we would not have to explicitly import it, but that is not the case
in this example.

Finally we can wire things up in our `main.go` file and simply call the exported
`bar.Bar()` function.

```go
// main.go

package main

import "linkname-example/pkg/bar"

func main() {
        bar.Bar()
}
```

Running this program prints the following, and we can see that even though
`foo.foo` was unexported we have successfully called it via the `bar.Bar()`
function and the symbol alias `bar.f`.

```shell
[linkname-example] > go run main.go
Bar() was called
foo() was called
```

This is not a new or unconventional thing at all. The Go standard library uses
this all over the place in order to share re-usable code between internal-only
packages without having to make such symbols exported in the first place.

Another reason for the stdlib to use `linkname` is to avoid circular import
dependencies. For example the `testing/synctest` package imports the `testing`
package, since it needs `testing.T` because it is used by the
`testing/synctest.Test` function.

Internally, `testing/synctest.Test` calls
`testing/synctest.testingSynctestTest`, which is a _pull_ linkname. The _push_
linkname symbol is `testing.testingSynctestTest`.

- `testing/synctest` imports `testing` (because it needs `testing.T`)
- `testing/synctest.Test` calls `testing/synctest.testingSynctestTest` (a linkname _pull_ symbol)
- `testing.testingSynctestTest` provides the function body and is the linkname _push_ symbol

Now, we could have moved the function body of `testing.testingSynctestTest` into
`testing/synctest.testingSynctestTest`, but the thing is that
`testing.testingSynctestTest` needs to work with lots of other internal-only
symbols in `testing`, e.g. `testing.tRunner` function, the `testing.common`
struct, etc. We either have to `linkname` all of these, or make them exported.

My best guess here is that the idea behind having `testing.testingSynctestTest`
as a linkname push symbol is to keep the changes to a minimum when introducing
the `testing/synctest` package. In the end we have one `linkname` push symbol in
`testing` package, which is better than exposing everything else in `testing`
via additional linknames, or making them exported.

The tradeoffs however are that these magic comments don't really have a concrete
AST representation, unlike in other languages like Lisp or Rust. If we inspect
the AST tree we would find these directives as plain _Comment_ nodes in Go,
instead of something like _Directive_.

Fortunately, `gopls` already comes with
[support for parsing linkname directives](https://github.com/golang/go/issues/57312),
however support for is partial. For example, `gopls` can _jump_ to a referred
symbol when using the 2-arg form of `linkname`. However, it does not support the
_find references_ action, which allows us to view symbols that link to a given
symbol, which uses the 1-arg form.

It is also worth noting here that recent Go versions (> 1.23.x) come with
tightened up restrictions about using `linkname` directive when linking to a
symbol in the stdlib. See the following links for additional information.

- https://go.dev/doc/go1.23#linker
- https://github.com/golang/go/issues/67401

In order to fill that gap and be able to navigate through all the `linkname`
symbols in the stdlib I've built a tool for this purpose only.

The tool can be found in the
[dnaeon/golinkname](https://github.com/dnaeon/golinkname) repo and can be
installed via the following command.

```shell
go install -v github.com/dnaeon/golinkname/cmd/golinkname@latest
```

The `golinkname` tool supports a number of sub-commands, which allow you to
generate an index of all `linkname` directives in a given Go module in JSON
format, list a user-friendly table of the discovered symbols, and find _related_
symbols.

```shell
$ golinkname help
NAME:
   golinkname - Lookup //go:linkname directives in a Go module

USAGE:
   golinkname [global options] [command [command options]]

COMMANDS:
   index    Emit a JSON array of every directive in the module
   refs     Find directives whose target is exactly <pkgpath>.<name>
   related  Find every directive related to <pkgpath>.<name>
   list     Display a table of discovered directives
   help, h  Shows a list of commands or help for one command

GLOBAL OPTIONS:
   --help, -h  show help
```

We can list all linkname symbols from the current Go module using the following
command.

```shell
golinkname list
```

Running the `list` sub-command against the example project we've built before we
can see these symbols.

```shell
$ golinkname list --dir /path/to/linkname-example
FILE:LINE          FORM          DIR   LOCAL  TARGET                        RESOLVED           WARNINGS
pkg/bar/bar.go:13  func/two-arg  pull  f      linkname-example/pkg/foo.foo  pkg/foo/foo.go:19  -
pkg/foo/foo.go:18  func/one-arg  push  foo    -                             -                  -
```

In order to list all `linkname` symbols from the standard library we can use the
following command.

```shell
golinkname list --dir "$(go env GOROOT)/src"
```

Example snippet from the command above looks like this.

```shell
$ golinkname list --dir "$( go env GOROOT)/src"
FILE:LINE                                           FORM                 DIR   LOCAL                                          TARGET                                              RESOLVED                                            WARNINGS
arena/arena.go:87                                   func/two-arg         push  reflect_arena_New                              reflect.arena_New                                   reflect/arena.go:18                                 -
arena/arena.go:92                                   func/one-arg         pull  runtime_arena_newArena                         -                                                   -                                                   -
arena/arena.go:95                                   func/one-arg         pull  runtime_arena_arena_New                        -                                                   -                                                   -
arena/arena.go:101                                  func/one-arg         pull  runtime_arena_arena_Slice                      -                                                   -                                                   -
arena/arena.go:104                                  func/one-arg         pull  runtime_arena_arena_Free                       -                                                   -                                                   -
arena/arena.go:107                                  func/one-arg         pull  runtime_arena_heapify                          -                                                   -                                                   -
crypto/fips140/enforcement.go:40                    func/one-arg         pull  setBypass                                      -                                                   -                                                   -
crypto/fips140/enforcement.go:43                    func/one-arg         pull  isBypassed                                     -                                                   -                                                   -
crypto/fips140/enforcement.go:46                    func/one-arg         pull  unsetBypass                                    -                                                   -                                                   -
crypto/internal/fips140/cast.go:16                  func/two-arg         pull  fatal                                          crypto/internal/fips140.fatal                       crypto/internal/fips140/cast.go:17                  -
crypto/internal/fips140/check/check.go:32           var/two-arg-extern   pull  Linkinfo                                       go:fipsinfo                                         -                                                   -
crypto/internal/fips140/check/checktest/test.go:20  var/two-arg          pull  RODATA                                         crypto/internal/fips140/check/checktest.RODATA      crypto/internal/fips140/check/checktest/test.go:21  -
...
```

The `index` sub-command returns a JSON index of the discovered `linkname`
directives and their respective symbols.

```shell
golinkname index --pretty
```

The result from the `index` sub-command is what can be used to build
integrations with other tools, editors, or IDEs. An Emacs package which wraps
`golinkname` is available in the
[editor/emacs](https://github.com/dnaeon/golinkname/tree/main/editor/emacs)
directory, which lets you list, and navigate through the linkname directives in
a Go codebase.

The following demo shows the Emacs integration with `golinkname`, which takes
advantage of the `golinkname` index.

[![]({{ site.baseurl }}/images/golinkname-emacs.gif)]({{ site.baseurl }}/images/golinkname-emacs.gif){:.glightbox}

We can also find who _references_ a given symbol via a `linkname` directive
using the `refs` sub-command.

```shell
$ golinkname refs linkname-example/pkg/foo.foo | jq
[
  {
    "schemaVersion": 1,
    "file": "pkg/bar/bar.go",
    "line": 13,
    "col": 1,
    "form": "two-arg",
    "direction": "pull",
    "localName": "f",
    "declName": "f",
    "declKind": "func",
    "target": {
      "raw": "linkname-example/pkg/foo.foo",
      "pkgPath": "linkname-example/pkg/foo",
      "name": "foo",
      "resolved": [
        {
          "file": "pkg/foo/foo.go",
          "line": 19,
          "col": 6,
          "inModule": true
        }
      ]
    },
    "hasUnsafeImport": true,
    "warnings": []
  }
]
```

The example above shows that the `pkg/bar.f` symbol references the `pkg/foo.foo`
symbol via a `linkname` directive, which is exactly what the example project
did.

If we run the same command, but against the `pkg/bar.f` symbol we would see that
nobody references it, which is correct, since nobody has a `linkname` reference
to it.

```shell
$ golinkname refs --dir /tmp/linkname-example linkname-example/pkg/bar.f | jq
[]
```

However, we can see to which other symbols `pkg/bar.f` is _related_.

```shell
$ golinkname related --dir /tmp/linkname-example linkname-example/pkg/bar.f | jq
[
  {
    "schemaVersion": 1,
    "file": "pkg/bar/bar.go",
    "line": 13,
    "col": 1,
    "form": "two-arg",
    "direction": "pull",
    "localName": "f",
    "declName": "f",
    "declKind": "func",
    "target": {
      "raw": "linkname-example/pkg/foo.foo",
      "pkgPath": "linkname-example/pkg/foo",
      "name": "foo",
      "resolved": [
        {
          "file": "pkg/foo/foo.go",
          "line": 19,
          "col": 6,
          "inModule": true
        }
      ]
    },
    "hasUnsafeImport": true,
    "warnings": []
  }
]
```

The output above shows that `pkg/bar.f` is _related_ to `pkg/foo.f`, because it
_references_ it.

Make sure to check the [dnaeon/golinkname](https://github.com/dnaeon/golinkname)
repo for additional details.
