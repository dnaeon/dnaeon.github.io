---
layout: post
title: Choosing Lua as the data description and configuration language
tags: golang lua programming
---
One the projects I work on during my spare time these days is
[Gru](https://github.com/dnaeon/gru), which is an orchestration and
configuration management framework written in Go and Lua.

Version 0.2.0 of Gru has incorporated
[HCL](https://github.com/hashicorp/hcl) as part of the
configuration language used for describing data structures,
which was a nice addition to Gru and was a step forward to
implementing the [DSL](https://en.wikipedia.org/wiki/Domain-specific_language)
language of the project.

HCL just like other similar configuration languages do one thing -
they describe data structures. Of course there are alternatives
such as [YAML](http://yaml.org/), [JSON](http://www.json.org/),
[TOML](https://github.com/toml-lang/toml) and others, but I found the
syntax of HCL to be somewhat more flexible than the others and also
more to my liking. Besides I haven't used HCL in any of my projects yet,
so I thought that I should just give it a try and see how it goes.

Putting it all together the initial implementation looked like this.

```hcl
// Manages the tmux package
package "tmux" {
  state = "present"
}

// Manages the tmux configuration file
file "/home/dnaeon/.tmux.conf" {
  state = "present"
  mode = 0644
  source = "data/tmux/tmux.conf"
  require = [
    "package[tmux]",
  ]
}
```

The data structures described by this HCL configuration is in turn
used to construct *resources*, which are responsible for executing
[idempotent](https://en.wikipedia.org/wiki/Idempotence) operations.

Overall I was very pleased with how things have progressed so far, but
few things were missing in HCL, which was an issue if I was about
to have a proper DSL language for the project.

In order to provide a good DSL for the project the language I use
must be able to perform other things such as integrating logic,
be able to perform iterations, have conditional expressions, etc.

Unfortunately HCL does not provide support for such things, as it's
primary goal is only describing data structures, but that is the same
case with YAML, JSON and TOML as well.

One way to solve part of this was to integrate
[HIL](https://github.com/hashicorp/hil) together with HCL.

HIL is a small embedded language for string interpolations created by
Hashicorp and is primarily being used in combination with HCL, but
HCL is not strictly required in order to use it.

One thing to note though is that HIL is not a general purpose
programming language, so even if we go with HCL and HIL we will still
not be able to use somewhat complex logic in our configurations.

Another big issue with HIL in my opinion is that even though it is
described as an interpolation language it fails bad when used in
intermediate interpolation, that is a variable created by the value of
another variable. You can read more about this issue
[here](https://github.com/dnaeon/gru/issues/30).

Taking into considerations all these issues, I decided it's time to
try out something else, as clearly HCL would not be the best fit
for my requirements.

[Lua](https://www.lua.org/) on the other hand seems to be exactly
what I was looking for - it is a small, lightweight, fast and dynamic
programming language, which can easily be embedded into a project.

Additional functionallity can be introduced in Lua by writing modules
for it and it's pretty simple to embed Lua into your Go host
programs by using [gopher-lua](https://github.com/yuin/gopher-lua).

Recent versions of [Gru](https://github.com/dnaeon/gru) now ship with
Lua support as the DSL language of the project. The previous HCL
configuration written in the Lua DSL looks like this.

```lua
-- Manage the tmux package
pkg = resource.package.new("tmux")
pkg.state = "present"

-- Manage the tmux configuration file
cfg = resource.file.new("/home/dnaeon/.tmux.conf")
cfg.state = "present"
cfg.mode = tonumber("0644", 8)
cfg.source = "data/tmux/tmux.conf"
cfg.require = { pkg:ID() }

-- Add the resources to catalog
catalog:add(pkg, cfg)
```

Since the DSL is now built on top of a real programming language we
can do things like this.

```lua
for i=1, 10 do
  f = resource.file.new("/tmp/" .. i)
  f.state = "present"
  catalog:add(f)
end
```

If we had to do the same thing as the code above, but in HCL we would
have to write ten
[file resources](https://godoc.org/github.com/dnaeon/gru/resource#File)
separately, which is a bit tedious and doesn't seem right.

This is a simple example of what we can do with Lua, but hopefully
you get the idea and see its benefits.

What's also nice about having Lua as the DSL language for the project is
that we can easily extend it - exposing Go functionality in Lua is
pretty straightforward and [gopher-lua](https://github.com/yuin/gopher-lua)
provides all the necessary bits to make that happen.

Another great feature of having Lua as part of the project is that
we can take advantage of using a
[REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop),
which is a nice addition in situations when we are debugging some
code or just want to test things out.

Orchestration and configuration management done via a REPL seems like a
strange idea, but I'm sure this would come handy during the development
process or a debug session.

In order to ensure the future growth of the project we must first
ensure that the DSL language can be easily extended, and I believe that
Lua is going to help me get there. It is a tiny language, but with
great potential.
