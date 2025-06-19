---
layout: post
title: Generating graphs from your Makefiles
tags: graph gnu make dot graphviz
---
Anyone who has done any kind of programming at some point comes across
[GNU Make](https://www.gnu.org/software/make/) and `Makefile`.

So what is `make(1)` and what are these `Makefile`s?

Instead of trying to explain this I'll simply quote here the official [GNU Make
documentation](https://www.gnu.org/software/make/).

> GNU Make is a tool which controls the generation of executables and other
> non-source files of a program from the program's source files.
>
> Make gets its knowledge of how to build your program from a file called the
> makefile, which lists each of the non-source files and how to compute it from
> other files. When you write a program, you should write a makefile for it, so
> that it is possible to use Make to build and install the program.

If you haven't heard or used `make(1)` before, I'd recommend that you go and
check the [GNU Make documentation](https://www.gnu.org/software/make/) for
introduction and a tutorial on creating your first Makefile.

In the rest of this post I assume that you have already used `make(1)` and know
what it does.

Okay, with that out of the way it's time to focus on the topic of this post.
Working with Makefiles can sometimes be challenging, especially if your project
is large enough to contain lots of
[targets](https://www.gnu.org/software/make/manual/make.html#Makefile-Contents)
and lots of dependencies between each of them.

Navigating through these targets and their dependencies may not be an easy task.
For example [including other
Makefiles](https://www.gnu.org/software/make/manual/html_node/Include.html)
while it helps keeping things logically separated introduces additional
complexity when a person needs to understand the additional targets and
additional dependencies from the included Makefile.

Okay, so what can we do about that? How to navigate through large Makefiles and get
a better understanding of all the targets and their dependencies?

Internally, GNU Make (and other make implementations) are building a [dependency
graph](https://en.wikipedia.org/wiki/Dependency_graph), where the targets
represent the _vertices_ and the _edges_ which connect them are represented by the
target
[prerequisite](https://www.gnu.org/software/make/manual/make.html#Prerequisite-Types).

Knowing this we can actually build a graph from our Makefiles and visualize
them, which will help us navigate through the jungle of targets and their
dependencies.

Okay, first things first. How do we get the graph representation which `make(1)`
is using? This one is easy with GNU Make.

All you have to do in order to dump make's internal database is call `make(1)` with these flags.

``` shell
make --print-data-base \
     --no-builtin-rules \
     --no-builtin-variables \
     --dry-run \
     --always-make \
     --question
```

Calling this `make(1)` command inside a directory with a Makefile will dump the
internal database of GNU Make.

Here's a sample output from one of my Makefiles.

``` text
# GNU Make 4.4.1
# Built for x86_64-pc-linux-gnu
# Copyright (C) 1988-2023 Free Software Foundation, Inc.
# License GPLv3+: GNU GPL version 3 or later <https://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# Make data base, printed on Sun Apr 28 16:38:33 2024

# Variables

# environment
TMUX_PANE = %0
# environment
GO111MODULE = on
# environment
QT_WAYLAND_RECONNECT = 1
# variable set hash-table stats:
# Load=109/1024=11%, Rehash=0, Collisions=6/141=4%

# Pattern-specific Variable Values

# No pattern-specific variable values.

# Directories

# . (device 65041, inode 3154786): 17 files, no impossibilities.

# 17 files, no impossibilities in 1 directories.

# Implicit Rules

# No implicit rules.

# Files

test_cover:
#  Phony target (prerequisite of .PHONY).
#  Implicit rule search has not been done.
#  File does not exist.
#  File has not been updated.
#  recipe to execute (from 'Makefile', line 8):
	go test -v -race -coverprofile=coverage.txt -covermode=atomic ./...

# Not a target:
Makefile:
#  Implicit rule search has been done.
#  File is secondary (prerequisite of .SECONDARY).
#  Last modified 2022-08-19 12:15:43.700764449
#  File has been updated.
#  Successfully updated.

# Not a target:
.DEFAULT:
#  Implicit rule search has not been done.
#  Modification time never checked.
#  File has not been updated.

test:
#  Phony target (prerequisite of .PHONY).
#  Implicit rule search has not been done.
#  File does not exist.
#  File has not been updated.
#  recipe to execute (from 'Makefile', line 5):
	go test -v -race ./...

get:
#  Phony target (prerequisite of .PHONY).
#  Implicit rule search has not been done.
#  Implicit/static pattern stem: ''
#  File does not exist.
#  File has been updated.
#  Needs to be updated (-q is set).
# automatic
# @ := get
# automatic
# * :=
# automatic
# < :=
# automatic
# + :=
# automatic
# % :=
# automatic
# ^ :=
# automatic
# ? :=
# automatic
# | :=
# variable set hash-table stats:
# Load=8/32=25%, Rehash=0, Collisions=1/11=9%
#  recipe to execute (from 'Makefile', line 2):
	go get -v -t -d ./...

# Not a target:
.SUFFIXES:
#  Implicit rule search has not been done.
#  Modification time never checked.
#  File has not been updated.

.PHONY: get test test_cover
#  Implicit rule search has not been done.
#  Modification time never checked.
#  File has not been updated.

# files hash-table stats:
# Load=7/1024=1%, Rehash=0, Collisions=0/23=0%
# VPATH Search Paths

# No 'vpath' search paths.

# No general ('VPATH' variable) search path.

# strcache buffers: 1 (0) / strings = 26 / storage = 226 B / avg = 8 B
# current buf: size = 8162 B / used = 226 B / count = 26 / avg = 8 B

# strcache performance: lookups = 36 / hit rate = 27%
# hash-table stats:
# Load=26/8192=0%, Rehash=0, Collisions=0/36=0%
# Finished Make data base on Sun Apr 28 16:38:33 2024

```

If you look closely you will see that this output contains multiple sections:

- A header describing the version of the make tool
- A section containing the environment variables
- Pattern-specific Variable Values
- No pattern-specific variable values
- etc.

Within the `Files` section we can also see our targets and this is exactly what
we need and what we are going to be parsing.

The good news is that I've already implemented the parser and you can install it
as a CLI tool, or use it as a Go package in case you want to implement different
graph representations.

You can find the code in the
[dnaeon/makefile-graph](https://github.com/dnaeon/makefile-graph) and here's how
you can install it.

``` shell
go install github.com/dnaeon/makefile-graph/cmd/makefile-graph@latest
```

If you want to use the parser and do whatever you want to do with the parsed
graph you can install the `parser` package.

``` shell
go get -v github.com/dnaeon/makefile-graph/pkg/parser
```

Here are some screenshots of `makefile-graph` in action. The commands below are
using the following Makefile, from which we will be generating the graph
representation in [dot format](https://graphviz.org/doc/info/lang.html).

``` makefile
edit : main.o kbd.o command.o display.o \
       insert.o search.o files.o utils.o
	cc -o edit main.o kbd.o command.o display.o \
		   insert.o search.o files.o utils.o

main.o : main.c defs.h
	cc -c main.c
kbd.o : kbd.c defs.h command.h
	cc -c kbd.c
command.o : command.c defs.h command.h
	cc -c command.c
display.o : display.c defs.h buffer.h
	cc -c display.c
insert.o : insert.c defs.h buffer.h
	cc -c insert.c
search.o : search.c defs.h buffer.h
	cc -c search.c
files.o : files.c defs.h buffer.h command.h
	cc -c files.c
utils.o : utils.c defs.h
	cc -c utils.c
clean :
	rm edit main.o kbd.o command.o display.o \
	   insert.o search.o files.o utils.o
```

Here's how to render the graph for all targets from our sample Makefile.

``` shell
makefile-graph --makefile examples/Makefile --direction TB | dot -Tsvg -o graph.svg
```

And this is what it looks like.

[![]({{ site.baseurl }}/images/makefile-graph-1.svg)]({{ site.baseurl }}/images/makefile-graph-1.svg){:.glightbox}

In large enough Makefiles the generated graph may not be easy to understand as
well, and for that reason `makefile-graph` supports highlighting specific
targets and their dependencies.

For example this command will highlight the `files.o` target and it's
dependencies.

``` shell
makefile-graph \
    --makefile examples/Makefile \
    --direction TB \
    --target files.o \
    --highlight \
    --highlight-color lightgreen
```

The results look like this.

[![]({{ site.baseurl }}/images/makefile-graph-2.svg)]({{ site.baseurl }}/images/makefile-graph-2.svg){:.glightbox}

And if that is not good enough, we can always focus on a specific target and
it's dependencies only.

``` shell
makefile-graph \
    --makefile examples/Makefile \
    --direction TB \
    --target files.o \
    --related-only
```

The results look like this.

[![]({{ site.baseurl }}/images/makefile-graph-3.svg)]({{ site.baseurl }}/images/makefile-graph-3.svg){:.glightbox}

For additional information and examples, please refer to the
[dnaeon/makefile-graph](https://github.com/dnaeon/makefile-graph) repo.
