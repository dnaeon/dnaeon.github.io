---
layout: post
title: clingon -- command-line options parser system for Common Lisp
tags: common-lisp lisp getopt options-parser command-line
---
Not long ago I've decided I would spend more time with Common
Lisp. This is how [my journey in the world of Common Lisp
started](https://dnaeon.github.io/starting-with-common-lisp-in-2020/).

Since then I have been using Common Lisp in places where I would have
usually used Python, Go, a Shell script, or some other language.

It's been less than ~ 2 years since I've started, but I've found the
language to be solid, flexible and powerful, which to me these are
good enough reasons to justify spending even more time with it.

The ecosystem is not as large as the one found in Python or Go, but
that is not a bad thing. I see this as an opportunity. An opportunity
to create an impact to the Common Lisp community by introducing the
"missing" library, which everyone can benefit from.

Since I've started with Common Lisp I have developed and released a
number of systems, which are now part of Quicklisp -
[cl-bcrypt](https://github.com/dnaeon/cl-bcrypt),
[cl-covid19](https://github.com/dnaeon/cl-covid19),
[cl-ssh-keys](https://github.com/dnaeon/cl-ssh-keys),
[cl-rfc4251](https://github.com/dnaeon/cl-rfc4251) and
[cl-migratum](https://github.com/dnaeon/cl-migratum).

Besides these I've also developed a number of internal-only systems,
some of them being small which I've used as my utility systems, others
medium to large sized which have helped me process and transform large
amounts of data as part of my work activities.

I've also had the chance to interact with the community via `/r/lisp`
and the various IRC channels. I've found the community friendly and
helpful. Whenever I was stuck or needed an advice there was always
someone there to help me out and provide some guidance.

Here's another fun fact. For the past ~ 2 years since I've started
with Common Lisp I barely left the REPL. Before I've started with
Common Lisp I was reading about how the CL REPL is superior compared
to other REPLs like Python for example, but I didn't fully realize and
understand that.

I have been and still am using Python as part of my daily job, so
using a REPL is not a new thing for me. However, as I was soon to
discover the REPLs I've used are nothing close to what the Common Lisp
REPL is.

Using the Common Lisp REPL to develop, compile, re-compile and test
your program without having to leave it is a fun and addictive way to
interact with your program.

If you have to compare this workflow to what you do when working with
a statically compiled language like Go for example things can be
summarized to more or less like this:

a) write some code
b) make sure it compiles
c) run it and confirm it behaves as it should
d) start all over and fix those bugs

This is a tedious process compared to the image-based development in
Common Lisp, where all these things are naturally done in the REPL.

However at some point you have to leave the REPL. Once you are done
with your project you would want to provide to the users a nice
command-line version of your code, so they can plug it anywhere they
like as part of their workflows. They might want to wrap it as part of
another script or tool, integrate it within their CI/CD pipelines,
etc.

"Modern" world now also runs everything in micro-services, you have
Docker and Kubernetes everywhere around you, so if you want to be
buzz-word compliant you have to take your application on the
command-line and be ready to be containerized.

This is where I've found something that can be improved in the Common
Lisp ecosystem -- a command-line options parser, which is
feature-rich, flexible, extensible, supports shell completions, and
can generate documentation for you automatically based on the
application structure you are building.

A good example from the Go's world is
[urfave/cli](https://github.com/urfave/cli) and Python's
[click](https://click.palletsprojects.com/). I've used both of these
for production applications in the past and I've like them a lot.

I was looking for a similar library in Common Lisp, but unfortunately
I didn't find such. There are of course
[unix-opts](https://github.com/libre-man/unix-opts),
[clon](https://github.com/didierverna/clon) (which failed to build for
me) and [adopt](https://github.com/sjl/adopt/), but none of them
provided the all-in-one feature set I was looking for.

So, I've decided I could spend some time and work on a command-line
options parser for Common Lisp, which provides similar feature set as
the ones found in Go and Python. That would be my contribution to the
Common Lisp community. This is how
[clingon](https://github.com/dnaeon/clingon) was born.

[![]({{ site.baseurl }}/images/clingon-demo.gif)]({{ site.baseurl }}/images/clingon-demo.gif){:.glightbox}

And here's a summary of the features supported by the `clingon` as of
writing this post.

* Native support for sub-commands
* Support for command aliases
* Short and long option names support
* Short options may be collapsed as a single argument, e.g. `-xyz`
* Long options support both notations - `--long-opt arg` and
  `--long-opt=arg`.
* Automatic generation of help/usage information for commands and
  sub-commands
* Out of the box support for `--version` and `--help` flags
* Support for various kinds of options like `string`, `integer`,
  `boolean`, `switches`, `enums`, `list`, `counter`, etc.
* Sub-commands can lookup global options and flags defined in parent
  commands
* Support for options, which may be required
* Options can be initialized via environment variables
* Single interface for creating options using `CLINGON:MAKE-OPTION`
* Generate documentation for your command-line app
* Support for shell completions
* `clingon` is extensible, so if you don't find something you need you
  can extend it by developing a new option kind, or even new mechanism
  for initializing options, e.g. by looking up an external key/value
  store.

So, there it is. My contribution to the Common Lisp
community. `clingon` serves me well and I hope other Lispers would
benefit from it.  Make sure to check the [clingon
documentation](https://github.com/dnaeon/clingon/blob/master/README.org),
which provides more details and includes various examples such as how
to develop new option kinds.

Take care and stay Lispy!
