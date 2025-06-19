---
layout: post
title: Zsh completions support added in clingon
tags: common-lisp lisp getopt options-parser command-line zsh completions
---
In a [previous
post](http://dnaeon.github.io/clingon-command-line-options-parse-for-cl/)
I've introduced a new Common Lisp system for parsing command-line
options named [clingon](https://github.com/dnaeon/clingon).

Starting with `clingon` version `0.3.0` we now have support for
generating Zsh completions for our command-line apps.

[![]({{ site.baseurl }}/images/clingon-zsh-completions.gif)]({{ site.baseurl }}/images/clingon-zsh-completions.gif){:.glightbox}

You can read more about the [shell
completions](https://github.com/dnaeon/clingon#shell-completions)
support in the documentation. Also make sure to check the
[clingon-demo](https://github.com/dnaeon/clingon/tree/master/examples/demo)
application, which contains many other examples.
