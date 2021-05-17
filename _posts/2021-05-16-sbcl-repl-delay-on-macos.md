---
layout: post
title: REPL delay when using SLIME and SBCL on macOS
tags: sbcl common-lisp lisp repl emacs
---
If you are running SLIME with SBCL on macOS you may notice that the
REPL output seems to experience a bit of delay when compared to other
implementations like Clozure CL or ECL for example.

This has been bothering me for a while, but I had no idea how to fix
this.

I was suspecting this is related somehow to threading in macOS, but
had no clue where the actual problem was, so I've posted a [question
on /r/lisp about
it](https://www.reddit.com/r/lisp/comments/n9dyz1/sbcl_slime_macos_slow_repl/).

Shortly after, @stassats has provided a patch, which I've tested and it
appears to fix my REPL delay issues.

You can find the original [patch
here](https://gist.github.com/stassats/2ff288d821937309aecca2424a2214ac).

Here's how to apply the patch, if you are experiencing the same
issues.

The steps below have been tested on:

* GNU Emacs 27.2
* SLIME 20210504.2053
* SBCL 2.1.4

The steps below assume that you have installed SLIME using `M-x
package-install` and your SLIME installation directory resides in
`~/.emacs.d/elpa/slime-<version>`.

If you have SLIME installed somewhere else instead (e.g. you are using
the latest version from the Git repo), make sure to switch to the
correct directory first.

``` bash
cd ~/.emacs.d/elpa/slime-20210504.2053/
cp swank/sbcl.lisp swank/sbcl.lisp.orig
wget https://gist.githubusercontent.com/stassats/2ff288d821937309aecca2424a2214ac/raw/b688959bc38c1d94c79e65f4b17088d7550e8efe/slime.diff
patch < slime.diff
```

Start up your SLIME session and you should be good now.
