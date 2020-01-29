---
layout: post
title: Configure Common Lisp HyperSpec (CLHS) lookup in Emacs
tags: lisp programming
---
[Emacs](https://www.gnu.org/software/emacs/) provides a mechanism for
[advising
functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html),
which is useful in situations when you need to override the original
function with your own one, transform the arguments before passing
them down to the advised function, execute some code before, or after
the function being advised is called, etc.

For more information about the possible ways you can compose the
original function and an advise, please make sure to check the [Emacs
documentation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advice-combinators.html#Advice-combinators).

In this post we will see how to create an advising function for
`hyperspec-lookup` - the function, which takes care for looking up
documentation in the [Common Lisp
HyperSpec](http://www.lispworks.com/documentation/common-lisp.html).

When you invoke `hyperspec-lookup` (bound to `C-c C-d h` when using Slime) the function
will open up your browser with the URL to the CLHS symbol you are looking up.
The browser that will be used is defined by the the `browse-url-browser-function` variable,
which by default is set to `browse-url-default-browser`.

Here is how we can create an Emacs advising function for
`hyperspec-lookup`, which will use the
[w3m](http://w3m.sourceforge.net) text-based browser. This is useful
when you want to stay within your Emacs session and simply have a new
buffer open up with the documentation, instead of having to switch to
an external browser.

``` emacs-lisp
(defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
  (let ((browse-url-browser-function 'w3m-browse-url))
    (apply orig-fun args)))
```

Next, we need to add the advice.

``` emacs-lisp
(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)
```

You can put above code in your `~/.emacs` file. Try `M-x hyperspec-lookup` or
`C-c C-d h` and it should open up a tab in your `w3m` buffer.
