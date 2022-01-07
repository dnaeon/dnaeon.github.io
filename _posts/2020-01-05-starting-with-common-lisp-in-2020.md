---
layout: post
title: Starting with Common Lisp in 2020
tags: lisp programming
---
In my spare time (whenever I get to have some these days) I like to
poke around in the world of
[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)).

I do have some experience with various Lisp dialects such as Scheme,
[Racket](https://racket-lang.org) and [Clojure](https://clojure.org),
so this is not my first introduction to the Lisp family. I've been
developing in Clojure at my current job and has also used Scheme
([Chicken Scheme](https://www.call-cc.org) in particular) and Racket
in various small to mid-sized personal and hobby projects. Having
spent a fair amount of my time with the various Lisp dialects I
wouldn't say that I'm expert in Lisp, not even close.

We are now in the 2020 year, and this is also the year that Lisp would
become 62 years old! That is an impressive age for a programming
language. With that said that does not mean that the concepts and
ideas developed in Lisp are old.

A thing so common for all languages these days such as
[if-then-else](https://en.wikipedia.org/wiki/If–then–else) was first
introduced by the creator of Lisp itself - [John
McCarthy](https://en.wikipedia.org/wiki/John_McCarthy_(computer_scientist)).
Another notable thing so common to lots of modern languages such as
[garbage
collection](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science))
came from Lisp too. The [Read-Eval-Print-Loop
(REPL)](https://en.wikipedia.org/wiki/Read–eval–print_loop) that most
of us cannot live without is another good example.

And these are just a few of the things that Lisp gave to the world.

These days Lisp isn't very popular compared to other languages as it
used to be back in the days, but that isn't to say that Lisp itself is
dead. As a matter of fact it is in a pretty good shape - various
Common Lisp implementations are actively being maintained -- few days
ago [SBCL 2.0.0](http://www.sbcl.org/all-news.html#2.0.0) got released
for example. Research and innovations are still happening in Racket
(check out the Honu project for instance), and Clojure is kicking
butts in the JVM world.

So, why spending time with Lisp you might ask?

Is it for the famous Lisp enlightenment everyone is talking about? I
don't know, maybe. Personally, I find programming in Lisp quite fun.
Exploring stuff and poking around at the REPL in Common Lisp is a
whole different story than most of the REPLs you've dealt with.  Maybe
it is my personal curiousity about programming and computer science
that drove me to Lisp.

The language has a long history, so you would find lots of
reading materials, research papers and it's fascinating to know that
you can still run such old code in modern Common Lisp implementations.

Have I been enlighted by Lisp? I don't feel smarter after learning
Lisp. I do however feel that I've gained more experience as a
programmer. And maybe that's the *Lisp enlightment* these old Lispers
talk about, maybe it's something else. What I do know however is that
learning new paradigms and techniques definitely makes you a better
problem solver, so that's another another reason to try learning some
Lisp.

Oh, and I *do* like the s-expr syntax. That for me is a good enough
reason to be excited about Lisp.

In this post I'm going to summarize my experience with [Common
Lisp](https://en.wikipedia.org/wiki/Common_Lisp) so far, and hopefully
the things I write here are going to be useful to someone else in
his/hers own journey in the world of Lisp.

I also need to mention that even though I have experience with Scheme
and Clojure, I'm quite new to Common Lisp. These being Lisp dialects
does not mean they are fully compatible with each other. Scheme takes
the functional-first approach to solving problems and Clojure is a
more modern reincarnation of Lisp which is closer to Scheme being a
Lisp-1 dialect, while Common Lisp is the ANSI standardized vision of
what Lisp should be.

Also keep in mind that this post is not intended as a tutorial for all
things related to Lisp, but rather they document my particular
development setup and show various code samples which I've found
useful, and hopefully others will too.

At the end of this post you will also find additional references to
learning materials such as books and documentation, which I'd
recommend checking out. These references will be valuable to anyone
starting up with Common Lisp as they provide useful information and
insight.

## Installing Common Lisp

The nice thing about Common Lisp being ANSI standardized language is
that you can [choose between various
implementations](https://common-lisp.net/implementations).

You need high-performance Lisp compiler -
[SBCL](http://www.sbcl.org/all-news.html#2.0.0) has got you covered,
you need to use the JVM platform - [ABCL](https://abcl.org) is there
for you. You can check out the various Common Lisp implementations and
choose the one that suits you most. My personal choice has been SBCL,
but as long as you develop standard compliant code your project be
portable to any conforming implementation.

Installing SBCL on my Mac is as simple as the following command.

``` shell
brew install sbcl
```

And when I'm working on my [Arch Linux](https://www.archlinux.org) system
I would install SBCL using the following command.

``` shell
pacman -S sbcl
```

You would want to get a copy of the [Common Lisp
Hyperspec](http://www.lispworks.com/documentation/common-lisp.html) as
well. On Macs you can install the HyperSpec using the following command.

``` shell
brew install hyperspec
```

Or you can grab the HyperSpec from
[here](http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz)
instead.

After installing SBCL you can test things up quickly in the REPL and
confirm everything has been installed properly.

``` shell
$ sbcl
This is SBCL 2.0.0, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
```

Evaluating the following expression from the REPL would return
the Lisp implementation type and version.

``` emacs-lisp
* (values (lisp-implementation-type)
          (lisp-implementation-version))
"SBCL"
"2.0.0"
```

## Editor setup

I've been a long time Emacs user and for me the choice of editor
support for Common Lisp is simple -
[SLIME](https://common-lisp.net/project/slime/).  Vim users would
probably want to pick [Vlime](https://github.com/l04m33/vlime).

What follows below are snippets from my Emacs config. I keep these as global
settings at the top of my `~/.emacs` file.

``` emacs-lisp
(server-start)

(setq column-number-mode t)
(global-display-line-numbers-mode)
(global-hl-line-mode 1)
(show-paren-mode 1)
(save-place-mode 1)

;; Disable tabs globally and enable them
(setq indent-tabs-mode nil)

;; We want newlines at end of file
(setq require-final-newline 1)
```

Config settings for installing packages from [MELPA](https://melpa.org).

``` emacs-lisp
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
```

I also keep various personal Elisp snippets that I usually throw at
`~/.emacs.d/lisp`, so I have this as well.

``` emacs-lisp
;; For storing custom snippets
(let ((path (expand-file-name "~/.emacs.d/lisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))
```

My theme settings - I use one for day-time coding and another one for
late-night work. I've found that personally for myself working in
light themes during the day works best for my eyes and usually at
night I would switch my theme to a darker one.

``` emacs-lisp
;; Theme customizations

;; Night theme
(load-theme 'doom-nova t)

;; Daylight theme
;; (load-theme 'solarized-light t)
```

And then I have my package declarations.

``` emacs-lisp
(use-package beacon
  :config
  (beacon-mode 1))

(use-package winner
  :init
  (winner-mode t))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package go-eldoc
  :defer t)

(use-package go-mode
  :init
  (progn
    (setq gofmt-command "goimports")
    (setq indent-tabs-mode t)
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package racket-mode
  :init
  (progn
    (add-hook 'racket-mode-hook
	      (lambda ()
		(define-key racket-mode-map (kbd "C-c r") 'racket-run)))))

(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("C-x o" . ace-window))
  :config
  (setq aw-background nil))

(use-package which-key
  :config
  (which-key-mode t))

(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-init))

(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-indent-width 4))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ivy
  :bind
  (("C-s" . 'swiper)
   ("M-x" . 'counsel-M-x)
   ("C-x C-f" . 'counsel-find-file)
   ("C-c C-r" . 'ivy-resume))
  :init
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  (setq magit-completing-read-function 'ivy-completing-read)
  :config
  (use-package ivy-rich)
  (ivy-rich-mode t))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package company
  :defer t
  :config
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location))

(use-package slime-company
  :defer t)

(use-package slime
  :bind (("M-TAB" . company-complete)
	 ("C-c C-d C-s" . slime-describe-symbol)
	 ("C-c C-d C-f" . slime-describe-function))
  :init
  (setq slime-lisp-implementations '((sbcl ("sbcl")))
	slime-default-lisp 'sbcl)
  (setq common-lisp-hyperspec-root
        "/usr/local/share/doc/hyperspec/HyperSpec/")
  (setq common-lisp-hyperspec-symbol-table
        (concat common-lisp-hyperspec-root "Data/Map_Sym.txt"))
  (setq common-lisp-hyperspec-issuex-table
        (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))
  (slime-setup '(slime-fancy slime-company slime-cl-indent)))

(defun slime-description-fontify ()
  (with-current-buffer "*slime-description*"
    (slime-company-doc-mode)))

(defadvice slime-show-description (after slime-description-fontify activate)
  "Fontify sections of SLIME Description."
  (slime-description-fontify))
```

## Quicklisp

Quicklisp is a package manager for Lisp libraries. You would want to
have it, when you need to pull in some external library that your code
will use, so now it is a good time to go ahead and install it.

Go ahead and read the installation instructions
[here](https://www.quicklisp.org/beta/#installation). The summarized
steps can be found below.

``` shell
curl -O https://beta.quicklisp.org/quicklisp.lisp
curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
curl -O https://beta.quicklisp.org/release-key.txt
gpg --import release-key.txt
gpg --verify quicklisp.lisp.asc quicklisp.lisp
sbcl --load quicklisp.lisp
```

Make sure to check out the [documentation at
Quicklisp](https://www.quicklisp.org/beta/#installation) for more
information and some examples.

## Code samples

What follows below are some code snippets I've written in Common Lisp.
When starting with a new language I usually have a list of problems
and tasks that I go through, just to get a rough idea how expressing a
solution to a task in the language feels like.

## Hash Tables

[Hash tables](http://cl-cookbook.sourceforge.net/hashes.html) in
Common Lisp are the analog of dictionaries in Python or hash maps in
other languages. In order to create a new hash table in Common Lisp
you would use the
[MAKE-HASH-TABLE](http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_has.htm)
function, e.g.

``` common-lisp
CL-USER> (defparameter *ht* (make-hash-table))
*HT*
```

This is how our hash table is represented when printed in the REPL.

``` common-lisp
CL-USER> *ht*
#<HASH-TABLE :TEST EQL :COUNT 0 {1002179983}>
```

You can see the number of keys within the hash table and the function
used to test for equality of keys (in this case
[EQL](http://www.lispworks.com/documentation/HyperSpec/Body/f_eql.htm#eql)).

Let's add a few keys to our hash table.

``` common-lisp
CL-USER> (setf (gethash :a *ht*) 1)
1
CL-USER> (setf (gethash :b *ht*) 2)
2
CL-USER> (setf (gethash :c *ht*) 3)
3
CL-USER> *ht*
#<HASH-TABLE :TEST EQL :COUNT 3 {1002179983}>
```

[SETF](http://www.lispworks.com/documentation/HyperSpec/Body/m_setf_.htm)
is macro used for general-purpose assignment in Common Lisp. When
given a *place* and a value it knows how to properly set the place to
the given value.

``` common-lisp
CL-USER> *ht*
#<HASH-TABLE :TEST EQL :COUNT 3 {1002179983}>
```

We've got 3 keys now, but as you can see you don't know which are
they, just by looking at the above representation of the hash table.

We can however *map* a function over each key/value pair in a hash table
using the [MAPHASH](http://clhs.lisp.se/Body/f_maphas.htm) function.

``` common-lisp
CL-USER> (maphash (lambda (k v)
                    (format t "Key ~a has value ~a~%" k v))
                  *ht*)
Key A has value 1
Key B has value 2
Key C has value 3
```

Let's add this to a utility function we can call up when we need this later.

``` common-lisp
(defun hash-table-dump-kvs (table)
  "Dumps the key/value pairs in a given hash table"
  (maphash (lambda (k v)
             (format t "Key ~a: ~a~%" k v))
           table))
```

## HASH-TABLE-KEYS

Common Lisp as mentioned in the beginning of this post is quite an old
language and predates the "batteries included" language era. With that
said we won't find a function which returns all keys and/or values
within a given hash table as the ANSI standard does not mention such,
but that doesn't mean we cannot implement such functions on our own.

Using `MAPHASH`as shown previously can implement these utility
functions when working with hash tables.

``` common-lisp
(defun hash-table-keys (ht)
  "Returns a list of all keys in a given hash table"
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k result))
             ht)
    result))
```

``` common-lisp
CL-USER> (hash-table-keys *ht*)
(:C :B :A)
```

## HASH-TABLE-VALUES

Using a similar approach we can implement a function to retrieve the
list of values too.

``` common-lisp
(defun hash-table-values (ht)
  "Returns a list of all values in a given hash table"
  (let (result)
    (maphash (lambda (k v)
               (declare (ignore k))
               (push v result))
             ht)
    result))
```

Let's try them out.

``` common-lisp
CL-USER> (hash-table-values *ht*)
(3 2 1)
```

Things seems to work as expected. Retrieving the keys/values in a hash
table is such a common thing, that you probably don't want to
implement that on your own each time you have to work with hash
tables.

Fortunately, this functionality is already part of the [Alexandria
library](https://common-lisp.net/project/alexandria/draft/alexandria.html),
which amongst this contains other helpful utilities.

When I initially started with Common Lisp I wasn't aware of Alexandria, so I
ended up implementing these functions myself, which surprisingly are almost
identical. Nevertheless, this is also a good example of how easy it is
implement functionality, which may not be covered by the ANSI standard.

In order to use Alexandria you need to load it first.

``` common-lisp
CL-USER> (ql:quickload :alexandria)
To load "alexandria":
  Load 1 ASDF system:
    alexandria
; Loading "alexandria"
[package alexandria.1.0.0]......................
(:ALEXANDRIA)
```

And now you can call the functions.

``` common-lisp
CL-USER> (alexandria:hash-table-keys *ht*)
(:C :B :A)
CL-USER> (alexandria:hash-table-values *ht*)
(3 2 1)
```

## HASH-TABLE-GET-IN

If you've done some Clojure programming you would know that hash-maps
are very common when it comes to representing data structures. In Clojure you can
use the [get-in](https://clojuredocs.org/clojure.core/get-in) function to
retrieve a value in a nested hash-map, e.g.

``` clojure
user=> (get-in {:a {:b {:c 42}}} [:a :b :c])
42
```

This is really useful and convenient way to access keys in nested hash tables.

In Common Lisp you don't have that, or at least I haven't found one. I
think this is because Common Lisp does not embrace the idea of using
hash tables for almost every data structure to the same level as it is
in Clojure, because in Common Lisp you can use proper structures and
classes instead. And that's probably a good thing - classes and
structs might be a better choice in representing your data structures
than pure hash tables.

Anyways, I still think that `get-in` is useful on it's own, so I've
implemented it in Common Lisp for the fun of it. Actually, we will
see more than one implementation of the `get-in` function.

One thing worth mentioning is that Common Lisp is a multi-paradigm
language - you can program in functional style, imperative or object-oriented
style. It's up to you, so pick your poison. Personally I like this kind of
freedom that the language allows you to express a solution to a problem
in the most suitable way.

Let's add a few more keys to the `*ht*` hash table.

``` common-lisp
CL-USER> (setf (gethash :foo *ht*) (make-hash-table))
#<HASH-TABLE :TEST EQL :COUNT 0 {1002739123}>
CL-USER> (setf (gethash :bar (gethash :foo *ht*)) 42)
42
```

And here's the first version of our function to retrieve nested keys.

``` common-lisp
(defun hash-table-get-in (ht key &rest keys)
  "Get value of a key in nested hash tables - recursive approach"
  (labels ((process (ht keys)
             (if (endp keys)
                 ht
                 (process (gethash (car keys) ht) (cdr keys)))))
    (process (gethash key ht) keys)))
```

You should note that above function is
[tail-recursive](https://en.wikipedia.org/wiki/Tail_call). However,
you should also know that the Common Lisp ANSI standard does not
specify that implementations should provide support for tail
recursion. Some implementations such as SBCL support tail recursion,
while others simply do not, since this is not a strict requirement of
the standard.

In any case this doesn't really matter for our sample hash table as it
is not deep enough to overflow our stack and SBCL got our backs
covered, since it supports tail recursion.

Here's another variation of above function, but this time using
iteration instead of recursion. The function below uses the
[DO](http://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm)
iteration macro.

``` common-lisp
(defun hash-table-get-in (ht key &rest keys)
  "Get value of a key in nested hash tables - iterative approach"
  (do ((ht (gethash key ht) (gethash (car keys) ht))
       (keys keys (cdr keys)))
      ((endp keys) ht)))
```

And last, but not least here's a solution using
[REDUCE](http://clhs.lisp.se/Body/f_reduce.htm).

``` common-lisp
(defun hash-table-get-in (ht key &rest keys)
  "Get value of a key in nested hash tables - REDUCE approach"
  (reduce (lambda (ht x)
	    (gethash x ht))
	  keys
	  :initial-value (gethash key ht)))
```

Now we have solutions expressed in recursive, iterative and functional
style. I'd say having that kind of choice when programming is truly a
*feature* of the language on it's own.

You can try out above functions in the REPL and confirm they work as
expected, e.g.

``` common-lisp
(hash-table-get-in *ht* :foo :bar)
42
```

We do have now functions to fetch keys in nested hash tables, but is
there a way we can tell what are all the keys in a given hash table?
In other words we want to know the *key paths* we can fetch in a hash
table using above functions.

## HASH-TABLE-KEY-PATHS

Here's one possible implementation of a function that returns the list
of valid *key paths*.  You can use this function to see which valid
keys paths you can use with our `HASH-TABLE-GET-IN` functions we
implemented previously.

``` common-lisp
(defun hash-table-key-paths (ht)
  "Returns all key paths in a hash table"
  (let ((result nil))
    (labels ((process (ht paths)
               (maphash (lambda (k v)
                          (if (hash-table-p v)
                              (process v (append paths (list k)))
                              (setf result (append result (list (append paths (list k)))))))
                        ht)))
      (process ht nil))
    result))
```

Above function does the following,

* Walks over each key/value pair in our hash table and looks up the *value*
* If the *value* is a hash table - note down the *key* we have so far
  and traverse into the new hash table
* When we reach a value, which isn't a hash table - store the
  traversed path of keys we have so far

Another possible approach to above function is to completely get rid
of the recursion as a whole and use a stack data structure along with
iteration.

``` common-lisp
(defun hash-table-key-paths (ht)
  "Returns all key paths in a given hash table - this time using iteration and a stack"
  (let ((stack (list (list ht nil))) ;; <- use a stack to hold all key paths we need to traverse
        (result nil))                ;; <- holds the final result we will return
    (loop while stack do             ;; <- loop until we exhaust the stack
      (destructuring-bind (table paths) (pop stack) ;; <- grab the hash-table/paths pair from the stack
        (when paths
          (push paths result))
        (maphash (lambda (k v) ;; <- map our lambda to the k/v pairs of the hash table
                   (let ((key-path (append paths (list k)))) ;; <- note down the current path so far
                     (if (hash-table-p v)
                       (push (list v key-path) stack) ;; <- new hash table found, which we need to lookup
                       (push key-path result))))      ;; <- regular value found, store the path so far
                 table))
          finally (return (nreverse result)))))
```

Let's try it out.

``` common-lisp
CL-USER> (hash-table-key-paths *ht*)
((:A) (:B) (:C) (:FOO) (:FOO :BAR))
```

Perfect, now we know the list of valid key paths we can use with our
`HASH-TABLE-GET-IN` function.  We will see yet another implementation
of the `HASH-TABLE-KEY-PATHS` function later on in this post when we
talk briefly about graphs and tree traversal. Then we will see another
implementation using a [Depth-First
Search](https://en.wikipedia.org/wiki/Depth-first_search) approach.

## HASH-TABLE-SELECT-KEYS

Another useful function from Clojure is
[select-keys](https://clojuredocs.org/clojure.core/select-keys).

This one is pretty straight-forward using
[REDUCE](http://clhs.lisp.se/Body/f_reduce.htm).

``` common-lisp
(defun hash-table-select-keys (table &rest keys)
  "Returns a new HASH-TABLE with only the keys specified by KEYS"
  (reduce (lambda (acc key)
            (let ((val (gethash key table)))
              (if val
                  (progn (setf (gethash key acc) val) acc)
                  acc)))
          keys
          :initial-value (make-hash-table)))
```

A minor comment here - in above lambda we can replace the `LET` and
`IF` forms with an
[IF-LET](https://common-lisp.net/project/alexandria/draft/alexandria.html#Macro-Writing)
macro from Alexandria. I've left it out from the code for clarity, but
what you would probably want to do is to use `IF-LET` instead.

Now we can have a new hash table with just a subset of the keys of an
existing one, e.g.

``` common-lisp
CL-USER> (hash-table-select-keys *ht* :a :b)
#<HASH-TABLE :TEST EQL :COUNT 2 {100197CB03}>
```

## GROUP-BY

Another useful bit from the functional world is the `group-by`
function. Check out [group-by in
Racket](https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._group-by%29%29)
and [group-by in
Clojure](https://clojuredocs.org/clojure.core/group-by) for more info.

Using `group-by` we can group items in equivalence groups. Say for
example we want to group all items in a list in groups determined by
their length. Here's how we can implement our own `group-by` function.

``` common-lisp
(defun group-by (fun list)
  "Groups items in equivalence groups"
  (reduce (lambda (acc item)
            (let* ((k (funcall fun item))
                   (v (gethash k acc nil)))
              (setf (gethash k acc) (append v (list item)))
              acc))
          list
          :initial-value (make-hash-table)))
```

And now, let's try our `group-by` function.

``` common-lisp
CL-USER> (hash-table-dump-kvs (group-by #'length
                                        (list "a" "bb" "ccc" "dd" "e")))
Key 1: (a e)
Key 2: (bb dd)
Key 3: (ccc)
NIL
```

Another example showing how to group items in a list based on whether
the item is being an odd number.

``` common-lisp
CL-USER> (hash-table-dump-kvs (group-by #'oddp
                                        (list 1 2 3 4 5 6 7 8 9 10)))
Key T: (1 3 5 7 9)
Key NIL: (2 4 6 8 10)
NIL
```

One final word about `group-by` - you should note that this
functionality can already be used as part of the [group-by
library](https://quickref.common-lisp.net/group-by.html),
which is available in Quicklisp, and contains more
features out of the box such as using custom functions to
extract the keys and values from items.

I've included my implementation of `group-by` in this post,
mainly because it shows a different approach to grouping items
using `REDUCE`.

## REPEAT

Often times I find myself needing to repeat some sequence of items a
given number of times. In order to do that in Common Lisp we can use the
[MAKE-LIST](http://clhs.lisp.se/Body/f_mk_lis.htm) function. 

``` common-lisp
CL-USER> (make-list 10 :initial-element :A)
(:A :A :A :A :A :A :A :A :A :A)
```

Or, if you need to implement one yourself you can use one of the following,
although I'd discourage doing that unless you are doing this for the sake of
experimenting and practicing.

``` common-lisp
(defun repeat (n item)
  "Returns a list of ITEMs repeated N times using recursion"
  (labels ((process (i acc)
             (if (>= i n)
                 acc
                 (process (1+ i) (cons item acc)))))
    (process 0 nil)))
```

And another approach using iteration using with the
[DOTIMES](http://clhs.lisp.se/Body/m_dotime.htm) iteration macro.

``` common-lisp
(defun repeat (n item)
  "Returns a list of ITEMs repeated N times using iteration"
  (let ((result nil))
    (dotimes (i (1- n))
      (push item result))
    result))
```

You can use `REPEAT` like this for example.

``` common-lisp
CL-USER> (repeat 10 :A)
(:A :A :A :A :A :A :A :A :A)
```

## TAKE & DROP

The `TAKE` and `DROP` functions are really useful when we are
interested in just a subset of a sequence. Here is how we can
implement them.

``` common-lisp
(defun take (n list)
  "Takes N items from LIST"
  (if (>= n (length list))
      list
      (subseq list 0 n)))
```

Example usage of `TAKE`:

``` common-lisp
CL-USER> (take 3 (list 1 2 3 4 5))
(1 2 3)
```

Now we have a safer wrapper around
[SUBSEQ](http://clhs.lisp.se/Body/f_subseq.htm).

And this is how we can write `DROP`.
``` common-lisp
(defun drop (n list)
  "Drops N items from LIST and returns the CDR"
  (nthcdr n list))
```

Example usage of `DROP`.

``` common-lisp
CL-USER> (drop 3 (list 1 2 3 4 5))
(4 5)
```

## PARTITION

When you need to partition a list of items into groups we can use the
`partition` function as described below. Here is a recursive approach to
implementing this function.

``` common-lisp
(defun partition (n list)
  "Partitions the given LIST into groups of N items - recursive approach"
  (labels ((process (list acc)
             (cond
               ((endp list) acc)
               (t (process (drop n list) (append acc (list (take n list))))))))
    (process list nil)))
```

And the iterative approach using `DO` iteration macro looks like this.

``` common-lisp
(defun partition (n list)
  "Partitions the given LIST into groups of N items - iterative approach"
  (do ((result nil (append result (list (take n list))))
       (list list (drop n list)))
      ((endp list) result)))
```

And you can partition a list of values like this for example.

``` common-lisp
CL-USER> (partition 2 (list 1 2 3 4 5 6))
((1 2) (3 4) (5 6))
```

However, see what happens if we try partitioning with a number greater
than the items we can put in the group.

``` common-lisp
CL-USER> (partition 4 (list 1 2 3 4 5 6))
((1 2 3 4) (5 6))
```

The last group contains only two items, since there was nothing left
to fill in it.  We can improve our first version of the `partition`
function to support padding as well. Here's how we can do it.

``` common-lisp
(defun partition (n list &key (pad nil pad-supplied-p))
  "Partitions the given LIST into groups of N items - with padding support"
  (labels ((process (list acc)
             (let ((group (take n list)))
               (cond
                 ((endp list) acc)
                 (pad-supplied-p (let ((padding (make-list (- n (length group))
                                                           :initial-element pad)))
                                   (process (drop n list)
                                            (append acc (list (append group padding))))))
                  (t (process (drop n list)
                              (append acc (list group))))))))
    (process list nil)))
```

Using the `:pad` key we can now fill our groups with whatever items is
required to make it up for the number of items we initially requested,
e.g.

``` common-lisp
CL-USER> (partition 4 (list 1 2 3 4 5 6) :pad nil)
((1 2 3 4) (5 6 NIL NIL))
```

## DFS

Implementing [DFS](https://en.wikipedia.org/wiki/Depth-first_search)
is one of the things I tend to implement when starting with a new
language. It is a good task to practice on.

The solution below is more-or-less a translation of how I would
implement this in Scheme for example.

I will use a property list to represent the adjacency list of vertices
in my sample graph. Perhaps a better way to represent this would be to
use a class, but I have yet to learn about CLOS in my Common Lisp
journey, so for now a plist would be more than enough.

``` common-lisp
(defparameter *g* '(:A (:B :C)
                    :B (:A)
                    :C (:A :D)
                    :D (:C)))
```

Then we will write this helper function which simply returns the
neighbors of a given node.

``` common-lisp
(defun neighbors (graph node)
  "Returns the neighbors of NODE"
  (getf graph node))
```

And this is our first implementation of DFS.

``` common-lisp
(defun dfs (graph root)
  "Traverses the graph in Depth-First Search order starting with ROOT node"
  (labels ((traverse (stack visited)
             (let* ((node (car stack))
                    (neighbors (neighbors graph node))
                    (not-seen (remove-if (lambda (x) (member x visited))
                                         neighbors))
                    (new-visited (cons node visited))
                    (new-stack (append not-seen (cdr stack))))
               (cond
                 ((endp new-stack) (nreverse new-visited))
                 (t (traverse new-stack new-visited))))))
    (traverse (list root) nil)))
```

And now let's try it.

``` common-lisp
CL-USER> (dfs *g* :A)
(:A :B :C :D)
CL-USER> (dfs *g* :D)
(:D :C :A :B)
CL-USER> (dfs *g* :B)
(:B :A :C :D)
CL-USER> (dfs *g* :C)
(:C :A :B :D)
```

This works fine for our sample graph, but what if we wanted to
traverse other data structures? Turns out that with slight
modifications to above code we can make our DFS function work just
fine with anything that can be traversed.

A minor improvement to our function would be to add support for
passing a function, which when given a node it will simply return it's
neighbors.

``` common-lisp
(defun dfs (neighbors-fn root)
  "DFS walks a tree starting with ROOT using the NEIGHBORS-FN to get the neighbors"
  (labels ((traverse (stack visited)
             (let* ((node (car stack))
                    (neighbors (funcall neighbors-fn node))
                    (not-seen (remove-if (lambda (x) (member x visited)) neighbors))
                    (new-stack (append not-seen (cdr stack)))
                    (new-visited (cons node visited)))
               (cond
                 ((endp new-stack) (nreverse new-visited))
                 (t (traverse new-stack new-visited))))))
    (traverse (list root) nil)))
```

Using our latest version of DFS we can now pass functions, which would
evaluate the list of neighbors for a given node. Using our sample
graph from before we can now DFS walk it like this.

``` common-lisp
CL-USER> (dfs (lambda (node)
                 (neighbors *g* node))
                 :A)
(:A :B :C :D)
```

And here's an interesting thing we can do now. Remember the
`HASH-TABLE-KEY-PATHS` function we wrote before?

We can use now implement `HASH-TABLE-KEY-PATHS` by performing a DFS
walk. This is how we can evaluate the list of all possible key paths
in a a given hash table using the previously implemented
`HASH-TABLE-KEYS`, `HASH-TABLE-GET-IN` and `DFS` functions.

First, let's write a function that knows how to get the neighbors of a
key in a hash table.

``` common-lisp
(defun hash-table-key-neighbors-fn (ht)
  "Returns a function to query the neighbors of a key in the hash table"
  (lambda (item)
    (let ((value (apply #'hash-table-get-in ht item)))
      ;; If the key we are looking up is associated with a value
      ;; which happens to be another hash-table then we consider
      ;; the neighbors of that key to be the keys of the nested
      ;; hash table.
      (when (hash-table-p value)
        (mapcar (lambda (k)
                  (append item (list k)))
                (hash-table-keys value))))))
```

And we can write our refactored version of `HASH-TABLE-KEY-PATHS`
using DFS walk.

``` common-lisp
(defun hash-table-key-paths (ht)
  "Returns all key paths in a given hash table using DFS walk"
  (let ((root-keys (mapcar #'list (hash-table-keys ht)))
        (neighbors-fn (hash-table-key-neighbors-fn ht)))
    (apply #'append
           (mapcar (lambda (x)
                     (dfs neighbors-fn x))
                   root-keys))))
```

And we can test it to confirm it works as expected.

``` common-lisp
CL-USER> (hash-table-key-paths *ht*)
((:FOO) (:FOO :BAR) (:C) (:B) (:A))
```

## Macros

Macros is one of the areas in which Lisp truly shines compared to
other programming languages. There are whole books written on the
subject and I'm still fairly new to advanced macro techniques, but I'm
slowly making my way through them.

Clojure programmers are quite familiar with the [threading
macros](https://clojure.org/guides/threading_macros), and in Elixir a
similar concept exists in the form of the [pipe
operator](https://elixirschool.com/en/lessons/basics/pipe-operator/).
Basically what the thread-macros or pipe operator does is that it
allows you to *thread* an expression through series of
transformations. It works in a manner similar to what you do with
[Unix pipes](https://en.wikipedia.org/wiki/Pipeline_(Unix)) in the
shell.

The code below implements *thread-first* and *thread-last* macros.
Note that such functionality has already been implemented by libraries
available via Quicklisp, so you should probably use them instead. I've
written the following macros, just so that I can get a better feel of
how to implement these on my own.The idea behind the code I'm using
below is that we have an *initial-form* and a series of
transformations that we need to
[REDUCE](http://clhs.lisp.se/Body/f_reduce.htm).

This is how the implementation of *thread-first* looks like.

``` common-lisp
(defmacro -> (initial-form &body forms)
  (reduce (lambda (acc form)
            (if (listp form)
                (list* (car form)
                       acc
                       (cdr form))
                (list form acc)))
          forms
          :initial-value initial-form))
```

And this is the implementation of *thread-last*.

``` common-lisp
(defmacro ->> (initial-form &body forms)
  (reduce (lambda (acc form)
            (if (listp form)
                (append form (list acc))
                (list form acc)))
          forms
          :initial-value initial-form))
```

We can test things out now.

``` common-lisp
CL-USER> (-> (list 1 2 3) car sqrt)
1.0
```

Above expression gets expanded to the following.

``` common-lisp
CL-USER> (macroexpand-1 '(-> (list 1 2 3) car sqrt))
(SQRT (CAR (LIST 1 2 3)))
T
```

And here's an example of using *thread-last*.

``` common-lisp
CL-USER> (->> (list 1 2 3)
           (mapcar (lambda (x) (* x 2)))
           (reduce #'+))
12
```

Above example expression gets expanded to the following.

``` common-lisp
CL-USER> (macroexpand-1 '(->> (list 1 2 3)
                          (mapcar (lambda (x) (* x 2)))
                          (reduce #'+)))
(REDUCE #'+ (MAPCAR (LAMBDA (X) (* X 2)) (LIST 1 2 3)))
T
```

## Wrap up

I still need to work through a lot of areas in the language, which I
haven't touched on -
[CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System), the
[Condition
System](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html),
[images](http://www.sbcl.org/manual/#Saving-a-Core-Image), advanced
macro techniques, etc. I'm still quite new to Common Lisp, but I hope
to change that by spending more time with it.

I bought a copy of the [Practical Common Lisp book by Peter
Siebel](http://www.gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html),
which I plan to continue reading and work throught the various
projects in it. It is an excellent introductory book, if you already
have some programming experience, so if you looking for a good book to
get you started with Lisp, I'd recommend this one.

Other books I plan to read once I finish PCL would be the [Common Lisp
Recipes](https://www.amazon.com/Common-Lisp-Recipes-Problem-Solution-Approach/dp/1484211774),
as this one seems to be recommended by a lot of experienced Lispers,
and there's also a good list of [books on Common
Lisp](https://lisp-lang.org/books/) here as well that are worth
checking out.

So far I like the language - it gives the freedom of choice in regards
to which paradigm you want to program in - functional, imperative or
object-oriented. That coupled with the powerful capabilities of the
macro system and the whole expressiveness of the language makes it a
joy to program in.

And finally, if this post sparked some interested in you in regards to
Common Lisp I'd recommend checking these links too, which would be
helpful to anyone starting with Common Lisp.

* [Quicklisp](https://www.quicklisp.org/beta/#installation)
* [Library documentation for Common Lisp libraries](http://quickdocs.org)
* [Common Lisp HyperSpec](http://www.lispworks.com/documentation/common-lisp.html)

You can also go through the [99 Lisp
Problems](http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html)
to practice your skills and you can also see answers to these problems
[here](https://www.informatimago.com/develop/lisp/l99/index.html).
If you've got some experience with other Lisp dialects then this
[reference sheet](http://hyperpolyglot.org/lisp) will provide useful
bits of information.

Oh, and by the way [Lisp is out of this
world](https://www.youtube.com/watch?v=_gZK0tW8EhQ). Happy hacking!
