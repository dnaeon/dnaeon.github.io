---
layout: post
title: Making Emacs and consult-imenu better with tree-sitter
tags: emacs consult lsp eglot consult lisp tree-sitter
---
Not long ago I have switched from
[lsp-mode](https://github.com/emacs-lsp/lsp-mode) to
[eglot](https://github.com/joaotavora/eglot) and
[tree-sitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter).

Approximately at the same time I've also moved from
[helm](https://github.com/emacs-helm/helm) to
[vertico](https://github.com/minad/vertico) and the complementary packages for
it such as [marginalia](https://github.com/minad/marginalia),
[consult](https://github.com/minad/consult) and
[orderless](https://github.com/oantolin/orderless).

So far I've been pretty happy with the transition. `eglot` and `vertico` feel
snappy and more responsive than `lsp-mode` and `helm`. And `marginalia` is great
as well, because it provides nice annotations next to your completion
candidates.

Also, `eglot` is part of Emacs for quite some time already, and `vertico` (and
it's complementary packages) build on top of the standard Emacs APIs such as
`completing-read`, which is another good reason to stick with them.

[consult](https://github.com/minad/consult) provides search and navigation
commands, and also comes with support for (amongst other things)
[imenu](https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html)
via `M-x consult-imenu`.

`consult` and `marginalia` pair nicely together. Here's an example of `M-x
consult-imenu` displaying the symbols for an Elisp buffer, which look great. The
extra annotations next to each completion candidate are provided by `marginalia`
itself.

[![]({{ site.baseurl }}/images/consult-imenu-elisp.png)]({{ site.baseurl }}/images/consult-imenu-elisp.png){:.glightbox}

You get a nice menu with entries grouped by `Types`, `Functions`, `Variables`,
etc. You can also narrow down the search for entries of specific kind, e.g. when
you want to browse the `functions` only, simply type `f SPC` in the
`consult-imenu` prompt and it will give you the function definitions only.

At `$WORK` I'm primary working on Go code these days, and one thing I've noticed
when using `M-x consult-imenu` is that the results are less impressive when
compared to the Elisp code.

[![]({{ site.baseurl }}/images/consult-imenu-eglot-imenu.png)]({{ site.baseurl }}/images/consult-imenu-eglot-imenu.png){:.glightbox}

As you can see the results are nowhere near to what `M-x consult-imenu`
(enriched by `marginalia`) displays for Elisp code.

In this post we will see how to make `M-x consult-imenu` look better when
working with Go code (or any other language with `tree-sitter` support).

After some time spent on reading documentation and manuals I was not able to fix
it myself, so I made a [post at
r/emacs](https://www.reddit.com/r/emacs/comments/1ld119l/imenu_with_gomode/)
asking whether someone else was having the same issues as I do. In the meantime
I kept digging.

So, what turned out to be the issue for me, is that `eglot` provides it's own
[imenu-create-index-function](https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html#index-imenu_002dcreate_002dindex_002dfunction)
called `eglot-imenu`, which actually wraps `treesit-simple-imenu`, and
`treesit-simple-imenu` is tree-sitter's approach to building the `imenu` index.

Here's what the value of `imenu-create-index-function` looks like, when set by
`eglot`.

Use `C-h v imenu-create-index-function` (or `M-x describe-variable imenu-create-index-function`).

``` emacs-lisp
imenu-create-index-function is a variable defined in ‘imenu.el’.

Its value is
#f(advice eglot-imenu :before-until treesit-simple-imenu)
Local in buffer main.go; global value is
imenu-default-create-index-function

The function to use for creating an index alist of the current buffer.

It should be a function that takes no arguments and returns
an index alist of the current buffer.  The function is
called within a ‘save-excursion’.

See ‘imenu--index-alist’ for the format of the buffer index alist.

  Automatically becomes buffer-local when set.
  This variable may be risky if used as a file-local variable.
```

My current tree-sitter configuration looks like this. Make sure to check [How to
Get Started with
Tree-Sitter](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter)
if you are just starting out with tree-sitter.

After configuring tree-sitter you should also install the language grammars
using `M-x treesit-install-language-grammar`.

``` emacs-lisp
;; tree-sitter settings

(use-package treesit
  :config
  ;; Language grammars and sources
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")))

  ;; Remap major modes to treesitter modes
  (setq major-mode-remap-alist
        '((go-mode . go-ts-mode)
          (python-mode . python-ts-mode)
          (js-json-mode . json-ts-mode)
          (json-mode . json-ts-mode)
          (sh-mode . bash-ts-mode))))
```

In order to make `eglot` stop messing with `imenu` we need to tell it so. My
current (simplified) `eglot` config looks like this.

Note that we are configuring `eglot-stay-out-of` in the `:config` section, so
that `eglot` doesn't interfere with `imenu` index generation.

``` emacs-lisp
;; eglot settings

(defun eglot-format-buffer-before-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(defun eglot-organize-imports-before-save ()
  (add-hook 'before-save-hook (lambda ()
                                (call-interactively #'eglot-code-action-organize-imports))
            nil t))

(use-package eglot
  :ensure t
  :init
  (require 'company)
  (require 'yasnippet)
  (require 'go-mode)
  :config
  ;; We want to use treesit-simple-imenu for imenu index creation
  (add-to-list 'eglot-stay-out-of 'imenu)

  ;; Shutdown LSP server when last project buffer is closed as well
  (setq eglot-autoshutdown t)

  ;; Enable eglot for the following modes
  (dolist (mode '(go-mode-hook go-ts-mode-hook))
    (add-hook mode 'eglot-ensure))

  ;; Buffer-local hooks which install before-save hooks
  (add-hook 'go-ts-mode-hook #'eglot-format-buffer-before-save)
  (add-hook 'go-ts-mode-hook #'eglot-organize-imports-before-save))

;; eglot-booster requires that `emacs-lsp-booster' is installed [1]
;; [1]: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure t
  :after (eglot)
  :config
  (eglot-booster-mode 1))
```

And my (simplified) settings for `go-ts-mode` look like this.

``` emacs-lisp
;; Go specific settings

;; Help out the builtin project find the Go module path
(require 'project)

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package go-ts-mode
  :ensure t
  :config
  (add-hook 'go-ts-mode-hook #'company-mode))
```

Instead of using `eglot-imenu` we will configure a hook for our
`go-ts-mode` which sets `imenu-index-create-function` to `treesit-simple-imenu`.

``` emacs-lisp
(add-hook 'go-ts-mode (lambda () (setq-local imenu-index-create-function #'treesit-simple-imenu)))
```

Later we will move this hook to the `use-package` definition for `go-ts-mode`.

Next, we need to tell `treesit-simple-imenu` how to generate the imenu
index. `treesit-simple-imenu` relies on `treesit-simple-imenu-settings` variable
when generating the imenu index.

``` emacs-lisp
treesit-simple-imenu-settings is a variable defined in ‘treesit.el’.

Its value is nil

Settings that configure ‘treesit-simple-imenu’.

It should be a list of (CATEGORY REGEXP PRED NAME-FN).

CATEGORY is the name of a category, like "Function", "Class",
etc.  REGEXP should be a regexp matching the type of nodes that
belong to CATEGORY.  PRED should be either nil or a function
that takes a node an the argument.  It should return non-nil if
the node is a valid node for CATEGORY, or nil if not.

CATEGORY could also be nil.  In that case the entries matched by
REGEXP and PRED are not grouped under CATEGORY.

NAME-FN should be either nil or a function that takes a defun
node and returns the name of that defun node.  If NAME-FN is nil,
‘treesit-defun-name’ is used.

‘treesit-major-mode-setup’ automatically sets up Imenu if this
variable is non-nil.

  Probably introduced at or before Emacs version 30.1.
```

By default the `go-ts-mode.el` defines `treesit-simple-imenu-settings` like
this.

``` emacs-lisp
    ;; Imenu.
    (setq-local treesit-simple-imenu-settings
                `(("Function" "\\`function_declaration\\'" nil nil)
                  ("Method" "\\`method_declaration\\'" nil nil)
                  ("Struct" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)))
```

The docstring of `treesit-simple-imenu-settings` shown above shows the structure
of the list. The second item in the each list as mentioned there is a regexp
which matches the type of the tree-sitter node.

When making any changes to `treesit-simple-imenu-settings` it is best that you
review the AST tree as you go by using `M-x treesit-explore-mode`.

This is what the AST looks like for our sample Go program.

[![]({{ site.baseurl }}/images/treesit-explore-mode.png)]({{ site.baseurl }}/images/treesit-explore-mode.png){:.glightbox}

So, after spending some time configuring `treesit-simple-imenu-settings` with
the help of `M-x treesit-explore-mode` here's what my current config looks like.

``` emacs-lisp
;; Treesitter helpers to extract node names for various symbols such as consts,
;; vars, imports, etc.

(defun my/go-ts-var-is-global-p (node)
  "Predicate which tests whether a `var_spec' node belongs to the global scope"
  ;; The parent node for `var_spec' which is `var_declaration' belongs to the
  ;; global scope, if the `var_declaration' node belongs to a parent node of
  ;; type `source_file'.
  (let* ((var-declaration (treesit-parent-until
                           node
                           (lambda (item)
                             (string-equal (treesit-node-type item) "var_declaration"))
                           t))
         (var-declaration-parent (treesit-node-parent var-declaration)))
    (string-equal (treesit-node-type var-declaration-parent) "source_file")))

(defun my/go-ts-const-is-global-p (node)
  "Predicate which tests whether a `const_spec' node belongs to the global scope"
  ;; The parent node for `const_spec' which is `const_declaration' belongs to the
  ;; global scope, if the `const_declaration' node belongs to a parent node of
  ;; type `source_file'.
  (let* ((const-declaration (treesit-parent-until
                             node
                             (lambda (item)
                               (string-equal (treesit-node-type item) "const_declaration"))
                             t))
         (const-declaration-parent (treesit-node-parent const-declaration)))
    (string-equal (treesit-node-type const-declaration-parent) "source_file")))

(defun my/go-ts-const-spec-node-name (node)
  "Returns the name of a `const_spec' node"
  (let ((const-name (treesit-node-child-by-field-name node "name")))
    (treesit-node-text const-name)))

(defun my/go-ts-var-spec-node-name (node)
  "Returns the name of a `var_spec' node"
  (let ((var-name (treesit-node-child-by-field-name node "name")))
    (treesit-node-text var-name)))

(defun my/go-ts-import-node-name (node)
  "Returns the name of a `import_spec' node"
  (let* ((import-path (treesit-node-child-by-field-name node "path"))
         (import-alias (treesit-node-child-by-field-name node "name"))
         (package-name (treesit-search-subtree import-path "interpreted_string_literal_content")))
    (cond
     (import-alias
      (format "%s (%s)" (treesit-node-text package-name) (treesit-node-text import-alias)))
      (t (treesit-node-text package-name)))))

(defun my/treesit-simple-imenu-settings ()
  (setq-local imenu-max-item-length 200)
  (setq-local imenu-index-create-function #'treesit-simple-imenu)
  (setq-local treesit-simple-imenu-settings
              '(("Imports" "\\`import_spec\\'" nil my/go-ts-import-node-name)
                ("Functions" "\\`function_declaration\\'" nil nil)
                ("Constants" "\\`const_spec\\'" my/go-ts-const-is-global-p my/go-ts-const-spec-node-name)
                ("Variables" "\\`var_spec\\'" my/go-ts-var-is-global-p my/go-ts-var-spec-node-name)
                ("Interfaces" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                ("Types" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                ("Aliases" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                ("Structs" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                ("Methods" "\\`method_declaration\\'" nil nil)))

  (setq-local consult-imenu-config
              '((go-ts-mode :toplevel "Imports"
                            :types ((?I "Imports"    font-lock-constant-face)
                                    (?f "Functions"  font-lock-function-name-face)
                                    (?c "Constants"  font-lock-constant-face)
                                    (?v "Variables"  font-lock-variable-name-face)
                                    (?i "Interfaces" font-lock-type-face)
                                    (?t "Types"      font-lock-type-face)
                                    (?a "Aliases"    font-lock-type-face)
                                    (?s "Structs"    font-lock-type-face)
                                    (?m "Methods"    font-lock-function-name-face))))))
```

What is left now is to add a hook for `go-ts-mode`, which configures everything for us.

My updated `go-ts-mode` config looks like this.

``` emacs-lisp
(use-package go-ts-mode
  :ensure t
  :config
  (add-hook 'go-ts-mode-hook #'my/treesit-simple-imenu-settings)
  (add-hook 'go-ts-mode-hook #'company-mode))
```

Here's how it looks like with the sample Go program shown before.

[![]({{ site.baseurl }}/images/consult-imenu-treesit.png)]({{ site.baseurl }}/images/consult-imenu-treesit.png){:.glightbox}

Things look much better now. We have various categories such as `Imports`,
`Functions`, `Variables`, etc. And the index generation happens much faster now,
because it relies on `tree-sitter` only.
