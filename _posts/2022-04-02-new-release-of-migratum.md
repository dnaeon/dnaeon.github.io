---
layout: post
title: New release of cl-migratum available
tags: common-lisp lisp database schema migration
---
A new version of [cl-migratum](https://github.com/dnaeon/cl-migratum) has been
released.

[![]({{ site.baseurl }}/images/migratum-demo.gif)]({{ site.baseurl }}/images/migratum-demo.gif){:.glightbox}

This release comes with some interesting new features, a summary of
which you can find below.

* New database driver `migratum.driver.rdbms-postgresql` has been
  contributed. Driver is based on `hu.dwim.rdbms`.
* New migration resource kind has been implemented - `:lisp`. This
  migration resource allows you to invoke a regular Lisp function as
  part of the upgrade/downgrade process. With it you can handle more
  complex migrations from Lisp itself, rather than relying on SQL
  only.
* New system for driver mixins has been committed -
  `cl-migratum.driver.mixins`. The new `:lisp` migration kind is one
  of the mixins provided by the system.
* CLI application based on `clingon` and the core `migratum` systems
  has been created.
* The core system has been refactored.

There are breaking changes with this release of `cl-migratum`, so make
sure that you check the documentation and the
[changelog](https://github.com/dnaeon/cl-migratum/blob/master/CHANGELOG.org)
for more details.
