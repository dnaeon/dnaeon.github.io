---
layout: post
title: cl-migratum: Database Schema Migration System for Common Lisp
tags: lisp programming database schema migration
---
Continuing [my Lisp
journey](./2020-01-05-starting-with-common-lisp-in-2020.md) over the
past few months I went ahead and started another side project, so I
could spend more time with Common Lisp.

I often have to deal with some kind of a database as part of my daily
activities, where changes to the database are managed via a process
and require some form of versioning your changes. The idea behind it
is that you can version, track, revert and audit changes made in a
database.  This is what is referred to as [database
migration](https://en.wikipedia.org/wiki/Schema_migration).

Exploring the [Common Lisp ecosystem](http://quickdocs.org) I found a couple of
existing libraries that can manage database migrations, but they were either
too much coupled with SQL databases or lacking good documentation, so I thought
I could fill that gap in the ecosystem by implementing a database migration system in
Common Lisp, which is extensible, documented and well tested.

The migration system should support implementing new extensions in
such a way that would allow it to work even with databases it was not
initially designed to work with -- by doing that we can not only
manage and migrate SQL databases, but also provide support for other
databases when needed, e.g. multi-model, graph databases.

I'm happy to share that this system is now implemented and ready -
[cl-migratum](https://github.com/dnaeon/cl-migratum).  In order to get
better understanding of how `cl-migratum` works, get familiar with
concepts and the APIs I'd suggest that you check the [cl-migratum
documentation](https://github.com/dnaeon/cl-migratum/blob/master/README.md),
which includes many usage examples and code for implementing new
extensions.

You can also read the [initial announcement of cl-migratum at
r/lisp](https://www.reddit.com/r/lisp/comments/g99th7/clmigratum_database_schema_migration_system_for/)
and check this [PR about adding cl-migratum to
Quicklisp](https://github.com/quicklisp/quicklisp-projects/issues/1838).

In order to start migrating your database with `cl-migratum` you can follow
these easy steps. First, load the `cl-migratum` system.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum)
To load "cl-migratum":
  Load 1 ASDF system:
    cl-migratum
; Loading "cl-migratum"

(:CL-MIGRATUM)
```

Create a *provider* for discovering migration resources. The example
below creates a *provider*, which discovers migrations from a local-path.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum.provider.local-path)
To load "cl-migratum.provider.local-path":
  Load 1 ASDF system:
    cl-migratum.provider.local-path
; Loading "cl-migratum.provider.local-path"

(:CL-MIGRATUM.PROVIDER.LOCAL-PATH)
CL-USER> (defparameter *provider*
           (migratum.provider.local-path:make-local-path-provider #P"~/Projects/lisp/cl-migratum/t/migrations/"))
*PROVIDER*
```

Finally, we need a *driver*, which is used to communicate with the
database we will be migrating. The following example uses the `sql`
driver of `cl-migratum`, which can migrate SQLite, MySQL and
PostgreSQL databases.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum.driver.sql)
To load "cl-migratum.driver.sql":
  Load 1 ASDF system:
    cl-migratum.driver.sql
; Loading "cl-migratum.driver.sql"

(:CL-MIGRATUM.DRIVER.SQL)
CL-USER> (defparameter *conn*
           (dbi:connect :sqlite3 :database-name "cl-migratum.db"))
*CONN*
CL-USER> (defparameter *driver*
           (migratum.driver.sql:make-sql-driver *provider* *conn*))
*DRIVER*
```

Once we've got a *provider* and *driver* we can now use the exported
symbols from the `MIGRATUM` package to actually discover, apply, view,
revert migrations, e.g. this is how to display the pending migrations.

``` common-lisp
CL-USER> (migratum:display-pending *driver*)
.-----------------------------------.
|        PENDING MIGRATIONS         |
+----------------+------------------+
| ID             | DESCRIPTION      |
+----------------+------------------+
| 20200421173657 | create_table_foo |
| 20200421173908 | create_table_bar |
| 20200421180337 | create_table_qux |
+----------------+------------------+
| TOTAL          |                3 |
+----------------+------------------+
NIL
```

And we can apply the pending migrations by simply invoking the
`MIGRATUM:APPLY-PENDING` function, e.g.

``` common-lisp
CL-USER> (migratum:apply-pending *driver*)
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-pending) -
  Found 3 pending migration(s) to be applied
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173657 - create_table_foo
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421173908 - create_table_bar
 <INFO> [18:10:14] cl-migratum.core core.lisp (apply-and-register) -
  Applying migration 20200421180337 - create_table_qux
NIL
```

In order to review the applied migrations we can use the
`MIGRATUM:DISPLAY-APPLIED` function, e.g.

``` common-lisp
CL-USER> (migratum:display-applied *driver*)
.---------------------------------------------------------.
|                   APPLIED MIGRATIONS                    |
+----------------+------------------+---------------------+
| ID             | DESCRIPTION      | APPLIED             |
+----------------+------------------+---------------------+
| 20200421180337 | create_table_qux | 2020-04-21 15:17:46 |
| 20200421173908 | create_table_bar | 2020-04-21 15:14:13 |
| 20200421173657 | create_table_foo | 2020-04-21 15:12:52 |
+----------------+------------------+---------------------+
|                | TOTAL            |                   3 |
+----------------+------------------+---------------------+
NIL
```

In order to run the test suite you can evaluate the following
expressions.

``` common-lisp
CL-USER> (ql:quickload :cl-migratum.test)
CL-USER> (asdf:test-system :cl-migratum.test)
```

The `cl-migratum` Common Lisp system amongst the features mentioned in this
post supports other useful bits, such as stepping-through migration
sequences, reverting to a previous state of the schema, etc.

In order to learn more about `cl-migratum`, see more examples or
implement your own extension make sure to check the
[documentation](https://github.com/dnaeon/cl-migratum/blob/master/README.md).
