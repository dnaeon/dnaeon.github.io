---
layout: post
title: New release of clingon available
tags: common-lisp lisp getopt command-line options parser
---
A new version of [clingon](https://github.com/dnaeon/clingon) has been
released.

This minor release provides the following new features.

* Added support for grouping related options into same
  _category_. Useful when you have many options and want to organize
  them better.
* New option kind has been added - `:list/filepath`. Allows specifying
  multiple paths on the command-line when using Zsh completions.
* Added support for hierarchical `pre-` and `post-` hooks for
  commands. Typical use case for these would be to initialize things
  based on global options, or tear things down on shutdown,
  e.g. setting up the logging level based on a global option.

Here's an example of a CLI app's usage info built with `clingon` which
contains option categories for better organization.

``` shell
NAME:
  migratum - migration tool

USAGE:
  migratum [global-options] [<command>] [command-options] [arguments ...]

OPTIONS:
      --help                         display usage information and exit
      --version                      display version and exit
  -d, --driver <VARIANT>             migration driver to use [env: $MIGRATUM_DRIVER] [choices: dbi,
                                     rdbms-pgsql]
  -l, --log-level <VARIANT>          log level [default: info] [env: $MIGRATUM_LOG_LEVEL] [choices: off,
                                     info, debug]
  -p, --provider <VARIANT>           migration resource provider to use [env: $MIGRATUM_PROVIDER] [choices:
                                     local-path]

DBI driver options:
      --dbi-db-host <VALUE>          database host [default: localhost] [env: $MIGRATUM_DBI_DB_HOST]
      --dbi-db-kind <VARIANT>        database kind [env: $MIGRATUM_DBI_DB_KIND] [choices: sqlite3,
                                     postgres, mysql]
      --dbi-db-name <VALUE>          database name [env: $MIGRATUM_DBI_DB_NAME]
      --dbi-db-pass <VALUE>          database password [env: $MIGRATUM_DBI_DB_PASSWORD]
      --dbi-db-port <INT>            database port [env: $MIGRATUM_DBI_DB_PORT]
      --dbi-db-user <VALUE>          database username [env: $MIGRATUM_DBI_USER]

RDBMS-PGSQL driver options:
      --rdbms-pgsql-db-host <VALUE>  database host [default: localhost] [env: $PGHOST]
      --rdbms-pgsql-db-name <VALUE>  database name [env: $PGDATABASE]
      --rdbms-pgsql-db-pass <VALUE>  database password [env: $PGPASSWORD]
      --rdbms-pgsql-db-port <INT>    database port [default: 5432] [env: $PGPORT]
      --rdbms-pgsql-db-user <VALUE>  database username [env: $PGUSER]

local-path provider options:
      --resources <PATH>             migration resources path [env: $MIGRATUM_LP_RESOURCES]

COMMANDS:
  applied           list applied migrations
  create, new       create new migration
  migrate           apply pending migrations
  pending           list pending migrations
  print-doc         print the documentation
  reset             revert all migrations and re-apply them
  revert, rollback  revert latest applied migration(s)
  status            get status info
  zsh-completions   generate the Zsh completions script

AUTHORS:
  Marin Atanasov Nikolov <dnaeon@gmail.com>

LICENSE:
  BSD 2-Clause
```

The `clingon` system version has been bumped to `v0.3.5`.

[![]({{ site.baseurl }}/images/clingon-zsh-completions.gif)]({{ site.baseurl }}/images/clingon-zsh-completions.gif){:.glightbox}

You can read more about it in the
[changelog](https://github.com/dnaeon/clingon/blob/master/CHANGELOG.org)
as well.
