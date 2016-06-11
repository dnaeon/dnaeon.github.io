---
layout: post
title: Orchestration made easy with Gru v0.2.0
tags: golang programming orchestration configuration mgmt gru
---
For the past few months I've continued working on
[my first Go project](https://github.com/dnaeon/gru) and overall
I am very pleased with how the project evolved as a whole.

Over this time a lot of improvements have made into the project, and
here are just some of them.

The new version of Gru has introduced `resources` as a way to declare
and execute [idempotent](https://en.wikipedia.org/wiki/Idempotence)
operations, making the project better suited for things such as
configuration management as well.

Gru has also adopted [HCL](https://github.com/hashicorp/hcl) as the
configuration language, which is used to express resources.
Resources can now be grouped together into modules, which allows for
code re-use by making it possible modules to import other modules.

This is how a
[package resource](https://github.com/dnaeon/gru/blob/master/docs/resources/package.md)
can be expressed in HCL for example.

```hcl
// Installs the tmux package
package "tmux" {
  state = "present"
}
```

Using [DAG graphs](https://en.wikipedia.org/wiki/Directed_acyclic_graph)
we can now have relationship between resources, allowing us to
express what the resource evaluation order should be.

```hcl
file "/home/dnaeon/.tmux.conf" {
  state = "present"
  mode = 0644
  source = "data/tmux/tmux.conf"
  require = [
    "package[tmux]",
  ]
}
```

Once we do a
[topological sorting](https://en.wikipedia.org/wiki/Topological_sorting) of the
resource graph nodes, we can evaluate and process our resources in the
correctly specified order, or detect circular dependencies in our
resources.

Another new feature which was introduced in version 0.2.0 of Gru is the
[site repo](https://github.com/dnaeon/gru/blob/master/docs/quickstart.md#setting-up-the-site-repo),
which basically allows the remote minions to sync modules and data
files from upstream Git repository. Each branch from the `site repo`
also represents an `environment`, which minions can checkout and use
during the `catalog` processing.

Having support for different environments, means that each minion
can track different branch from the site repo, and also makes
testing of new features and modules much easier, since you can push an
environment to just specific minions, while the rest of them
continue to track the `production` environment.

With this release we can also make use of some core resources, such as
`service`, `file`, `shell`, `pacman`, `yum` and others.

I've also finally caught up on actually writing some documentation
about Gru, so now you will be able to find the
[Gru Quickstart Guide](https://github.com/dnaeon/gru/blob/master/docs/quickstart.md),
which will walk you through your first steps with Gru.

The latest Gru documentation you can find
[here](https://github.com/dnaeon/gru/tree/master/docs) and
you can also see how a module in Gru looks like expressed in HCL at the
[example site repo for Gru](https://github.com/dnaeon/gru/tree/master/site).

Finally, I'd like to say that when I first started working on this
project I had no idea how it will evolve, but over the time new
features have been implemented, fixes have been made as well, and
overall I had the chance to cover a lot of ground using the
Go programming language, which made this whole experience working on
this project so much fun!
