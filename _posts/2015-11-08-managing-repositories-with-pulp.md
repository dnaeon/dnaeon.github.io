---
layout: post
title: Managing repositories with Pulp
tags: linux pulp repository
---
[Pulp](http://www.pulpproject.org/) is a platform for managing
repositories of content, such as software packages, and pushing that
content out to large numbers of consumers.

With Pulp you could easily create and manage local mirrors of upstream
repositories for your systems or create repositories with
custom content for your clients.

In this post we will see how we can use Pulp to create a local
mirror of the upstream [CentOS](https://centos.org/) repositories,
which can be used by our internal client systems.

First, let's setup mirrors of the CentOS 7 `base` and `updates`
repositories. This would give a bare minimum of packages that we can
already provide to our client systems.

```bash
$ sudo pulp-admin rpm repo create \
        --repo-id=centos-7-x86_64-base \
        --description 'CentOS 7 Base Repo' \
        --display-name 'CentOS 7 Base Repo' \
        --feed=http://mirror.centos.org/centos/7/os/x86_64/
```

And now we create a mirror of the `updates` repository.

```bash
$ sudo pulp-admin rpm repo create \
        --repo-id=centos-7-x86_64-updates \
        --description 'CentOS 7 Updates Repo' \
        --display-name 'CentOS 7 Updates Repo' \
        --feed=http://mirror.centos.org/centos/7/updates/x86_64/
```

You might also consider creating mirrors for other repositories as
well, e.g. `extras`, `centosplus`, `cr`, `fasttrack`, etc. For more
information about the available repositories for CentOS, please
refer to the list of
[Available Repositories for CentOS](https://wiki.centos.org/AdditionalResources/Repositories).

Once you are done with setting up all repositories you need,
you should consider creating a schedule for your repositories, so that
you are in sync with the upstream repositories.

The command below creates a schedule for our CentOS 7 `base`
repository to sync with the upstream repository once a day at midnight.

```bash
$ sudo pulp-admin rpm repo sync schedules create \
        --schedule 2015-11-08T00:00:00Z/P1DT \
        --repo-id centos-7-x86_64-base
```

Make sure to create a schedule for your other repositories as well.

Finally we can optionally start the sync from upstream if needed, by
executing the command below.

```bash
$ sudo pulp-admin rpm repo sync run --repo-id centos-7-x86_64-base
```

And that is how easy it is to setup local mirrors of upstream
repositories using [Pulp](http://www.pulpproject.org/).

Also make sure to check the
[official Pulp documentation](http://www.pulpproject.org/docs/) for
more information about Pulp and different recipes that you can use.
