---
layout: post
title: Continuous Integration with Jenkins on FreeBSD
tags: jenkins ci freebsd
---

In this post we will see how to install and configure
[Jenkins CI](https://jenkins-ci.org) on FreeBSD.

So, what is Jenkins?

Here's just a short description of what Jenkins CI is and for what
purposes you might want to consider using it.

Jenkins provides a [Continuous Integration System](http://en.wikipedia.org/wiki/Continuous_integration),
which makes the whole process of integrating changes to a project
much easier for developers.

Thinking of a software project where each developer submits changes,
the more the developers the higher the chance is of someone
breaking the build at some point.

Jenkins can help you overcome this problem with building and testing
software projects continuously, and help you discover failures quickly
as they happen.

Besides just building and testing a project you can use Jenkins in
virtually anything you can think of.

Here are just a few examples of what you can use Jenkins for:

* Building and testing of a software project
* Monitoring executions of externally-run jobs
* Package building server - continuous building of packages for your GNU/Linux or BSD systems
* Deployment tool - deploy quckly changes as soon as they are verified and confirmed to your systems
* etc

In this handbook we will start off with the installation and
configuration of Jenkins and later we will see how to create a few
example projects.

For more information on Jenkins CI and Continuous Integration,
please check the links below:

* [Official site of Jenkins](https://wiki.jenkins-ci.org/)
* [Continuous Integration](http://en.wikipedia.org/wiki/Continuous_integration)

## Requirements

* root access or sudo rights

## Tested And Verified

The setup explained in this handbook has been tested and verified on:

* FreeBSD 9.0 system
* jenkins-1.472

## Installation of Jenkins

To install Jenkins using the
[FreeBSD Ports Collection](http://www.freebsd.org/doc/handbook/ports.html)
execute the command below:

```bash
$ sudo make -C /usr/ports/devel/jenkins make install clean
```

To install Jenkins using the
[FreeBSD's pkgng](http://wiki.freebsd.org/pkgng/) execute the command below:

```bash
$ sudo pkg install -y jenkins
```

Once you've got Jenkins installed, we can continue to the next step and configure it.

## Enable Jenkins during boot-time

First let's enable Jenkins, so that it starts during boot-time.

To do that add the following line to your `/etc/rc.conf` file:

```bash
jenkins_enable="YES"
```

## Starting Jenkins

Now let's start Jenkins CI for the first time:

```bash
$ sudo /usr/local/etc/rc.d/jenkins start
```

## Configuration of Jenkins

Once Jenkins is fully operational it will start listening on port 8180.

To check your new Jenkins CI instance open up a browser
to your system on port 8180, e.g. `http://localhost:8180/jenkins/`.

Jenkins is fully configurable from its web interface. From now on
everything we need to configure on Jenkins is going to be done using
the web interface.

In order to configure your Jenkins CI instance you need to go to
`Manage Jenkins -> Configure System`.

Every configuration setting has a question mark next to it's right
explaining what a setting does, so whenever in doubt check the help
messages.

Here we are going to do just a basic configuration of Jenkins, for a
more detailed configuration examples please refer to the
[official web site of Jenkins](https://jenkins-ci.org) and the
[Jenkins Wiki page](https://wiki.jenkins-ci.org/display/JENKINS/Use+Jenkins)
for advanced configuration settings.

![_config.yml]({{ site.baseurl }}/images/jenkins-main-config.png)

On the above screenshot you can see that we've configured the
`System Message` and `number of executors` Jenkins will use during builds.

Now let's secure a bit our Jenkins instance:

![_config.yml]({{ site.baseurl }}/images/jenkins-security.png)

As you can see from the above screenshot we've configured one user
with full administrator access to the Jenkins instance and we've
configured access for the anonymous users.

For more information on securing your Jenkins CI instance, please
refer to the [Securing Jenkins](https://wiki.jenkins-ci.org/display/JENKINS/Securing+Jenkins)
documentation.

And the last thing we are going to configure is the `E-mail Notifications`,
so that we get notification status about the builds.

![_config.yml]({{ site.baseurl }}/images/jenkins-mail.png)

As mentioned earlier this is just a basic configuration of our
Jenkins CI instance and you are also advised to play a bit with the
configuration options and check the online documentation as well for
more information about Jenkins.

## Setting up a Jenkins job for building a software project

Lets create now a Jenkins project that will build and test a real-world
project. For the purposes of this example we will be building and
testing the [FreeBSD's pkgng project](http://wiki.freebsd.org/pkgng/).

First lets start with installing a few plugins for Jenkins.

We will need the following plugins installed before being able to
build our example project.

* [Git Plugin](https://wiki.jenkins-ci.org/display/JENKINS/Git+Plugin)
* [Github Plugin](https://wiki.jenkins-ci.org/display/JENKINS/Github+Plugin)
* [Clone Workspace SCM Plugin](https://wiki.jenkins-ci.org/display/JENKINS/Clone+Workspace+SCM+Plugin)

In order to install the needed plugins navigate through your Jenkins CI
instance to `Manage Jenkins -> Manage Plugins -> Available` and
select the above plugins, then click on the `Install` button.

Once the above plugins have been installed we need to configure the
Git plugin, so navigate to `Manage Jenkins -> Configure System` and
scroll down to the `Git` section

If you see the `There's no such executable git in PATH: /sbin, /bin, /usr/sbin, /usr/bin.`
like on the screenshot below you will need to update your path to the Git executable:

![_config.yml]({{ site.baseurl }}/images/jenkins-git-config.png)

As with FreeBSD any software installed that is not part of the base
system gets installed in `/usr/local`, so you need to update the path
to the Git executable to `/usr/local/bin/git`.

![_config.yml]({{ site.baseurl }}/images/jenkins-git-config-fixed.png)

Now that we have the plugins installed and configured, lets create our
first Jenkins job. To do that navigate to `New Job`, give the job a name
and select the `Build a free-style software project`, then press the
`OK` button.

## Setting up an upstream project in Jenkins

A few things to note here is that our Jenkins job will be
configured in the following way:

1. One upstream project that will poll the remote Git repository every 30 minutes
2. If there are new changes made in the remote Git repository we pull those changes
3. Archive the Git repository so that downstream projects can clone it and then build it
4. Trigger a build on the downstream projects
5. Downstream projects will be building the project using GCC and LLVM Clang

In other words our upstream project is the one that triggers a
build on the downstream projects whenever a new change to Git
repository is made.

Once you create a new job in Jenkins you need to configure it.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-job-config-1st.png)

On the above screenshot we have configured the following things:

* Description of the job
* We want to keep max of 100 builds for the project
* The project link at Github

The next thing we need to configure is `Source Code Management`.
This is where we define the remote Git repository,
the branches we want to build and the repository browser.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-job-config-2nd.png)

Now, lets continue to the `Build Triggers`. The `Build Triggers` tell
Jenkins when and how a build is being scheduled.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-job-config-3rd.png)

In the screenshot above we tell Jenkins to poll the remote Git
repository every 30 minutes for new changes.

One last thing that we are going to do is to prepare the repository
for cloning on the downstream projects as well.

This saves the time needed for doing a clone of the Git repository
and also makes sure that all builds we make are done from a clean state,
meaning a full rebuild of project itself.

To do that we need to select `Archive for Clone Workspace SCM` in the
`Post-build Actions` section as shown in the screenshot below:

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-gcc-clone-workspace.png)

And that was it with the configuration of our `upstream project` in
Jenkins. Now we can configure our `downstream projects` which will
take care of building `pkgng` with GCC and LLVM Clang.

## Setting up downstream projects in Jenkins for FreeBSD's pkgng

Just like in the previous step where we've configured the
`upstream pkgng project` in Jenkins we will now configure a
`downstream project` for pkgng* for building the project
using GCC and LLVM Clang.

Navigate to `New Job -> Build a free-style software project` and
use a project name `pkgng-gcc`.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-gcc-downstream.png)

Now we will add the downstream project description,
max number of builds and Github project just like we did in the
previous step.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-gcc-downstream-description.png)

Next we do the configuration in the `Source Code Management` section.
Here we are going to use the workspace of the `upstream pkgng`
project which we've already archived and now we can use for building.

In `Source Code Management` select `Clone Workspace` and choose
`pkgng` as the `Parent Project` as shown in the screenshot below:

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-gcc-clone-workspace.png)

In the `Build Triggers` section we will configure that our project
gets build after the upstream `pkgng` project is built.

This creates the relation between the upstream and downstream
projects in Jenkins.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-gcc-downstream-build-triggers-after.png)

Now it is time to define the commands used to perform the actual build
of the project and that is defined in the `Build` section.

To do that in the `Build` section we select `Execute shell`
option define the commands used to build the project as shown in the
screenshot below.

![_config.yml]({{ site.baseurl }}/images/jenkins-pkgng-gcc-downstream-build-section.png)

To build the project we just add one line:

```bash
env CC=gcc make
```

Now whenever a new change being pushed to the remote Git
repository of `pkgng` this will trigger a new build of the project
using GCC on the downstream project.

In order to create another downstream project using LLVM Clang you can
follow the steps of this section, and the only change you
need to do is to replace the command for building the project with the
one below:

```bash
env CC=clang make
```

In order to test that everything works as expected just go ahead and
trigger a build on the `upstream pkgng` project and monitor the
result in it's `downstream projects` for GCC and LLVM Clang.

## Further reading

As mentioned in the beginning in this post this is just a quick and
dirty introduction to Continuous Integration with Jenkins under FreeBSD.

You are also advised to check the online documentation about Jenkins
and Continuous Integration on the topic.

Jenkins also provides lots of ready-to-use plugins which extend
further it's functionality. You can check the
[available plugins for Jenkins here](https://wiki.jenkins-ci.org/display/JENKINS/Plugins)
and install them as needed.
