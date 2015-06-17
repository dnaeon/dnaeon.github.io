---
layout: post
title: Introduction to writing plugins for libpkg in FreeBSD
tags: freebsd programming
---
Soon after the
[1.0 release announcement of
pkgng](http://lists.freebsd.org/pipermail/freebsd-ports/2012-August/077909.html),
the development branch of
[pkgng](http://wiki.freebsd.org/pkgng) got support for plugins.

Plugins are being used for extending *pkgng*'s functionallity allowing
you to write a custom code which hooks into *libpkg*.

Another use of plugins is the ability to write new commands for the
frontend.

In this handbook we will see how plugins work and how to write a few
simple plugins for *pkgng*.

At the end of this handbook you can find links to example pkgng
plugins and some demos of pkgng plugins in action.

## Requirements

* Basic programming skills

## How plugins work inside pkgng

Here I will add a few short notes on how plugins work inside *libpkg*.

In general they can be summarized in these simple steps:

* Plugins discovery
* Plugins initialization
* Plugins registration and hooking
* Exposing libpkg to plugins
* Plugins shutdown

In the next sections of this handbook we discuss about each of the
above.

## Plugins discovery

The first thing we need in order to get plugins working in *pkgng* is
that we have a way to discover them.

Plugins are being discovered by *pkgng* during initialization and each
valid plugin is being loaded as a dynamic shared library.

So how does plugins discovery work?

Upon initialization *pkgng* will try to load the plugins defined by
the *PLUGINS* configuration option.

The *PLUGINS* configuration option is a comma-separated list, which
specifies the plugins we want to have loaded.

Example of setting the *PLUGINS* option in *pkg.conf(5)* would look
like this:

```text
PLUGINS : [commands/mystats, zfssnap]
```

This would load the *mystats* plugins which provides a new *pkg
mystats* command and the *zfssnap* plugin for creating ZFS snapshots
before any install/deinstall actions are taken.

As mentioned in the beginning of this handbook, plugins a simply a
shared libraries and reside in the directory pointed by the
*PKG_PLUGINS_DIR* directory, which by default is set to
*/usr/local/lib/pkg/* directory.

It is important to mention that only the plugins specified by the
*PLUGINS* configuration option will be loaded. With that being said
this means that if you just place a file in *PKG_PLUGINS_DIR*
directory, that plugin will *not* be loaded until you add it to the
*PLUGINS* list.

And that is how simple the plugins discover in *pkgng* is - just add
drop your plugin to *PKG_PLUGINS_DIR* directory and add the plugin to
the *PLUGINS* option in order to load the plugin and make it available
for use.

## Plugins initialization

Discovering the plugins by *pkgng* and loading them is just the first
step.

Once a plugin is loaded *libpkg* will ask for the plugin to initialize
itself.

During the plugin initialization step a plugin generally does two
things:

* Registering it's metadata information - plugin name, version and short description
* Registering a hook into the library

Registering a hook into the library is not required by all
plugins. For example plugins that provide new commands to the *pkg*
frontend are not registering hooks into the library. Such example
plugin is the [command-mystats
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/command-mystats).

Other plugins which directly hook into *libpkg* must register a
hook. Such plugins are being executed when a certain condition occurs
- like for example executing a plugin before any install/deinstall
actions are taken.

These are the two required steps a plugin must perform during
initialization. Of course other initialization steps could be taken,
if for example the plugin requires it - like for example loading a
plugin specific configuration file. See the [zfssnap
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/zfssnap)
for such an example.

Enough theory, let's look at some code now!

In order to get a plugin initialized *libpkg* will look for a function
with specific name provided by the plugin which does the
initialization. That function is called *init()*.

The plugin's init function has the following prototype:
	
```c
int init(struct pkg_plugin *p);
```
	
The plugin's init function should also take care of returning proper
return values, where *EPKG_OK* (0) means successful initialization and
*EPKG_FATAL* ( > 0 ) means failure during initialization. Plugins
which failed to initialize will not be loaded.

Below you can see the definition of the [stats
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/stats)
*init()* function, which performs the initialization of the [stats
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/stats) and
returns *EPKG_OK* or *EPKG_FATAL* depending on whether the plugin
initialized successfully or not.

```c
/*
 * stats plugin initialization
 */
int
init(struct pkg_plugin *p)
{
        /*
         * Hook into the library and provide package stats for the following actions:
         *
         * - pre-install 
         * - post-install
         * - pre-deinstall
         * - post-deinstall
         */
        pkg_plugin_set(p, PKG_PLUGIN_NAME, name);
        pkg_plugin_set(p, PKG_PLUGIN_DESC, description);
        pkg_plugin_set(p, PKG_PLUGIN_VERSION, version);

        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_PRE_INSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }

        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_POST_INSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }
        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_PRE_DEINSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }
        
        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_POST_DEINSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }
        
        return (EPKG_OK);
}
```
	
Don't worry about the *pkg_plugin_hook_register()* function if it
doesn't ring any bell to you for now as we'll be discussing it a bit
later in the next section of the handbook.

In the next section we take a closer look at the *init()* functions
and how to register a hook into *libpkg*.

## Plugins registration and hooking

In the plugin's *init()* function we perform any internal
initialization of the plugin and also registering a hook into the
library, so that *libpkg* can trigger an execution of our plugin upon
certain event.

This is where we actually *hook* into the library using the
*pkg_plugin_hook_register()* and providing a callback function which
will be called by *libpkg*.

The *pkg_plugin_hook_register()* function prototype can be seen below:

```c
int pkg_plugin_hook_register(struct pkg_plugin *p, pkg_plugin_hook_t hook, pkg_plugin_callback callback);
```

And here's a short description of the argument it expects.

Argument *p*:

* This is a *struct pkg_plugin ** object that is passed by the library to the plugin itself
* Used for referencing the plugin itself

Argument *hook*:

* The *hook* argument specifies at what event/time our plugin will be triggered

Valid values for the *hook* argument and a short description follows
below:

* *PKG_PLUGINS_HOOK_PRE_INSTALL* - plugin will be triggered *prior* any install actions taken by the library
* *PKG_PLUGINS_HOOK_POST_INSTALL* - plugin will be triggered *after* any install actions were taken by the library
* *PKG_PLUGINS_HOOK_PRE_DEINSTALL* - plugin will be triggered *prior* any deinstall actions taken by the library
* *PKG_PLUGINS_HOOK_POST_DEINSTALL* - plugin will be triggered *after* any deinstall actions were taken by the library
* *PKG_PLUGINS_HOOK_PRE_FETCH* - plugin will be triggered *prior* any package fetching 
* *PKG_PLUGINS_HOOK_POST_FETCH* - plugin will be triggered *after* any package fetching

Argument *callback*:

* Is the plugin's callback function called by the library
* Is the plugin's function which performs the real work

Looking into the [stats
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/stats)
plugin *init()* function from the previus chapter of the handbook we
will now look into the different parts of the plugin's initialization.

So, here's the initialization function for the *stats plugin*:

```c
/*
 * stats plugin initialization
 */
int
init(struct pkg_plugin *p)
{
        /*
         * Hook into the library and provide package stats for the following actions:
         *
         * - pre-install 
         * - post-install
         * - pre-deinstall
         * - post-deinstall
         */
        pkg_plugin_set(p, PKG_PLUGIN_NAME, name);
        pkg_plugin_set(p, PKG_PLUGIN_DESC, description);
        pkg_plugin_set(p, PKG_PLUGIN_VERSION, version);

        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_PRE_INSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }

        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_POST_INSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }
        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_PRE_DEINSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }
        
        if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_POST_DEINSTALL, &plugin_stats_callback) != EPKG_OK) {
                fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
                return (EPKG_FATAL);
        }
        
        return (EPKG_OK);
}
```

Now, we will separate this init function into two sections, just like
we've mentioned in the beginning of this handbook.

If you remember correctly the plugin's init function is responsible
for two things:

* Registering the plugin's metadata information - plugin name, version and short description
* Registering a hook into the library

Now looking into the first part of the init function we see the plugin
registering it's metadata information (name, version and short
description) is done by this code here:

```c
pkg_plugin_set(p, PKG_PLUGIN_NAME, name);
pkg_plugin_set(p, PKG_PLUGIN_DESC, description);
pkg_plugin_set(p, PKG_PLUGIN_VERSION, version);
```

The second part of the plugin's init function is the place where we
hook into the library and provide a callback function for performing
the real work of the plugin:

```c
if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_PRE_INSTALL, &plugin_stats_callback) != EPKG_OK) {
       fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
               return (EPKG_FATAL);
}

if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_POST_INSTALL, &plugin_stats_callback) != EPKG_OK) {
       fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
               return (EPKG_FATAL);
}

if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_PRE_DEINSTALL, &plugin_stats_callback) != EPKG_OK) {
       fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
              return (EPKG_FATAL);
}
        
if (pkg_plugin_hook_register(p, PKG_PLUGIN_HOOK_POST_DEINSTALL, &plugin_stats_callback) != EPKG_OK) {
       fprintf(stderr, "Plugin '%s' failed to hook into the library\n", PLUGIN_NAME);
              return (EPKG_FATAL);
}
```

As you can see here we have registered four hooks into the library
with a callback function named *plugin_stats_callback()* which will be
called *prior* any install/deinstall and *after* any install/deinstall
actions are taken.

More about the callback functions and its interaction with *libpkg* in
the next section of the handbook.

## Exposing libpkg to plugins

In order to have plugins that do something really useful we need to
expose some of the capabilities of *libpkg* back to plugins.

The *callback* function prototype should have the following prototype:

```c
int plugin_callback(void *data, struct pkgdb *db);
```
	
In the above prototype *data* contains any data passed to the plugin
by the library. Proper casting of *data* to the correct type should be
done by the plugin itself, depending where a plugin actually hooks
in. The *db* argument contains a database pointer which can be used to
access the *pkg database* and perform any operations required by the
plugin to do it's job.

In this chapter of the handbook we will define our callback function
for the *stats plugin*.

And below follows the definition of the *plugin_stats_callback()*
function:

```c
int
plugin_stats_callback(void *data, struct pkgdb *db)
{
        int64_t flatsize = 0;
        char size[7];

        assert(db != NULL);
        /* assert(data != NULL); */

        flatsize = pkgdb_stats(db, PKG_STATS_LOCAL_SIZE);
        humanize_number(size, sizeof(flatsize), flatsize, "B", HN_AUTOSCALE, 0);
        printf(">>> Installed packages : %" PRId64 " | Disk space: %s <<<\n",
                                      pkgdb_stats(db, PKG_STATS_LOCAL_COUNT),
                                      size);

        return (EPKG_OK);
}
```

And that is all. When *libpkg* starts installing/deinstalling packages
our plugin will get executed *before* and *after* the actual process
of installing/deinstalling packages in order to provide us with some
package statistics.

The callback function should also take care of returning proper codes
to the library - *EPKG_OK* (0) on success and *EPKG_FATAL* ( > 0 ) on
failure.

## Plugins shutdown

Similar to the initialization phase of our plugin you should also
provide a shutdown function for the plugin which is executed right
before the plugin is unloaded by *libpkg*.

Having a shutdown function is something we use for things like -
free()'ing allocated memory, closing files and anything you can think
of a proper shutdown procedure should contain.

The plugin's shutdown function prototype is as follows:
	
```c
int pkg_plugins_shutdown_stats(struct pkg_plugin *p);
```
	
And here's the definition of the *shutdown()* function used by the
*stats plugin*:

```c
int
pkg_plugins_shutdown_stats(void)
{
	/* perform shutdown procedure here if needed */
	return (EPKG_OK);
}
```

The plugin's shutdown function should also take care of returning
proper codes to the library - *EPKG_OK* (0) on success and
*EPKG_FATAL* ( > 0 ) on failure.

As the *stats plugin* is a simple one it does not perform any real
shutdown actions. You may want to have a look at the existing [zfssnap
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/zfssnap)
for example shutdown function performing a real shutdown procedure.

## Plugin Applications

As this is just a short introduction to writing plugins for pkgng and
cannot cover all the possible plugin applications, here I'd like just
to mention what possible applications of plugins could be used.

Plugins could be used for writing new transfer protocols for pkgng,
for example we could have a custom package fetcher supporting *rsync*,
*ssh* or other protocols which are not supported natively by
pkgng. Such plugins should be generally hooking into
*PKG_PLUGINS_HOOK_PRE_FETCH* allowing them to perform the fetching.

We could also have plugins which could perform some post-actions like
for example when a package repository is created a plugin starts
serving the repository over HTTP to the clients.

Plugins would allow for extending pkgng's capabilities a lot and in
general writing a plugin is just a matter of using your imagination
and innovation skills

So go ahead and write your new plugin plugin for *pkgng*! :)

## Example pkgng plugins and demos

Here I will just add a few notes about example pkgng plugins and see
them in action.

In order to view the installed plugins on your pkgng-aware system you
should use the `pkg plugins` command:

```bash
% pkg plugins
NAME       DESC                                          VERSION   
mystats    A plugin to display package statistics        1.0.0     
serve      A http plugin for serving files               1.0.0     
zfssnap    ZFS snapshot plugin                           1.0.0     
stats      Plugin for displaying package stats           1.0.0   
```

[stats
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/stats) is a
plugin for displaying package stats during install/deinstall process.

Here's an output of it in action while installing a package:

```bash
% sudo pkg install apg
The following packages will be installed:

	Installing apg: 2.3.0b_2

The installation will require 245 kB more space

0 B to be downloaded

Proceed with installing packages [y/N]: y
>>> Triggering execution of plugin 'stats'
>>> Installed packages : 906 | Disk space: 6212 MB <<<
Checking integrity... done
Installing apg-2.3.0b_2... done
>>> Triggering execution of plugin 'stats'
>>> Installed packages : 907 | Disk space: 6213 MB <<<
```

As you can see from the above output *libpkg* is triggering our plugin
and we get useful stats for our database while *pkgng* is running.

[zfssnap
plugin](https://github.com/pkgng/pkgng/tree/master/plugins/zfssnap) is
a plugin for creating ZFS snapshots on your system *before* doing any
install/deinstall actions, so in case something goes wrong after the
install/deinstall process you can easily rollback to a previously
known and working state.

Here's an output of the plugin in action:

```bash
% sudo pkg install bash
Updating repository catalogue
Repository catalogue is up-to-date, no need to fetch fresh copy
The following packages will be installed:

	Installing bash: 4.2.28

The installation will require 4 MB more space

0 B to be downloaded

Proceed with installing packages [y/N]: y
>>> Triggering execution of plugin 'zfssnap'
>>> Creating ZFS snapshot
Checking integrity... done
Installing bash-4.2.28... done
```

What happens is before we install any packages we've created a ZFS
snapshot on our system, which makes it really useful in case something
goes wrong and we need to rollback later.
