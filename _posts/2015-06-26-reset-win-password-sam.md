---
layout: post
title: Recover SAM password for Windows from GNU/Linux
tags: linux windows
---
Security Account Manager (SAM) in Windows is used to store
users' passwords and can be used to authenticate local users on your
Windows systems.

This post is about recovering your account password from
Windows SAM by using a GNU/Linux system for the task.

In the cases when you happen to forget the Administrator password
for your Windows server this could be really handy to keep around.

Today was such a day for me, so I thought I should document this
somewhere for future references if needed.

We will be using an [Arch Linux](https://www.archlinux.org/) ISO
image to boot the system and then make our way to the SAM password
recovery.

You can read more about SAM at the
[Security Account Manager](https://en.wikipedia.org/wiki/Security_Account_Manager)
page on Wikipedia.

Lets get started!

First, go ahead to the [Arch Linux](https://www.archlinux.org/)
site and grab an ISO image.

Then boot your system using the Arch Linux image you've downloaded,
which should soon take you to the shell prompt.

For recovering the password from SAM we will be using the
[chntpw](https://en.wikipedia.org/wiki/Chntpw) tool, so in order to be
able to install the package we would need networking first.

The commands below are used to assign a static address to one of our
ethernet interfaces, but you could use DHCP instead if you happen
to have a running DHCP server in your subnet already.

```bash
ip addr add <address>/<mask> broadcast <broadcast> dev <interface>
ip route add default via <gateway>
echo 'nameserver <nameserver>' >> /etc/resolv.conf
```

Make sure that you have a working network connection then
proceed to the next steps.

Synchronize your package database.

```bash
pacman -Syy
```

Now lets install `ntchpw`.

```bash
pacman -S chntpw
```

It is now time to mount the Windows drive. The command below
assumes that your `Windows C:\` drive is at `/dev/sda1`, but it
might not be the case with your setup. Check `fdisk(8)` and `lsblk(8)`
to see which is the correct device of your Windows systems.

```bash
mount /dev/sda1 /mnt
```

The Windows SAM file location by default is at
`C:\Windows\System32\config`, so lets navigate to that directory first.

```bash
cd /mnt/Windows/System32/config
```

List the local users from the SAM file by executing the command below.

```bash
chntpw -l SAM
```

Select the user you wish to reset/unlock and run the following
command.

```bash
chntpw -u <user> SAM
```

From there on simply follow the menu instructions provided by
`chntpw` and you should be ready to go.
