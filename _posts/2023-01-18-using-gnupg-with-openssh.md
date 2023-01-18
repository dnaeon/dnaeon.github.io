---
layout: post
title: Using GnuPG with OpenSSH
tags: gnupg gpg openssh ssh
---
In this post we will see how to configure and use GnuPG keys for
authentication against remote OpenSSH servers.

It is by no means an introduction or detailed guide on using GnuPG in
general, but rather it focuses only on the steps you need to perform
in order to start using your GPG keys for SSH authentication.

In case you are new to GnuPG, and don't know what it stands for or
what it is being used for, I'd recommend that you visit the following
links and get yourself familiar with it.

- [GnuPG official site](https://gnupg.org/)
- [Arch Linux GnuPG wiki documentation](https://wiki.archlinux.org/title/GnuPG)
- [Debian GNU/Linux GnuPG wiki documentation](https://wiki.debian.org/GnuPG)

The nice thing about using GnuPG keys for SSH authentication is that
it makes key management less painful. You only have to worry about one
tool for key generation and management. If you are already using GnuPG
for encryption or digital signatures, it makes more sense to use GnuPG
for managing your OpenSSH keys as well. You can also transfer your
GnuPG keys to an OpenPGP smartcard (such as the Yubikey) as well, and
not have to worry about someone stealing your keys.

The steps from this post have been tested on an Arch Linux system with
`gpg` version `2.2.20`.

``` shell
> uname -a
Linux nr200p.internal 6.1.6-arch1-1 #1 SMP PREEMPT_DYNAMIC Sat, 14 Jan 2023 13:09:35 +0000 x86_64 GNU/Linux

> gpg --version
gpg (GnuPG) 2.2.40
libgcrypt 1.10.1-unknown
Copyright (C) 2022 g10 Code GmbH
License GNU GPL-3.0-or-later <https://gnu.org/licenses/gpl.html>
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Home: /home/dnaeon/.gnupg
Supported algorithms:
Pubkey: RSA, ELG, DSA, ECDH, ECDSA, EDDSA
Cipher: IDEA, 3DES, CAST5, BLOWFISH, AES, AES192, AES256, TWOFISH,
        CAMELLIA128, CAMELLIA192, CAMELLIA256
Hash: SHA1, RIPEMD160, SHA256, SHA384, SHA512, SHA224
Compression: Uncompressed, ZIP, ZLIB, BZIP2
```

A summary of the steps we need to perform in order to successfully
authenticate against remote SSH servers using our GPG keys is more or
less this.

1. Configure the GnuPG OpenSSH Agent
2. Create your primary PGP key using `gpg --expert --full-gen-key`
3. Create an `Authentication` subkey using `gpg --expert --edit-key <key-id>`
4. Export your `Authentication` subkey in the OpenSSH public key format
5. Distribute the OpenSSH public key to the systems you need to login

Alright, lets start.

First, we will configure the `gpg-agent(1)`. Add the following lines
to your `~/.gnupg/gpg-agent.conf` file.

``` shell
# ~/.gnupg/gpg-agent.conf
enable-ssh-support
pinentry-program /usr/bin/pinentry-curses
default-cache-ttl 3600
default-cache-ttl-ssh 3600
```

In the next step we will configure our `~/.bashrc` (or `~/.zshrc`, or
any other initialization file you use for your shell). Add the
following lines to your shell initialization file in order to
correctly configure the `SSH_AUTH_SOCK` and `GPG_TTY` environment
variables.

``` shell
# ~/.bashrc
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
  export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

export GPG_TTY=$(tty)
gpg-connect-agent updatestartuptty /bye >/dev/null
```

We also need to add the following line to our `~/.ssh/config` file, so
that everytime we use the `ssh(1)` command it will set the correct TTY
for the `pinentry` program.

``` shell
# ~/.ssh/config
Match host * exec "gpg-connect-agent updatestartuptty /bye"
```

That is all the configuration we need for our GnuPG Agent. Now, we can
go ahead and create our primary GPG key. The following output shows
the creation of a new primary GPG key.

``` shell
> gpg --expert --full-gen-key
gpg (GnuPG) 2.2.40; Copyright (C) 2022 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Please select what kind of key you want:
   (1) RSA and RSA (default)
   (2) DSA and Elgamal
   (3) DSA (sign only)
   (4) RSA (sign only)
   (7) DSA (set your own capabilities)
   (8) RSA (set your own capabilities)
   (9) ECC and ECC
  (10) ECC (sign only)
  (11) ECC (set your own capabilities)
  (13) Existing key
  (14) Existing key from card
Your selection? 1
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (3072) 4096
Requested keysize is 4096 bits
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want for the subkey? (3072) 4096
Requested keysize is 4096 bits
Please specify how long the key should be valid.
	 0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0) 1y
Key expires at Thu 18 Jan 2024 09:08:43 AM EET
Is this correct? (y/N) y

GnuPG needs to construct a user ID to identify your key.

Real name: Test User
Email address: test@example.org
Comment: 
You selected this USER-ID:
    "Test User <test@example.org>"

Change (N)ame, (C)omment, (E)mail or (O)kay/(Q)uit? o
We need to generate a lot of random bytes. It is a good idea to perform
some other action (type on the keyboard, move the mouse, utilize the
disks) during the prime generation; this gives the random number
generator a better chance to gain enough entropy.
gpg: directory '/home/dnaeon/.gnupg/openpgp-revocs.d' created
gpg: revocation certificate stored as '/home/dnaeon/.gnupg/openpgp-revocs.d/C6D7CF3AE42593261A7116ADB11582839FE31C64.rev'
public and secret key created and signed.

pub   rsa4096 2023-01-18 [SC] [expires: 2024-01-18]
      C6D7CF3AE42593261A7116ADB11582839FE31C64
uid                      Test User <test@example.org>
sub   rsa4096 2023-01-18 [E] [expires: 2024-01-18]
```

GnuPG will create a primary key and an encryption subkey. Now we need
to create an `Authentication` subkey. In order to do that we need to
edit the key we've just created.

The output below shows the creation of a new `Authentication` subkey.
Pay attentation that we are only assigning the `Authentication`
capability for our subkey.

``` shell
> gpg --expert --edit-key C6D7CF3AE42593261A7116ADB11582839FE31C64
gpg (GnuPG) 2.2.40; Copyright (C) 2022 g10 Code GmbH
This is free software: you are free to change and redistribute it.
There is NO WARRANTY, to the extent permitted by law.

Secret key is available.

gpg: checking the trustdb
gpg: marginals needed: 3  completes needed: 1  trust model: pgp
gpg: depth: 0  valid:   1  signed:   0  trust: 0-, 0q, 0n, 0m, 0f, 1u
gpg: next trustdb check due at 2024-01-18
sec  rsa4096/B11582839FE31C64
     created: 2023-01-18  expires: 2024-01-18  usage: SC  
     trust: ultimate      validity: ultimate
ssb  rsa4096/6807DFF921D6105C
     created: 2023-01-18  expires: 2024-01-18  usage: E   
[ultimate] (1). Test User <test@example.org>

gpg> addkey
Please select what kind of key you want:
   (3) DSA (sign only)
   (4) RSA (sign only)
   (5) Elgamal (encrypt only)
   (6) RSA (encrypt only)
   (7) DSA (set your own capabilities)
   (8) RSA (set your own capabilities)
  (10) ECC (sign only)
  (11) ECC (set your own capabilities)
  (12) ECC (encrypt only)
  (13) Existing key
  (14) Existing key from card
Your selection? 8

Possible actions for a RSA key: Sign Encrypt Authenticate 
Current allowed actions: Sign Encrypt 

   (S) Toggle the sign capability
   (E) Toggle the encrypt capability
   (A) Toggle the authenticate capability
   (Q) Finished

Your selection? s

Possible actions for a RSA key: Sign Encrypt Authenticate 
Current allowed actions: Encrypt 

   (S) Toggle the sign capability
   (E) Toggle the encrypt capability
   (A) Toggle the authenticate capability
   (Q) Finished

Your selection? e

Possible actions for a RSA key: Sign Encrypt Authenticate 
Current allowed actions: 

   (S) Toggle the sign capability
   (E) Toggle the encrypt capability
   (A) Toggle the authenticate capability
   (Q) Finished

Your selection? a

Possible actions for a RSA key: Sign Encrypt Authenticate 
Current allowed actions: Authenticate 

   (S) Toggle the sign capability
   (E) Toggle the encrypt capability
   (A) Toggle the authenticate capability
   (Q) Finished

Your selection? q
RSA keys may be between 1024 and 4096 bits long.
What keysize do you want? (3072) 4096
Requested keysize is 4096 bits
Please specify how long the key should be valid.
	 0 = key does not expire
      <n>  = key expires in n days
      <n>w = key expires in n weeks
      <n>m = key expires in n months
      <n>y = key expires in n years
Key is valid for? (0) 1y
Key expires at Thu 18 Jan 2024 09:10:53 AM EET
Is this correct? (y/N) y
Really create? (y/N) y
We need to generate a lot of random bytes. It is a good idea to perform
some other action (type on the keyboard, move the mouse, utilize the
disks) during the prime generation; this gives the random number
generator a better chance to gain enough entropy.

sec  rsa4096/B11582839FE31C64
     created: 2023-01-18  expires: 2024-01-18  usage: SC  
     trust: ultimate      validity: ultimate
ssb  rsa4096/6807DFF921D6105C
     created: 2023-01-18  expires: 2024-01-18  usage: E   
ssb  rsa4096/2C485A381019A0CA
     created: 2023-01-18  expires: 2024-01-18  usage: A   
[ultimate] (1). Test User <test@example.org>

gpg> save
```

If you list your keys you should see that you have the primary key,
one encryption subkey, and one authentication subkey.

``` shell
> gpg --list-secret-keys --with-subkey-fingerprint
/home/dnaeon/.gnupg/pubring.kbx
---------------------------
sec   rsa4096 2023-01-18 [SC] [expires: 2024-01-18]
      C6D7CF3AE42593261A7116ADB11582839FE31C64
uid           [ultimate] Test User <test@example.org>
ssb   rsa4096 2023-01-18 [E] [expires: 2024-01-18]
      EC96BFBEC7A36602E90F626B6807DFF921D6105C
ssb   rsa4096 2023-01-18 [A] [expires: 2024-01-18]
      C52287E75B8E9A544910A92F2C485A381019A0CA
```

The subkey identified with the
`C52287E75B8E9A544910A92F2C485A381019A0CA` fingerprint is our
`Authentication` subkey.

The next thing we need to do is to export our authentication subkey in
the OpenSSH public key format.

``` shell
gpg --output id_rsa.pub --export-ssh-key C6D7CF3AE42593261A7116ADB11582839FE31C64 
```

The `--export-ssh-key` command will export the latest authentication
subkey only. If you happen to have multiple authentication subkeys you
can export them instead by specifying their fingerprint followed by an
exclamation mark.

And finally we need to tell `gpg-agent(1)` to add our authentication
subkey to the SSH Agent. In order to do that we need to add our subkey
`keygrip` to the `~/.gnupg/sshcontrol` file.

``` shell
> gpg --list-keys --with-keygrip
/home/dnaeon/.gnupg/pubring.kbx
---------------------------
pub   rsa4096 2023-01-18 [SC] [expires: 2024-01-18]
      C6D7CF3AE42593261A7116ADB11582839FE31C64
      Keygrip = C14A8E8F436DB8C766D54DC302EEF553814631CA
uid           [ultimate] Test User <test@example.org>
sub   rsa4096 2023-01-18 [E] [expires: 2024-01-18]
      Keygrip = 2CB4E622CB0518A6E31E31B897EFCB99DB0F7AF6
sub   rsa4096 2023-01-18 [A] [expires: 2024-01-18]
      Keygrip = 0E8B08EA9172E962647152739968D841F3EA9940
```

The keygrip of our authentication subkey from the output above is
`0E8B08EA9172E962647152739968D841F3EA9940`.

``` shell
echo 0E8B08EA9172E962647152739968D841F3EA9940 >> ~/.gnupg/sshcontrol
```

You can now verify that your key is added to the SSH Agent.

``` shell
> ssh-add -l
4096 SHA256:fDdjCMQE0mPeBwGqmrpo5NFdv4lFEvE3JgcAewYdZcc (none) (RSA)
```

In case you need to verify the fingerprint of above identity you can
also query `gpg-agent(1)`.

``` shell
> gpg-connect-agent
> keyinfo --ssh-list --ssh-fpr=sha256
S KEYINFO 0E8B08EA9172E962647152739968D841F3EA9940 D - - - P SHA256:fDdjCMQE0mPeBwGqmrpo5NFdv4lFEvE3JgcAewYdZcc - S
OK
```

The only remaining thing is to distribute your OpenSSH public key to
the systems where you need to login to and add your public key to the
`~/.ssh/authorized_keys` file.

Once that is done you should be able to SSH into these systems.

In case you are having issues logging into the remote systems you can
troubleshoot the issues by enabling verbose logging for the SSH client
and `gpg-agent(1)` in order to get more details about it.

One issue I have faced when initially setting up GnuPG for OpenSSH
authentication is this one.

``` shell
> ssh box01
sign_and_send_pubkey: signing failed for RSA "(none)" from agent: agent refused operation
dnaeon@box01: Permission denied (publickey).
```

Looking around for previously reported bugs about it didn't provide
much insight what might be causing this one, as most of the reported
issues were from years ago and they all suggest that setting up the
`GPG_TTY` environment variable fixes the issue. However, that was not
my case, since I already had these env vars correctly set.

In order to troubleshoot the issue I've enabled verbose logging for
the `gpg-agent(1)` by adding the following lines to
`~/.gnupg/gpg-agent.conf` file.

``` shell
verbose
log-file /path/to/gpg-agent.log
```

You also need to reload the `gpg-agent(1)` after making above changes.

``` shell
gpg-connect-agent reloadagent /bye
```

Here is what my `gpg-agent(1)` logs looked like when trying to
authenticate against the remote SSH server, and then subsequently
failing with the error message shown previously.

``` shell
gpg-agent[22071]: handler 0x7fdb6b1fd6c0 for fd 9 started
gpg-agent[22071]: handler 0x7fdb6b1fd6c0 for fd 9 terminated
gpg-agent[22071]: handler 0x7fdb6b1fd6c0 for fd 9 started
gpg-agent[22071]: handler 0x7fdb6b1fd6c0 for fd 9 terminated
gpg-agent[22071]: ssh handler 0x7fdb6b1fd6c0 for fd 9 started
gpg-agent[22071]: ssh request 27 is not supported
gpg-agent[22071]: ssh request handler for request_identities (11) started
gpg-agent[22071]: new connection to SCdaemon established (reusing)
gpg-agent[22071]: ssh request handler for request_identities (11) ready
gpg-agent[22071]: ssh request handler for sign_request (13) started
gpg-agent[22071]: starting a new PIN Entry
gpg-agent[22071]: failed to unprotect the secret key: No such file or directory
gpg-agent[22071]: failed to read the secret key
gpg-agent[22071]: ssh sign request failed: No such file or directory <Pinentry>
gpg-agent[22071]: ssh request handler for sign_request (13) ready
gpg-agent[22071]: ssh handler 0x7fdb6b1fd6c0 for fd 9 terminated
```

The entries from above log snippet seem to suggest an issue with
`pinentry`, which is used by `gpg(1)`. In my case I have been using
`pinentry-curses` as the `pinentry-program`. Switching that to
`pinentry-qt` solved my issues.

In case you are seeing the same error try out with a different
pinentry program. Here's a `gpg-agent(1)` config file, which uses
`pinentry-qt`.

``` shell
# ~/.gnupg/gpg-agent.conf
enable-ssh-support
pinentry-program /usr/bin/pinentry-qt
default-cache-ttl 3600
default-cache-ttl-ssh 3600
```

Don't forget to reload your `gpg-agent(1)` after making any changes to
your `gpg-agent.conf` file.

``` shell
gpg-connect-agent reloadagent /bye
```
