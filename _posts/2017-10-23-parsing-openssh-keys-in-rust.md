---
layout: post
title: Parsing OpenSSH keys in Rust
tags: programming openssh rust
---
Recently I had a need to parse some
[OpenSSH certificate keys](https://cvsweb.openbsd.org/cgi-bin/cvsweb/src/usr.bin/ssh/PROTOCOL.certkeys?annotate=HEAD),
and since it's been a while since I've written anything in
[Rust](https://rust-lang.org/) I thought I'd dive into the
language again and create a library for parsing OpenSSH certificates in it.

A quick check in [crates.io](https://crates.io) didn't show any
crates that currently support OpenSSH certificates, so I was good to go.

There are in existence a few crates for parsing of OpenSSH public keys, but
they all are at this time of writing incomplete, since they support just a
few of the types a public key can be.

I was quickly able to create a prototype that was able to parse
an OpenSSH certificate, so it was time to polish a bit the code,
add tests to it, and finally publish it on [crates.io](https://crates.io/)

Fast forward a few weeks later I've published the
[sshkeys crate](https://crates.io/crates/sshkeys).

The crate can now parse all currently supported OpenSSH
public key types.

* RSA
* DSA
* ECDSA
* ED25519

In addition to that it also supports parsing of OpenSSH certificates,
which was the primary goal of this project in the first place.

* ssh-rsa-cert-v01@openssh.com
* ssh-dss-cert-v01@openssh.com
* ecdsa-sha2-nistp256-cert-v01@openssh.com
* ecdsa-sha2-nistp384-cert-v01@openssh.com
* ecdsa-sha2-nistp512-cert-v01@openssh.com
* ssh-ed25519-cert-v01@openssh.com

The code is available on [Github](https://github.com/dnaeon/rust-sshkeys)
and also as a [crate published on crates.io](https://crates.io/crates/sshkeys).

The library is well tested and comes with documentation, which you can
either read online [here](https://docs.rs/sshkeys/) or
build and read locally by executing the command below.

```bash
cargo doc --open
```

The crate also comes with examples which you can view and run.

The
[examples/certificate.rs](https://github.com/dnaeon/rust-sshkeys/blob/master/examples/certificate.rs)
example shows how to parse an OpenSSH certificate key.

This is what the
[examples/certificate.rs](https://github.com/dnaeon/rust-sshkeys/blob/master/examples/certificate.rs)
file looks like.

```rust
extern crate sshkeys;

fn main() {
    let cert = sshkeys::Certificate::from_path("examples/id_ed25519-cert.pub").unwrap();

    println!("Type: {} {}", cert.key_type.name, cert.cert_type);
    println!("Public key: {}", cert.key);
    println!("Signing CA: {}", cert.signature_key);
    println!("Key ID: {}", cert.key_id);
    println!("Serial: {}", cert.serial);
    println!("Valid from {} to {}", cert.valid_after, cert.valid_before);
    println!("Principals:");
    for p in cert.valid_principals {
        println!("\t{}", p);
    }
    println!("Critical Options:");
    for (name, value) in cert.critical_options {
        println!("\t{} {}", name, value);
    }
    println!("Extensions:");
    for (name, _) in cert.extensions {
        println!("\t{}", name);
    }
}
```

We can run this example by using `cargo` and the `--example` option.

```bash
cargo run --example certificate
```

And this is the output we get from our example program.

```text
   Compiling sshkeys v0.1.1 (file:///Users/mnikolov/Projects/rust/sshkeys)
    Finished dev [unoptimized + debuginfo] target(s) in 1.95 secs
     Running `target/debug/examples/certificate`
Type: ssh-ed25519-cert-v01@openssh.com user certificate
Public key: 256 SHA256:ppYFPx0k4Ogs230n6eX9vGPpnNsTB0LPrDWXh1YjClA  (ED25519-CERT)
Signing CA: 2048 SHA256:8bEmsdiV2BXhjrzPhp8dPrSLUK3U/YpIXT8NIw6Ym+s  (RSA)
Key ID: john.doe
Serial: 0
Valid from 1506934140 to 1538383841
Principals:
	root
Critical Options:
	force-command /usr/bin/true
Extensions:
	permit-user-rc
	permit-agent-forwarding
	permit-X11-forwarding
	permit-port-forwarding
	permit-pty
```

The output is similar to what `ssh-keygen` prints when using the `-L` flag.

Make sure to also check the [examples directory](https://github.com/dnaeon/rust-sshkeys/tree/master/examples)
for additional examples on using the `sshkeys` library.
