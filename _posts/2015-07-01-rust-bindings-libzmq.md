---
layout: post
title: Rust low-level bindings to libzmq
tags: programming rust zeromq
---
[ZeroMQ](https://github.com/zeromq/libzmq) is a lightweight messaging
library which makes it fairly easy for developers to design and
implement distributed systems.

ZeroMQ comes with a pretty nice API, so getting started with it is
quite easy. Lots of languages also provide bindings to the libzmq
library, so you could already get started with ZeroMQ using your
favourite language of choice.

Perhaps the best place to get started with ZeroMQ is to head over
to the [ZeroMQ Guide](http://zguide.zeromq.org/page:all) which
will teach you about ZeroMQ by going through various
examples and problems and show you how to solve them using the
library.

I've been using ZeroMQ for quite some time already and implemented a
few distributed systems with it, mostly written in Python and C.

These days I'm trying to learn myself a bit of
[Rust](http://www.rust-lang.org/), which is a systems programming
language with focus on safety, speed and concurrency.

Being a new language, Rust comes with some new concepts not found in
other languages. One of them is the
[Rust Ownership System](https://doc.rust-lang.org/nightly/book/ownership.html),
which takes a bit of time getting used to it.

New Rust users often
[fight with the borrow checker](https://doc.rust-lang.org/book/ownership.html),
where the Rust compiler refuses to compile your code and this is where
understanding how the
[Rust Ownership System](https://doc.rust-lang.org/nightly/book/ownership.html)
works is something that we as Rust developers should become quite
familiar with.

So far I really enjoy programming in Rust, and I really like how
nice and easy the language can bind into existing C libraries
using the
[Rust FFI interface](https://doc.rust-lang.org/nightly/book/ffi.html),
so as one of the first Rust projects I've decided to work on is
low-level bindings to the ZeroMQ messaging library.

At the time I've started writing the libzmq bindings I already knew
about the existence of the
[native implementation of ØMQ in Rust](https://github.com/zeromq/zmq.rs)
and also the [Rust ZeroMQ bindings](https://github.com/erickt/rust-zmq).

The
[native implementation of ØMQ in Rust](https://github.com/zeromq/zmq.rs)
project, though seems to have stalled development for a while already
and looks to be abandoned now, so it is far from complete.

The [upstream libzmq](https://github.com/zeromq/libzmq) version
(as of writing this post) is v4.2.0 and the
[existing Rust ZeroMQ bindings](https://github.com/erickt/rust-zmq)
so far do not fully support the latest version of libzmq as
already reported
[in this issue](https://github.com/erickt/rust-zmq/issues/69).

Of course it would be much easier to get the existing Rust ZeroMQ
bindings up to speed with the latest libzmq version, but
these bindings have been generated by
[rust-bindgen](https://github.com/crabtw/rust-bindgen), which is a
binding generator and a really cool project, but the thing is that
`rust-bindgen` generates a slightly different Rust code on my system
than the one I can already find in the existing repository.

I bet that others would also get different Rust code if they ran
`rust-bindgen` on their systems as well, depending on what Rust
version they have, OS and probably some other factors. And also
the resulting Rust code as generated by `rust-bindgen` does not
always look as pretty as it would if it was written by a human.

I'm not saying that `rust-bindgen` is bad, it's just that I think
for a library such as the
[ZeroMQ C API](https://github.com/zeromq/libzmq/blob/master/include/zmq.h),
which comes with a really nice and small factor API it's simply not
worth creating the bindings via an automatic generator, when we can
always easily translate them into Rust.

Overall this makes it *not* so easy to contribute back to the
[existing Rust ZeroMQ bindings](https://github.com/erickt/rust-zmq),
so I've decided to create my own
[Rust low-level bindings to libzmq](https://github.com/dnaeon/rust-libzmq),
which targets the latest libzmq version (v4.2.0 at the time of writing
this post).

That way I can practice my Rust and learn a few things along the way.

You can find the
[Rust low-level bindings to libzmq](https://github.com/dnaeon/rust-libzmq)
in the Github repo and here we will create a few simple projects using
the libzmq bindings for Rust.

Lets start off by creating a new Rust project, which will get the
`libzmq` library version that we have installed on our system.

```bash
$ cargo new --bin libzmq-version
$ cd libzmq-version
```

Now, open the `Cargo.toml` file and add the `libzmq` dependency to
your project. This is how my `Cargo.toml` file looks like.

```toml
[package]
name = "libzmq-version"
version = "0.1.0"
authors = ["Marin Atanasov Nikolov <dnaeon@gmail.com>"]

[dependencies.libc]
version = "*"

[dependencies.libzmq]
git = "https://github.com/dnaeon/rust-libzmq.git"
```

The code below is what represents our `src/main.rs` file from our
Rust project. When you build and run it, it will print the version
of your `libzmq` library.

```rust
extern crate libc;
extern crate libzmq;

unsafe fn print_version() {
    let mut major = 0;
    let mut minor = 0;
    let mut patch = 0;

    libzmq::zmq_version(&mut major, &mut minor, &mut patch);
    println!("Installed ZeroMQ version is {}.{}.{}", major, minor, patch);
}

fn main() {
    unsafe { print_version(); }
}
```

Lets build our project now.

```bash
$ cacargo build
    Updating registry `https://github.com/rust-lang/crates.io-index`
    Updating git repository `https://github.com/dnaeon/rust-libzmq.git`
 Downloading libc v0.1.8
   Compiling libc v0.1.8
   Compiling libzmq v4.2.0 (https://github.com/dnaeon/rust-libzmq.git#c39b529b)
   Compiling libzmq-version v0.1.0 (file:///home/mnikolov/libzmq-version)
```

Once our project is built, we can now actually run it, so lets do
that now.

```bash
$ cargo run
     Running `target/debug/libzmq-version`
Installed ZeroMQ version is 4.2.0
```

And that was easy!

Lets create another project now, which will use the
[Request-Reply Pattern](https://en.wikipedia.org/wiki/Request%E2%80%93response),
so that we can send messages back and forth between two nodes.

Similar to the previous example create a new Rust project called
`helloworld-server` and another one called `helloworld-client`.

Below is the code for the `helloworld-server` project, which will
bind a ZeroMQ `ZMQ_REQ` socket on `tcp://*:5555` endpoint and send
messages to whoever connects to it until we actually kill the process.

```rust
extern crate libc;
extern crate libzmq;

use std::ffi;

unsafe fn helloworld_server() {
    let context = libzmq::zmq_ctx_new();
    let responder = libzmq::zmq_socket(context, libzmq::ZMQ_REP as libc::c_int);

    let endpoint = ffi::CString::new("tcp://*:5555").unwrap();
    assert_eq!(libzmq::zmq_bind(responder, endpoint.as_ptr()), 0);

    let buffer = ffi::CString::new("Hello").unwrap();
    let data = ffi::CString::new("World").unwrap();

    loop {
        libzmq::zmq_recv(responder, buffer.as_ptr() as *mut libc::c_void, 5, 0);
        println!("Received Hello!");
        libzmq::zmq_send(responder, data.as_ptr() as *const libc::c_void , 5, 0);
    }
}

fn main() {
    unsafe { helloworld_server(); }
}
```

And here is the client part of our project, which will connect to our
server by using a ZeroMQ `ZMQ_REQ` socket and connect it to the
`tcp://127.0.0.1:5555` endpoint. It will then send ten messages to
the server and exit.

```rust
extern crate libc;
extern crate libzmq;

use std::ffi;
use std::thread;

unsafe fn helloworld_client() {
    let context = libzmq::zmq_ctx_new();
    let receiver = libzmq::zmq_socket(context, libzmq::ZMQ_REQ as libc::c_int);

    let endpoint = ffi::CString::new("tcp://127.0.0.1:5555").unwrap();
    assert_eq!(libzmq::zmq_connect(receiver, endpoint.as_ptr()), 0);

    let data = ffi::CString::new("Hello").unwrap();
    let buffer = ffi::CString::new("").unwrap();

    for request in 0..10 {
        println!("Sending request #{}", request);
        libzmq::zmq_send(receiver, data.as_ptr() as *const libc::c_void, 5, 0);

        thread::sleep_ms(1000);
        libzmq::zmq_recv(receiver, buffer.as_ptr() as *mut libc::c_void, 5, 0);
        println!("Received reply!");
    }
}

fn main() {
    unsafe { helloworld_client(); }
}
```

Once ready, simply build the projects and run them. You should now
see ZeroMQ messages passing back and forth between our client and
server.

You should also note that our functions are actually marked as
[unsafe](https://doc.rust-lang.org/nightly/book/unsafe.html), which is
required since we are calling foreign code via the
[Rust FFI interface](https://doc.rust-lang.org/nightly/book/ffi.html).

Further improvement on this code would be to actually write a nice
high-level wrapper around these low-level bindings that would make
use of Rust's main features on safety, but I'll leave that for
some other day.