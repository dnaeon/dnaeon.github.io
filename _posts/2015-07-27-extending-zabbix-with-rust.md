---
layout: post
title: Extending Zabbix monitoring using the Rust programming language
tags: monitoring zabbix programming rust
---
Since [Zabbix release 2.2.0](http://www.zabbix.com/rn2.2.0.php),
users and developers of Zabbix can now extend Zabbix monitoring
features by using
[loadable modules](https://www.zabbix.com/documentation/2.4/manual/config/items/loadablemodules).

Up until that release the only way to extend Zabbix was by using
scripts in the form of
[user parameters](https://www.zabbix.com/documentation/2.2/manual/config/items/userparameters),
[external scripts](https://www.zabbix.com/documentation/2.2/manual/config/items/itemtypes/external) and
[system.run[] items](https://www.zabbix.com/documentation/2.2/manual/config/items/itemtypes/zabbix_agent).

However the problem with using scripts for your metrics is that each
time you need to get that metric Zabbix needs to
[fork(2)](https://en.wikipedia.org/wiki/Fork_(system_call)) a new
process and that could be a serious performance penalty, especially if
you have lots of metrics.

On the other hand using the
[loadable modules](https://www.zabbix.com/documentation/2.4/manual/config/items/loadablemodules)
in recent versions of Zabbix you could now create new Zabbix items
easily without the overhead and performance issues that external
scripts have.

A loadable module in Zabbix is essentially a
[shared library](https://en.wikipedia.org/wiki/Library_(computing)#Shared_libraries),
which is loaded by the Zabbix Agent, Server or Proxy daemons during
startup. In order for a shared library to be recognized as a valid
Zabbix module your plugin needs to provide certain symbols in it's
[symbols table](https://en.wikipedia.org/wiki/Symbol_table).

Loadable modules for Zabbix are usually written in C and you can
already find a good starting point for creating a new Zabbix plugin
at the
[Zabbix loadable modules page](https://www.zabbix.com/documentation/2.4/manual/config/items/loadablemodules).

Another way that we could extend Zabbix monitoring features by using
loadable modules is to use the
[Rust programming language](http://www.rust-lang.org/), and this is
what we will focus on in this post.

[Rust](http://www.rust-lang.org/) is a systems programming language
with focus on safety, speed and concurrency. Rust can easily hook
into existing C libraries using the
[Rust FFI interface](https://doc.rust-lang.org/nightly/book/ffi.html), but
you could also write Rust code that compiles to a shared library and
can be called from C (or other languages, e.g. Python, Ruby, etc.) as well.

In this post we will see how to create a low-level Rust library by
translating the current Zabbix C API to Rust and once we are ready with
the low-level stuff we will see how to use that Rust crate in order to
create a Zabbix loadable module written in Rust.

Throughout this post we have used:

* CentOS release 7.1.1503 (3.10.0-229.el7.x86_64)
* Rust 1.1.0 (35ceea399 2015-06-19)
* Zabbix 2.4.5-1.el7

You can also find the code used throughtout this post in the
[rust-zbx](https://github.com/dnaeon/rust-zbx) repository.

With that said, lets get started!

## Translating the Zabbix C API to Rust

As a first step we would do is to get the existing Zabbix C API
translated to Rust, so that we can later build our loadable module
on top of it.

Lets start first by creating a new Rust project.

```bash
$ cargo new rust-zbx
```

Our `Cargo.toml` file looks like this:

```toml
[package]
name = "zbx"
version = "0.1.0"
description = "Crate for creating Zabbix loadable modules"
authors = ["Marin Atanasov Nikolov <dnaeon@gmail.com>"]

[dependencies]
libc = "*"
```

The Zabbix C API is located within the `include/module.h` header file
of the Zabbix source tree and this is the file that we will now
translate to Rust. In this header you will find various C macros and
structs used by the Zabbix modules.

First, lets translate the C macros to Rust now.

```c
/* include/module.h */

#define ZBX_MODULE_OK   0
#define ZBX_MODULE_FAIL -1

#define ZBX_MODULE_API_VERSION_ONE      1

/* flags for command */
#define CF_HAVEPARAMS           0x01    /* item accepts either optional or mandatory parameters */
#define CF_MODULE               0x02    /* item is defined in a loadable module */
#define CF_USERPARAMETER        0x04    /* item is defined as user parameter */

/* agent result types */
#define AR_UINT64       0x01
#define AR_DOUBLE       0x02
#define AR_STRING       0x04
#define AR_TEXT         0x08
#define AR_LOG          0x10
#define AR_MESSAGE      0x20

#define SYSINFO_RET_OK          0
#define SYSINFO_RET_FAIL        1

```

At the top of our `src/lib.rs` file we will first add the crates
that we'll use throughout our library.

```rust
extern crate libc;

use std::{ffi, mem};
use libc::{c_char, c_int, c_uint, uint64_t,  c_double, malloc, strncpy};
```

And now translating the above C macros to Rust would look like this:

```rust
// Return codes used by module during (un)initialization
pub const ZBX_MODULE_OK: c_int = 0;
pub const ZBX_MODULE_FAIL: c_int = -1;

// Module API versions
pub const ZBX_MODULE_API_VERSION_ONE: c_int = 1;

// Flags for commands
// Item does not accept parameters
pub const CF_NOPARAMS: c_uint = 0;

// Item accepts either optional or mandatory parameters
pub const CF_HAVEPARAMS: c_uint = 1;

// Item is defined in a loadable module
pub const CF_MODULE: c_uint = 2;

// Item is defined as user parameter
pub const CF_USERPARAMETER: c_uint = 4;

// Agent result types
pub const AR_UINT64: c_int = 1;
pub const AR_DOUBLE: c_int = 2;
pub const AR_STRING: c_int = 4;
pub const AR_TEXT: c_int = 8;
pub const AR_LOG: c_int = 16;
pub const AR_MESSAGE: c_int = 32;

// Return codes used by item callbacks
pub const SYSINFO_RET_OK: c_int = 0;
pub const SYSINFO_RET_FAIL: c_int = 1;
```

Pretty straightforwad, now lets translate the C structs as well into
Rust add implement common methods that we will use when working with
these types in Rust.

```c
typedef struct
{
        char            *key;
        unsigned        flags;
        int             (*function)();
        char            *test_param;    /* item test parameters; user parameter items keep command here */
}
ZBX_METRIC;

/* agent request structure */
typedef struct
{
        char            *key;
        int             nparam;
        char            **params;
        zbx_uint64_t    lastlogsize;
        int             mtime;
}
AGENT_REQUEST;

typedef struct
{
        char            *value;
        char            *source;
        zbx_uint64_t    lastlogsize;
        int             timestamp;
        int             severity;
        int             logeventid;
        int             mtime;
}
zbx_log_t;

/* agent return structure */
typedef struct
{
        int             type;
        zbx_uint64_t    ui64;
        double          dbl;
        char            *str;
        char            *text;
        char            *msg;

        /* null-terminated list of pointers */
        zbx_log_t       **logs;
}
AGENT_RESULT;
```

The `ZBX_METRIC` struct is used for creating new Zabbix items and
contains information about an item such as the name, whether the item
accepts parameters or not, a callback function used for processing and
calculating the item and optional test parameters.

The `AGENT_REQUEST` struct is used whenever a new request is sent to
Zabbix for processing. An instance of `AGENT_REQUEST` struct is usually
passed by Zabbix to our callback function which contains all details
about the actual request - name of the item, the number of parameters
passed to the item, the actual parameters and others.

The `AGENT_RESULT` struct is used for storing the result from our
callback. Zabbix passes a pointer to an actual instance of
`AGENT_RESULT` and our callback is expected to update this instance
with some result. The result can optionally contain a message with it,
usually used for indicating an error condition.

Now, lets translate these structs to Rust.

```rust
#[repr(C)]
pub struct ZBX_METRIC {
    pub key: *const c_char,
    pub flags: c_uint,
    pub function: extern "C" fn(*mut AGENT_REQUEST, *mut AGENT_RESULT) -> c_int,
    pub test_param: *const c_char,
}

#[repr(C)]
pub struct AGENT_REQUEST {
    key: *const c_char,
    nparam: c_int,
    params: *const *const c_char,
    lastlogsize: uint64_t,
    mtime: c_int,
}

#[repr(C)]
pub struct zbx_log_t {
    value: *const c_char,
    source: *const c_char,
    lastlogsize: uint64_t,
    timestamp: c_int,
    severity: c_int,
    logeventid: c_int,
    mtime: c_int,
}

#[repr(C)]
pub struct AGENT_RESULT {
    _type: c_int,
    ui64: uint64_t,
    dbl: c_double,
    _str: *const c_char,
    text: *const c_char,
    msg: *const c_char,
    logs: *const *const zbx_log_t,
}
```

Again, this is a pretty straightforward task. Since these types
need to be passed between Rust and C we need to make sure that our
Rust structs are compatible with the C representation, therefore we
apply the `#[repr(C)]` attribute to our Rust structs.

In order to create new Zabbix items easily from Rust we will also
introduce a new Rust type. This will be a high-level type that we'll
use for creating new Zabbix items for our loadable modules.

```rust
// Type used for creating new Zabbix item keys
pub struct Metric {
    pub key: ffi::CString,
    pub flags: c_uint,
    pub function: extern "C" fn(*mut AGENT_REQUEST, *mut AGENT_RESULT) -> c_int,
    pub test_param: ffi::CString,
}

impl Metric {
    pub fn new(key: &str, flags: u32, function: extern "C" fn(*mut AGENT_REQUEST, *mut AGENT_RESULT) -> c_int, test_param: &str) -> Metric {
        Metric {
            key: ffi::CString::new(key).unwrap(),
            flags: flags as c_uint,
            function: function,
            test_param: ffi::CString::new(test_param).unwrap(),
        }
    }

    pub fn to_zabbix_item(&self) -> ZBX_METRIC {
        ZBX_METRIC {
            key: self.key.as_ptr(),
            flags: self.flags as c_uint,
            function: self.function,
            test_param: self.test_param.as_ptr(),
        }
    }
}
```

The Zabbix C API also provides a C macro used for retrieving the
parameters passed to an item. This is how the C macro looks like.

```c
#define get_rparam(request, num)        (request->nparam > num ? request->params[num] : NULL)
```

We will translate this C macro to a Rust
[associated function](https://doc.rust-lang.org/book/method-syntax.html#associated-functions)
for the `AGENT_REQUEST` type. The result of the
`AGENT_REQUEST::get_params` associated function is a
[Vector](https://doc.rust-lang.org/book/vectors.html) containing
the parameters of this request as passed by Zabbix.

```rust
impl AGENT_REQUEST {
    pub fn get_params<'a>(request: *mut AGENT_REQUEST) -> Vec<&'a[u8]> {
        unsafe {
            let len = (*request).nparam;
            let mut v = Vec::new();

            for i in 0..len {
                let ptr = (*request).params.offset(i as isize);
                let param = ffi::CStr::from_ptr(*ptr).to_bytes();
                v.push(param);
            }

            v
        }
    }
}
```

Our Rust callbacks also need to set result, which will be returned
to Zabbix. To do that we will also need to translate the C API
macros which deal with setting result. These are the C API Zabbix
macros used for setting result from an agent request.

```c
#define SET_UI64_RESULT(res, val)               \
(                                               \
        (res)->type |= AR_UINT64,               \
        (res)->ui64 = (zbx_uint64_t)(val)       \
)

#define SET_DBL_RESULT(res, val)                \
(                                               \
        (res)->type |= AR_DOUBLE,               \
        (res)->dbl = (double)(val)              \
)

/* NOTE: always allocate new memory for val! DON'T USE STATIC OR STACK MEMORY!!! */
#define SET_STR_RESULT(res, val)                \
(                                               \
        (res)->type |= AR_STRING,               \
        (res)->str = (char *)(val)              \
)

/* NOTE: always allocate new memory for val! DON'T USE STATIC OR STACK MEMORY!!! */
#define SET_TEXT_RESULT(res, val)               \
(                                               \
        (res)->type |= AR_TEXT,                 \
        (res)->text = (char *)(val)             \
)

/* NOTE: always allocate new memory for val! DON'T USE STATIC OR STACK MEMORY!!! */
#define SET_LOG_RESULT(res, val)                \
(                                               \
        (res)->type |= AR_LOG,                  \
        (res)->logs = (zbx_log_t **)(val)       \
)

/* NOTE: always allocate new memory for val! DON'T USE STATIC OR STACK MEMORY!!! */
#define SET_MSG_RESULT(res, val)                \
(                                               \
        (res)->type |= AR_MESSAGE,              \
        (res)->msg = (char *)(val)              \
)
```

You should note that when the result of an item is a
string (text, message and log) Zabbix expects to receive a raw pointer
to a previously allocated memory of the resulting string and once
done with the result Zabbix will `free(3)` the memory.

If we tried to return a reference to a string from Rust to C we would
end up in some ugly situations such as double-freeing the result,
as Zabbix would try to `free(3)` the memory, when it was already
deallocated by the Rust [borrow checker](https://doc.rust-lang.org/book/references-and-borrowing.html)
once our string goes out of scope.

In order to deal with this we need to allocate some memory from Rust
for the resulting string and actually leak that memory to Zabbix,
which will be deallocated by Zabbix once done with the result. For
this purpose we will introduce a helper function that we will use
when working with string results in Zabbix.

```rust
// When the result of a Zabbix item is text (string, text and message)
// Zabbix expects to receive a pre-allocated pointer with the result
// string, which is free(3)'d by Zabbix once done with the result.
unsafe fn string_to_malloc_ptr(src: &str) -> *mut c_char {
    let c_src = ffi::CString::new(src).unwrap();
    let len = c_src.to_bytes_with_nul().len() as u64;

    let dst = malloc(len) as *mut c_char;
    strncpy(dst, c_src.as_ptr(), len);

    dst
}
```

Now, lets implement the functions for setting results as well.
We will implement them as
[associated functions](https://doc.rust-lang.org/book/method-syntax.html#associated-functions)
for the `AGENT_RESULT` type.

```rust
impl AGENT_RESULT {
    pub fn set_uint64_result(result: *mut AGENT_RESULT, value: u64) {
        unsafe {
            (*result)._type |= AR_UINT64;
            (*result).ui64 = value as uint64_t;
        }
    }

    pub fn set_f64_result(result: *mut AGENT_RESULT, value: f64) {
        unsafe {
            (*result)._type |= AR_DOUBLE;
            (*result).dbl = value as c_double;
        }
    }

    pub fn set_str_result(result: *mut AGENT_RESULT, value: &str) {
        unsafe {
            (*result)._type |= AR_STRING;
            (*result)._str = string_to_malloc_ptr(value);
        }
    }

    pub fn set_text_result(result: *mut AGENT_RESULT, value: &str) {
        unsafe {
            (*result)._type |= AR_TEXT;
            (*result).text = string_to_malloc_ptr(value);
        }
    }

    pub fn set_msg_result(result: *mut AGENT_RESULT, value: &str) {
        unsafe {
            (*result)._type |= AR_MESSAGE;
            (*result).msg = string_to_malloc_ptr(value);
        }
    }

    // TODO: Implement set_log_result(...)
}
```

Dereferencing a raw pointer is considered
[unsafe](https://doc.rust-lang.org/book/unsafe.html) operation in Rust,
therefore we are using the `unsafe` keyword in our functions above.

As a last thing we will create another helper function that we can
later use in our loadable modules for easy item creation.

```rust
pub fn create_items(metrics: &Vec<Box<Metric>>) -> *const ZBX_METRIC {
    let items = metrics
        .iter()
        .map(|metric| metric.to_zabbix_item())
        .collect::<Vec<_>>();

    // XXX: leak items into the void
    let ptr = items.as_ptr();
    mem::forget(items);

    ptr
}
```

So far we have built a Rust low-level interface of the Zabbix
loadable modules API. In the next section of this post we will
see how to create a fully functional loadable module for Zabbix in
Rust.

## Building a Zabbix module in Rust

Now that we have the low-level stuff sorted out, lets create a simple
Zabbix loadable module in Rust using the library that we've
created in the previous section.

First, lets create a new Rust project:

```bash
$ cargo new dummy
```

Our `Cargo.toml` file for this Zabbix `dummy` module like this:

```toml
[package]
name = "dummy"
version = "0.1.0"
description = "Example Zabbix loadable module written in Rust"
authors = ["Marin Atanasov Nikolov <dnaeon@gmail.com>"]

[lib]
name = "rust_dummy"
crate-type = ["dylib"]

[dependencies.rand]
version = "*"

[dependencies.zbx]
git = "https://github.com/dnaeon/rust-zbx.git"
version = "*"
```

As mentioned in the beginning of this post the Zabbix
[loadable modules](https://www.zabbix.com/documentation/2.4/manual/config/items/loadablemodules)
are simply shared libraries, and that is why our Rust crate
needs to be of `dylib` type.

This setting simply instructs the Rust compiler to build the resulting
Rust library as a shared library, which can then be used from other
languages as well.

A Zabbix loadable module should provide certain symbols in it's
[symbols table](https://en.wikipedia.org/wiki/Symbol_table) in order
be to recognized as valid Zabbix plugin and our Rust library should
provide these symbols as well.

These are the `zbx_module_api_version`, `zbx_module_init`,
`zbx_module_uninit` and `zbx_module_item_list` symbols.

This is how the C prototypes for the above functions look like:

```c
int         zbx_module_api_version(void);
int         zbx_module_init(void);
int         zbx_module_uninit(void);
ZBX_METRIC  *zbx_module_item_list(void);
```

The `zbx_module_api_version` callback should return the module API
version, and currently the only supported version for it is
`ZBX_MODULE_API_VERSION_ONE`.

The `zbx_module_init` callback is used by modules for performing
any initialiazation that the module needs to perform before it can be
used. This function is called during module loading and should
return either a success or a failure code indicating the result of the
initialization procedure.

Similar to the `zbx_module_init` the `zbx_module_uninit` callback is
used as the shutdown procedure for a Zabbix module, e.g. de-allocating
any resources, closing sockets, etc.

The `zbx_module_item_list` callback is the one that returns an
array of items that Zabbix will be able to use for processing
any metrics requests.

Lets first add the Rust crates that we'll use throughout our Rust
module at the top of our `src/lib.rs` file.

```rust
// Example Zabbix loadable module written in Rust

extern crate zbx;
extern crate rand;

use std::{boxed, mem, str};
use rand::Rng;
```

Now, lets implement the `zbx_module_api_version` function now.

```rust
#[no_mangle]
pub extern fn zbx_module_api_version() -> i32 {
    zbx::ZBX_MODULE_API_VERSION_ONE
}
```

Our initialization and shutdown module functions are pretty easy as well.

```rust
#[no_mangle]
pub extern fn zbx_module_init() -> i32 {
    zbx::ZBX_MODULE_OK
}

#[no_mangle]
pub extern fn zbx_module_uninit() -> i32 {
    zbx::ZBX_MODULE_OK
}
```

Since our example Zabbix module does not require any special
initialization or shutdown procedures we are simply returning a
success code back to Zabbix. In case your module requires any
initialization and shutdown procedures this is the place where you
would implement them.

And now we need to implement the `zbx_module_item_list` function,
which should return an array of items, that can later be used by
Zabbix for item processing.

```rust
#[no_mangle]
pub extern fn zbx_module_item_list() -> *const zbx::ZBX_METRIC {
    let metrics = vec![
        boxed::Box::new(zbx::Metric::new("rust.echo", zbx::CF_HAVEPARAMS, rust_echo, "")),
        boxed::Box::new(zbx::Metric::new("rust.random", zbx::CF_NOPARAMS, rust_random, "")),
    ];

    // XXX: Leak items into the void
    let items = zbx::create_items(&metrics);
    mem::forget(metrics);

    items
}
```

In the above example function we have created two new Zabbix items
using the `zbx::Metric::new` associated function.

The `rust.echo` item accepts parameters from Zabbix, while
`rust.random` does not and both of these items do not have any test
parameters.

Finally we are returning the items back to Zabbix by using the
`zbx::create_items` helper function.

Now we need to implement the `rust_echo` and `rust_random`
callbacks as well, which are the ones that perform the actual item
processing when a new request is sent to Zabbix.

Lets implement the `rust_echo` function now.

```rust
#[no_mangle]
pub extern fn rust_echo(request: *mut zbx::AGENT_REQUEST, result: *mut zbx::AGENT_RESULT) -> i32 {
    let params = zbx::AGENT_REQUEST::get_params(request);

    if params.len() != 1 {
        zbx::AGENT_RESULT::set_msg_result(result, "Invalid number of parameters");
        return zbx::SYSINFO_RET_FAIL;
    }

    let param = match str::from_utf8(params[0]) {
        Ok(p)  => p,
        Err(e) => panic!("Invalid UTF-8 sequence: {}", e),
    };

    zbx::AGENT_RESULT::set_str_result(result, param);

    zbx::SYSINFO_RET_OK
}
```

The `rust_echo` function expects to receive exactly one parameter
from Zabbix and echoes it back.

And this is our `rust_random` function that will generate a random
number each time it gets called and returns it to Zabbix.

```rust
#[no_mangle]
#[allow(unused_variables)]
pub extern fn rust_random(request: *mut zbx::AGENT_REQUEST, result: *mut zbx::AGENT_RESULT) -> i32 {
    let mut rng = rand::thread_rng();
    let num = rng.gen::<u64>();

    zbx::AGENT_RESULT::set_uint64_result(result, num);

    zbx::SYSINFO_RET_OK
}
```

If you have followed this post by far you should now have a fully
working Zabbix module written in Rust!

Time to test things out and see our Rust module in action!

First, lets build our Rust library using [Cargo](http://doc.crates.io/).

```bash
$ cargo build --release
```

Once you successfully build your project you should find the
Rust library within your `target/release` directory.

```bash
$ ls -l target/release/librust_dummy.so
-rwxr-xr-x 1 mnikolov mnikolov 3095064 Jul 27 14:41 target/release/librust_dummy.so
```

In order to load this library in your Zabbix Agent, Server or Proxy
you should update their respective configuration files and update the
`LoadModule` and `LoadModulePath` configuration settings.

This is how my Zabbix Agent settings look like:

```ini
####### LOADABLE MODULES #######

### Option: LoadModulePath
#       Full path to location of agent modules.
#       Default depends on compilation options.
#
# Mandatory: no
# Default:
LoadModulePath=/usr/lib/zabbix/modules

### Option: LoadModule
#       Module to load at agent startup. Modules are used to extend functionality of the agent.
#       Format: LoadModule=<module.so>
#       The modules must be located in directory specified by LoadModulePath.
#       It is allowed to include multiple LoadModule parameters.
#
# Mandatory: no
# Default:
LoadModule=librust_dummy.so
```

Finally, install the `librust_dummy.so` library in the path to
which `LoadModulePath` setting points to, so that the Zabbix services
can find and load the library.

```bash
$ sudo install target/release/librust_dummy.so /usr/lib/zabbix/modules
```

We can now actually test our Rust module for Zabbix by using the
`zabbix_get(8)` tool.

Lets get some random numbers by using the `rust.random` Zabbix item.

```bash
$ zabbix_get -s 127.0.0.1 -p 10050 -k rust.random
13559620181874123117
```

Every time we call `rust.random` item we should get a random number
generated from Rust.

And now lets test the `rust.echo` Zabbix item as well.

```bash
$ zabbix_get -s 127.0.0.1 -p 10050 -k rust.echo['Rust is awesome!']
Rust is awesome!
```

## Conclusion

Throughout this post we have seen how Rust can be used in places
where you would usually use C and that is probably one of the features
that makes Rust a language with great potential. Of course Rust is
more than just that - Rust has a really nice and vibrant community
behind it and a constantly growing ecosystem of libraries that you
could pick from.

I'm still making my baby steps with Rust and learning the language,
but so far I really enjoy coding in it and I'm definitely looking at
my next project to develop in Rust!
