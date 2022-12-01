---
layout: post
title: Common Lisp web development and the road to a middleware
tags: common-lisp lisp ningle fukamachi/ningle middleware
---
I've been looking recently for a Common Lisp system, which would allow
me to expose a service I am developing over a [REST
API](https://en.wikipedia.org/wiki/Representational_state_transfer)
and honestly the options available right now in the Common Lisp
ecosystem may be a bit overwhelming at first.

If you are like me and going through the same exercise of finding a
system to develop a simple REST API you will be hearing a lot about
`hunchentoot`, `lack`, `clack`, `caveman`, `caveman2`, `ningle`,
`snooze`, `radiance`, `cl-rest-server`, `woo`, `wookie`, etc.

The list goes on and on ... Prepare yourself for some reading what
exactly each system is and what it is meant to be used for. Hint: some
of these are web servers, others are web frameworks, and others are
micro web frameworks.

The [Common Lisp
Cookbook](https://lispcookbook.github.io/cl-cookbook/web.html) does
provide a good introduction to the various systems and what they do,
so make sure to check it out.

[The State of Common Lisp,
2022](https://lisp-journey.gitlab.io/web-dev/#web-application-environments)
also provides a good overview of the different options at your
disposable, but after reading it you will probably be even more
confused as to which one to use.

The [Why Hunchentoot instead of
Clack?](https://www.darkchestnut.com/2019/http-server-why-hunchentoot-instead-clack/)
article also makes some good points to think about when considering
one or the other, so check that one as well.

In my case I was looking for a simple web framework, which would allow
me to develop a simple REST API. Something along the lines of
[ring](https://github.com/ring-clojure/ring) in Clojure, which I've
used before, when I was doing some Clojure.

With that said I've looked into
[caveman](https://github.com/fukamachi/caveman) first, as this seemed
to be what people have been recommending. `caveman` is a
"batteries-included" web framework for Common Lisp. It appears to be
the [Django](https://www.djangoproject.com/) for Common Lisp, and
comes with nice goodies such as template systems, database
integration, configuration system, project skeletons, routing macros,
etc. I've tried out `caveman` and while I do liked it, it is a bit too
much for my simple use cases.

Another thing to keep in mind about `caveman` is that it is based on
`clack`, which is based on `lack`. What that means is that now you
have to learn all the abstractions the framework is built on top of.
Again, comparing that to [ring](https://github.com/ring-clojure/ring)
it is a bit of an overkill to me, so I've decided to keep looking
elsewhere for now.

One thing worth mentioning here is that if you decide to use `clack`
as a web framework it abstracts away the web server specifics by
providing a unified API. What that means is that you could be running
`hunchentoot` or other servers from your `clack` app, which is a nice
thing.

After some time (and lots of reading and testing things out), I've
reduced the list of systems I want to evaluate more to just two --
[snooze](https://github.com/joaotavora/snooze) and
[ningle](https://github.com/fukamachi/ningle).

Both of them provide interesting features, and approach the same
problems in different ways. For example in `snooze` a route is modeled
around CLOS and uses generic functions and the accompanying methods,
which is really nice, because you could decorate your HTTP routes with
`:before`, `:after` and `:around` methods. I really like that one! I'm
planning to spend more time soon with `snooze`, but on the surface -
it seems to be what I look for.

`ningle` is nice as well, and one of the features that are interesting
to me is the ability to define additional [requirements per
route](https://github.com/fukamachi/ningle#requirements). Getting
started with `ningle` is easy when following the README documentation,
but you will need to read more about
[clack](https://github.com/fukamachi/clack) and
[lack](https://github.com/fukamachi/lack) as well in order to fully
understand the design choices in `ningle`. An honestly `clack` doesn't
provide much of a documentation, so this makes it a bit of a challenge
initially. Compare that with the [ring
documentation](https://github.com/ring-clojure/ring/wiki), which is
great in my opinion, as it provides everything you need to get
started.

Okay, this opening paragraph ended up much longer than what I've
planned initially, but I wanted to give you a bit more context as to
what I was going for.

In a future post I may write more about my experience with web
development in Common Lisp, but before that let's get to the point of
this post -- how to create custom middlewares in
[ningle](https://github.com/fukamachi/ningle), as this has been
something I really wanted to get working, and the system doesn't
support it.

First, what is a middleware? A middleware is a function which wraps an
existing application and returns a new application. A typical use case
for a middleware is to provide logging capabilities for your web
service, where each request/response is logged as it passes through
between the client and the server. Other use cases for middlewares is
the ability to push (or inject) additional context to your HTTP route
handlers, e.g. a database connection which will be re-used by all HTTP
handlers (routes in `ningle` terms).

The [documentation of
ningle](https://github.com/fukamachi/ningle#using-session), clearly
states that "you can use other Lack middlewares with ningle".

While that is true, and you can use middlewares in `ningle`, and these
middlewares can interact with the surrounding HTTP request
environment/context before they hit the actual HTTP handler/route, it
appears that the HTTP handlers/routes in `ningle` are somewhat limited
when comparing them with a handler/route written in `lack`.

Here's an example of a HTTP route/handler from the [lack
documentation](https://github.com/fukamachi/lack):

``` common-lisp
(lambda (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, World")))
```

It is just a regular lambda function, which takes a single argument --
[the environment of your
app](https://github.com/fukamachi/lack#the-environment). Now, you can
use this environment, and in fact that's what middlewares do in order
to push additional context to your app. This is what a typical
middleware looks like in `lack`.

``` common-lisp
(defun middleware (app)
  (lambda (env)
    ;; preprocessing
    (let ((res (funcall app env)))
      ;; postprocessing
      res)))
```

You can then use this middleware to wrap an existing app and provide a
different behaviour when your route is called.

You could create a middleware that looks like this.

``` common-lisp
(defun middleware-message (app)
  (lambda (env)
    (setf (getf env :message) "A message from the environment")
    (funcall app env)))
```

If you enable this middleware using `lack:builder` now you can access
the message that was pushed into the environment by our middleware.

``` common-lisp
(lack:builder
  #'middleware-message
  (lambda (env)
     `(200 () (getf env :message))))
```

While this is a pretty contrived example, I hope you get the idea. You
could use middlewares for things such as pushing a database
connection, so that your HTTP routes can use it, when needed.

So, back to `ningle`. In `ningle` the HTTP handler/route looks like
this.

``` common-lisp
(defvar *app* (make-instance 'ningle:app))

(setf (ningle:route *app* "/hello/:name")
      #'(lambda (params)
          (format nil "Hello, ~A" (cdr (assoc :name params)))))
```

Again we see the well-known lambda handler, but this time the
lambda-list is different -- your `ningle` route will receive the HTTP
request params, and not the surrounding environment. And that is a bit
of an issue here, because if you want to create a custom middleware,
which touches the environment there is no way your route will ever be
able to get to it.

The README provides a section about [ningle
context](https://github.com/fukamachi/ningle#context), which seems to
be what you should use, but after trying that out that doesn't seem to
be the case. Here's the example from the README.

``` common-lisp
(setf (context :database)
      (dbi:connect :mysql
                   :database-name "test-db"
                   :username "nobody"
                   :password "nobody"))

(context :database)
;;=> #<DBD.MYSQL:<DBD-MYSQL-CONNECTION> #x3020013D1C6D>
```

You may end up with the impression that this thing will set things up
for you and you can use the `context` from within the routes and you
will be right. The problem is that `(setf (ningle:context ...))` works
only **within** the routes. And that is too late in the game already.

The funny thing is that the `(ningle:context)` is derived from the
same `environment` that we've seen before in the `lack` routes, but it
is stripped down to just a few things -- the `request`, `response` and
the `session`, which is set up by the `lack.middleware.session`
middleware.

So, to sum it up -- if you need to create custom middlewares in
`ningle`, which interact with the `environment` you are stuck, because
your HTTP routes won't be able to use it. This appears to be a known
thing and was [reported ~ 7 years
ago](https://github.com/fukamachi/ningle/issues/6). Before I
completely scrap `ningle` of the list of frameworks I'd like to use
I've decided to give it a stab and see if I can make it work.

Checking the code, the method of interest is [this
one](https://github.com/fukamachi/ningle/blob/master/app.lisp#L90),
which defines a method for `lack:call` of our ningle app.

``` common-lisp
(defmethod call ((this app) env)
  "Overriding method. This method will be called for each request."
  (declare (ignore env))
  (multiple-value-bind (res foundp)
      (dispatch (mapper this) (request-path-info *request*)
                :method (request-method *request*))
    (if foundp
        res
        (not-found this))))
```

As you can see the `environment` is being ignored here, and if you
look at the code of `DISPATCH` function you will find the parts which
invoke our routes and pass down the parsed HTTP request params.

So, to fix this I've created a new sub-class of `ningle:app` which
overrides the `lack:call` method, but also dynamically binds the
`environment`, so that at least it is present when HTTP routes need
it. So, time to make things work. Fire up your REPL and load the
systems we will use.

``` common-lisp
(ql:quickload :ningle)
(ql:quickload :lack)
(ql:quickload :clack)
```

And now we will define our sub-class of `ningle:app` and provide our
custom implementation of `lack:call`. The important bits here are that
we are dynamically binding `*REQUEST-ENV*`.

``` common-lisp
(defparameter *request-env* nil
  "*REQUEST-ENV* will be dynamically bound to the environment context
of HTTP requests")

(defclass app (ningle:app)
  ()
  (:documentation "Custom application based on NINGLE:APP"))
 
(defmethod lack.component:call ((app app) env)
  ;; Dynamically bind *REQUEST-ENV* for each request, so that ningle
  ;; routes can access the environment.
  (let ((*request-env* env))
    (call-next-method)))
```

Here's an example middleware we are going to use, which pushes a
message to the environment, which will later be used by our example
route.

``` common-lisp
(defun my-middleware (app)
  "A custom middleware which wraps a NINGLE:APP and pushes additional
metadata into the environment for HTTP routes.
  (lambda (env)
    (setf (getf env :my-middleware/message) "my middleware message")
    (funcall app env)))
```

Okay, that's pretty much it. Time to create an app instance and test
things out.

``` common-lisp
(defparameter *app* (make-instance 'app))

(setf (ningle:route *app* "/")
      (lambda (params)
        (declare (ignore params))
        (getf *request-env* :my-middleware/message)))

(defparameter *wrapped-app* (lack:builder #'my-middleware *app*))
(defparameter *server* (clack:clackup *wrapped-app*))
```

This should get your app started and listening on the default `5000`
port. Finally we can query our simple API and see if things work.

``` bash
$ curl -X GET -vvv http://localhost:5000/
*   Trying 127.0.0.1:5000...
* Connected to localhost (127.0.0.1) port 5000 (#0)
> GET / HTTP/1.1
> Host: localhost:5000
> User-Agent: curl/7.86.0
> Accept: */*
> 
* Mark bundle as not supporting multiuse
< HTTP/1.1 200 OK
< Date: Thu, 01 Dec 2022 17:48:18 GMT
< Server: Hunchentoot 1.3.0
< Transfer-Encoding: chunked
< Content-Type: text/html; charset=utf-8
< 
* Connection #0 to host localhost left intact
my middleware message
```

Great, things work as expected now and we are able to interact with
the `environment`, so our middlewares can do their job. Ideally, this
should be fixed in `ningle` itself, as it makes more sense to be that
way in my opinion.

You can find the full [code
here](https://gist.github.com/dnaeon/3a3f86dea1096db5a9231d1f56a565e2).

Next, I will be reviewing `snooze`, before I make up my mind between
`ningle` and `snooze`. Till next time!
