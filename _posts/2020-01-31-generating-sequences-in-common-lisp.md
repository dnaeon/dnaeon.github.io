---
layout: post
title: Generating sequences in Common Lisp
tags: lisp programming
---
Python provides a useful function for enumerating a sequence of
numbers in the form of the [range
function](https://docs.python.org/3/library/functions.html#func-range).
If you are looking for `range` function in Common Lisp you would find
lots of various implementations.

I think this is a good exercise to practice in any new language you
are learning, so here I'm going to post my own implementation of
`range` in Common Lisp.

First, we are going to implement [iota
sequences](https://en.wikipedia.org/wiki/Iota) which we will later use
as a helper for implementing `range`. The following implementation of
`IOTA` uses the
[DO](http://www.lispworks.com/documentation/HyperSpec/Body/m_do_do.htm)
iteration macro.

``` common-lisp
(defun iota (n &key (start 0) (step 1))
  "iota using DO iteration macro"
  (when (minusp n)
    (error "Invalid number of items specified"))
  (do ((i 0 (1+ i))
       (item start (+ item step))
       (result nil (push item result)))
      ((= i n) (nreverse result))))
```

We can now use `IOTA` to generate a sequence of numbers, e.g.

``` common-lisp
CL-USER> (iota 10)
(0 1 2 3 4 5 6 7 8 9)
CL-USER> (iota 10 :start 0 :step -1)
(0 -1 -2 -3 -4 -5 -6 -7 -8 -9)
```

With slight modifications to above function we can turn it into a
macro.

``` common-lisp
(defmacro with-iota ((var n &key (start 0) (step 1)) &body body)
  "Generates a sequence of numbers and bind VAR to each item in the sequence"
  (let ((i-var (gensym))
        (num-items (gensym)))
    `(let ((,num-items ,n))
       (when (minusp ,num-items)
         (error "Invalid number of items specified"))
       (do ((,i-var 0 (1+ ,i-var))
            (,var ,start (+ ,var ,step)))
           ((>= ,i-var ,num-items))
         ,@body))))
```

The reason we use `num-items` above is because we want to evaluate the
form represented by `n` only once. We could have used
`alexandria:once-only` macro for that as well, but I wanted to keep
things self-contained here. Example usage of our `WITH-IOTA` macro
looks like this.

``` common-lisp
CL-USER> (with-iota (i 5)
           (format t "Got number ~a~%" i))
Got number 0
Got number 1
Got number 2
Got number 3
Got number 4
NIL
```

Another possible implementation of `IOTA` would be to make it generate
new items from the sequence when needed. The following implementation
behaves like a
[generator](https://en.wikipedia.org/wiki/Generator_(computer_programming)). Basically
what the function does is to create a
[closure](https://en.wikipedia.org/wiki/Closure_(computer_programming))
over the number of items we need to produce.

``` common-lisp
(defun iota-generator (n &key (start 0) (step 1))
  (let ((i 0))
    (lambda ()
      (unless (>= i n)
	(prog1 start (incf start step) (incf i))))))
```

Example usage of our `IOTA-GENERATOR`. The generator will return `NIL`
when the sequence has been exhausted.

``` common-lisp
CL-USER> (defparameter *iota-gen* (iota-generator 3 :start 42 :step -42))
*IOTA-GEN*
CL-USER> (funcall *iota-gen*)
42
CL-USER> (funcall *iota-gen*)
0
CL-USER> (funcall *iota-gen*)
-42
CL-USER> (funcall *iota-gen*)
NIL
```

Calling `(funcall *iota-gen*)` from the REPL is fine, but what we would
really want to do is to wrap our generator in a macro like this for example.

``` common-lisp
(defmacro with-iota ((i n &key (start 0) (step 1)) &body body)
  (let ((generator-var (gensym)))
    `(let ((,generator-var (iota-generator ,n :start ,start :step ,step)))
       (loop for ,i = (funcall ,generator-var) then (funcall ,generator-var)
	     while ,i do
	       ,@body))))
```

Having sorted out `IOTA` we can now go ahead and implement the `RANGE`
function. Here's one possible implementation of `RANGE`.

``` common-lisp
(defun range (start stop &key (step 1))
  "range implemented using IOTA"
  (when (and (> start stop) (plusp step))
    (error (format nil "Invalid positive step: start=~a, stop=~a, step=~a" start stop step)))
  (when (and (< start stop) (minusp step))
    (error (format nil "Invalid negative step: start=~a, stop=~a, step=~a" start stop step)))
  (when (zerop step)
    (error (format nil "Invalid zero step: start=~a, stop=~a, step=~a" start stop step)))
  (let ((n (abs (ceiling (- start stop) (abs step)))))
    (iota n :start start :step step)))
```

Most of the code above is just validations to make sure that we have
valid inputs, while the actual generation of the sequence is just a
couple of lines. Here's how we can use our `RANGE` function.

``` common-lisp
CL-USER> (range 1 10)
(1 2 3 4 5 6 7 8 9)
CL-USER> (range 10 1 :step -1)
(10 9 8 7 6 5 4 3 2)
```

We could have implemented `RANGE` without using `IOTA`, but re-using what we
already have is always a good thing. If we wanted to implemented `RANGE`
without having to rely on `IOTA` we could have implemented a recursive
solution as well, e.g.

``` common-lisp
(defun range (start stop &key (step 1))
  "Recursive RANGE function"
  (when (and (> start stop) (plusp step))
    (error (format nil "Invalid positive step: start=~a, stop=~a, step=~a" start stop step)))
  (when (and (< start stop) (minusp step))
    (error (format nil "Invalid negative step: start=~a, stop=~a, step=~a" start stop step)))
  (when (zerop step)
    (error (format nil "Invalid zero step: start=~a, stop=~a, step=~a" start stop step)))
  (labels ((recur (i acc)
	     (cond
	       ((and (minusp step) (<= i stop)) (nreverse acc))
	       ((and (plusp step) (>= i stop)) (nreverse acc))
	       (t (recur (+ i step) (push i acc))))))
    (recur start nil)))
```

Note, that instead of re-inventing the wheel over and over again you
would probably want to use the functions already provided by libraries
such as
[Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html)
and [series](https://www.cliki.net/Series). The code provided here is
merely used for practicing purposes.
