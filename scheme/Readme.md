Scheme
======

I'm using Chicken Scheme.

Install
-------

```sh
$ brew install chicken
```

Interpret
---------

```sh
$ csi fullpath.scm
```


Build and run
-------------

```sh
$ csc fullpath.scm
$ ./fullpath
```

Thoughts
--------

It was fun to write lisp again, but I had the hardest time figuring out what
I could do in this environment. There's a lot of lisp docs out there,
and I frequently wound up at the wrong ones. This wouldn't really be a problem,
but they're apparently different enough that I many of the instructions for
other lisps just didn't work here.

I also was almost never able to guess what the function name should be.

When I went to their docs specifically, they frequently didn't work.
Eg I never figured out how to get the list functions working, so I had
to write my own `any?` function, even though this is really their `ormap` function.
I think some of the difficulty is that half the packages seem to be named
`srfi-#` which is totally meaningless to me, and I may have misspelled the `srfi` part.

Anyway, it compiled reasonably quickly.
