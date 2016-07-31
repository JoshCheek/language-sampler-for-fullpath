Language Sampler For Fullpath
=============================

Fullpath is a program I initially wrote in Ruby
(source is [here](https://github.com/JoshCheek/dotfiles/blob/3cabefef72d2d97639f213726b8cd2550740ea7b/bin/fullpath)).
It makes a good small but still useful program (I use it very frequently).
So I've started using it to try out new languages, and am consolidating the results here.

**NOTE: THIS ONLY WORKS ON OSX** (due to use of the `pbcopy` program)

Languages: [go](#go), [ruby](#ruby), [elixir](#elixir), [common lisp](#common-lisp),
[kotlin](#kotlin), [python](#python), [java](#java).


Tests
-----

Install Ruby. There are any number of options here, I use chruby, so will show that one.

```sh
# install ruby-install (lets you install different ruby versions)
$ brew install ruby-install

# install chruby (lets you change between the installed rubies)
$ brew install chruby

# install the specific ruby
$ ruby-install ruby 2.3.0

# switch to it
$ chruby use 2.3.0

# now that you have Ruby, install the dependencies you need to run the tests
$ gem install bundler
$ bundle install
```

Build all implementations and run all tests:

```sh
$ rake
```

See which language tests you can run:

```sh
$ rake -T
```


Languages
---------

### [Go](https://golang.org/)

Install (for Mac, anyway):

```sh
$ brew install go
```

Build:

```sh
$ go build -o golang/fullpath golang/fullpath.go
```

Run with the interpreter:

```sh
$ go run golang/fullpath.go path1 path2
```


### [Ruby](https://www.ruby-lang.org/en/)

This one is the first one, I wrote a long time ago,
and here I started plaing around with different options there
(ie its the only one that reads from stdin and writes to
stdout in real-time).

Installation instructions are above.

Run with the interpreter:

```
$ ruby ruby/fullpath path1 path2
```

Note that it is also a valid script, so you can omit the interpreter and just run with:

```
$ ruby/fullpath path1 path2
```


### [Elixir](http://elixir-lang.org/)

[Video](https://vimeo.com/170785037) of me writing the code (my first 3 hours of Elixir,
from no experience to getting `fullpath` written in it). I felt okay with it,
but got stuck on stupid things for really long times. Eg the different kinds of strings
and mix.

Install

```sh
$ brew install elixir
```

Build:

```sh
$ cd elixir
$ mix escript.build
```

Interpret (stupidly it has to cd into the directory for mix to work correctly, tried futzing around with
[these](https://github.com/elixir-lang/elixir/blob/master/lib/mix/lib/mix.ex#L162-L168)
options, but couldn't get it t.t

```sh
$ elixir/runner a b c
```


### [Common Lisp](http://www.sbcl.org/)

Thoughts: I found this one pretty frustrating. Eventually
switched from Clisp to SBCL, because I couldn't get it to
omit the newline. Spent hours on that. Also spent a really
long time getting it to wait for the output of pbcopy,
finally hard-coded the path to the executable, but that's
a bit frustrating. Also, the compile options, as you will see,
are a bit much.


Install

```sh
$ brew install sbcl
```

Build:

```sh
$ sbcl --load common_lisp/fullpath.lisp --eval "(save-lisp-and-die \"common_lisp/fullpath\" :executable t :toplevel 'main :save-runtime-options t)" --end-toplevel-options
```

Interpret:

```sh
$ sbcl --noinform --load common_lisp/fullpath.lisp --eval '(main)' --eval '(quit)' --end-toplevel-options a b c
```


### [Kotlin](https://kotlinlang.org)

A fellow at a bar was talking about this language,
so [@mlarraz](https://github.com/mlarraz) and I sat down to try
our hand at it. We got a reasonable bit figured out that night,
and then I spent another couple of hours finishing / polishing today.

I don't have any real insights into it as a language,
other than I like the `it` thing within the blocks,
I like the `val`/`var` thing, I dislike the startup time,
my syntax highlighter needs to be better,
I like the fn bodies that omit the curlies,
Java is a little bit annoying.


Install

```sh
$ brew install kotlin
```


### [Python](https://www.python.org)

Wound up at a polyglot meetup, quite a few humans there knew some Python,
so I started writing it in Python. Went really smoothly,

so [@mlarraz](https://github.com/mlarraz) and I sat down to try
our hand at it. We got a reasonable bit figured out that night,
and then I spent another couple of hours finishing / polishing today.

I don't have any real insights into it as a language,
other than I like the `it` thing within the blocks,
I like the `val`/`var` thing, I dislike the startup time,
my syntax highlighter needs to be better,
I like the fn bodies that omit the curlies,
Java is a little bit annoying.

Install (I just ran with the version that was pre-installed, 2.7.10)



### [Java](https://docs.oracle.com/javase/8/)

I already knew a little bit of Java, and I had a working Kotlin solution,
which I figured could be switched over pretty easily.
So the first bit went pretty smoothly and was somewhat enjoyable.
When I got to the bits that obviously need map/filter/etc,
I couldn't figure out how to get them working.
Maybe I was too tired (haven't retried this morning),
but I really struggled to get the examples in the blogs working
(why don't they post full working solutions? -- my guess, Java is too verbose).
So, anyway, simple things would up taking a stupid amount of obfuscating code.

The solution isn't very fast becuse it takes too long to load everything up,
IDK if that's a fault of the JVM or of Java not doing something like dead-code elimination.
Maybe there are flags I could give it to improve this.

Figuring out how to package it was a pain in the ass.
I finally settled for "blow the whole thing away and rebuild it from scratch whenever there is a change".
The time difference is basically unnoticable, and it makes the build way simpler.

Also, the shell script executable trick is kind of ridiculous. But w/e.

Install (IDK, I already had Java 8).

Build/run:

```sh
$ javac Fullpath.java
$ java Fullpath
```

There is no interpreter AFAIK...



### [Bash](fill this in when I get internet)

Was with a friend who does sysadmin work, so he knows a ton of bash.
That made the whole thing go much smoother, really. I'd just shout at him
whenever I was confused. On the whole, it's reasonably decent.

Install (IDK, I already had bash 4.3).

Build: none

Interpret:

```sh
$ bash fullpath a b
```

REPL:

```sh
$ bash -l
```



### [C](fill this in when I get internet)

I did some C a while back, and you can use the man pages, so I wasn't at
such a disadvantage without internet. In the end, this is a miserable language.
Manual memory management makes it very difficult to do things well, it's just
much easier to let the function know about the caller and not have to set up
some contract about who is in charge of what memory.

With a decent string / array / subprocess library, this would have been a lot less painful.
But I have no clue how to locate C libraries (also I didn't have internet to install them).

I find it easier to think about pointer arithmetic than track tons of variables.
The lack of blocks means that logic of what you want to do has to be coupled to logic of
how to iterate over the collection. Sometimes the collection is a PITA to iterate over,
making the logic much harder to think about than it should be.
I enjoyed getting to use a goto. I should maybe go back in and check to see if there are
good opportunities to use a macro.

Docs:

```sh
$ man stdio   # man page for the header file, includes a list of functions with short descriptions
$ man malloc  # docs on the malloc function
$ man 3 exec  # exclusively search C Library functions (b/c there is more than one match)
$ man 2 open  # exclusively search system calls
```

Build and run:

```sh
$ gcc fullpath.c -o fullpath    # -o sets the output filename
$ ./fullpath
```

Interpret: No fkn clue.


<a href="http://www.wtfpl.net/"><img src="http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl.svg" height="20" alt="WTFPL" /></a> License
-------------------------------------------------------------------------------------------------------------------------------------------

    Copyright (C) 2016 Josh Cheek <josh.cheek@gmail.com>

    This program is free software. It comes without any warranty,
    to the extent permitted by applicable law.
    You can redistribute it and/or modify it under the terms of the
    Do What The Fuck You Want To Public License,
    Version 2, as published by Sam Hocevar.
    See http://www.wtfpl.net/ for more details.
