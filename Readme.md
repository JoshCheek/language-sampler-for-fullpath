Language Sampler For Fullpath
=============================

Fullpath is a program I initially wrote in Ruby
(source is [here](https://github.com/JoshCheek/dotfiles/blob/3cabefef72d2d97639f213726b8cd2550740ea7b/bin/fullpath)).
It makes a good small but still useful program (I use it very frequently).
So I've started using it to try out new languages, and am consolidating the results here.

**NOTE: THIS ONLY WORKS ON OSX** (due to use of the `pbcopy` program)


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


<a href="http://www.wtfpl.net/"><img src="http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl.svg" height="20" alt="WTFPL" /></a> License
-------------------------------------------------------------------------------------------------------------------------------------------

    Copyright (C) 2016 Josh Cheek <josh.cheek@gmail.com>

    This program is free software. It comes without any warranty,
    to the extent permitted by applicable law.
    You can redistribute it and/or modify it under the terms of the
    Do What The Fuck You Want To Public License,
    Version 2, as published by Sam Hocevar.
    See http://www.wtfpl.net/ for more details.
