Language Sampler For Fullpath
=============================

Fullpath is a program I initially wrote in Ruby
(source is [here](https://github.com/JoshCheek/dotfiles/blob/3cabefef72d2d97639f213726b8cd2550740ea7b/bin/fullpath)).
It makes a good small but still useful program (I use it very frequently)
to try out for a new language, so I'm going to try writing it in
new languages as I try to learn them.

Tests
-----

Build all implementations and run all tests:

```
$ rake
```

See which language tests you can run:

```
$ rake -T
```

Languages
---------

### Go

Build:

```sh
$ go build -o golang/fullpath golang/fullpath.go
```

Run with the interpreter:

```sh
$ go run golang/fullpath.go path1 path2
```


### Ruby

Run with the interpreter:

```
$ ruby ruby/fullpath path1 path2
```

Note that it is also a valid script, so you can omit the interpreter and just run with:

```
$ ruby/fullpath path1 path2
```


### Elixir

[Video](https://vimeo.com/170785037) of me writing the code (my first 3 hours of Elixir,
from no experience to getting `fullpath` written in it).

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


<a href="http://www.wtfpl.net/"><img src="http://www.wtfpl.net/wp-content/uploads/2012/12/wtfpl.svg" height="20" alt="WTFPL" /></a> License
-------------------------------------------------------------------------------------------------------------------------------------------

    Copyright (C) 2016 Josh Cheek <josh.cheek@gmail.com>

    This program is free software. It comes without any warranty,
    to the extent permitted by applicable law.
    You can redistribute it and/or modify it under the terms of the
    Do What The Fuck You Want To Public License,
    Version 2, as published by Sam Hocevar.
    See http://www.wtfpl.net/ for more details.
