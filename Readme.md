Language Sampler For Fullpath
=============================

<strong>
[Bash](#bash),
[C](#c),
[C Sharp](#c-sharp),
[Clojure](#clojure),
[Common Lisp](#common-lisp),
[Crystal](#crystal),
[Dart](dart),
[Elixir](#elixir),
[Elisp](emacs),
[Fish](#fish),
[Go](#go),
[Haskell](#haskell),
[Haxe](#haxe),
[IO](io),
[Java](#java),
[JavaScript](#javascript),
[Julia](#julia),
[Kotlin](#kotlin),
[Mirah](mirah),
[PHP](#php),
[Python](#python),
[Ruby](#ruby),
[Rust](#rust),
[Scheme](scheme),
[Swift](swift)
</strong>

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

Potential Future Languages
--------------------------

Probably next up: CoffeeScript, Fortran

APL, ActionScript, Ada, Algol, AppleScript, Assembly (I've already got this partially working, but it's a pita -.-),
Awk, Befunge, Brainfuck, C++, Cobol, CoffeeScript, ColdFusion, Coq, D, Dart,
Delphi/Object Pascal, Dogescript, Eiffel, Erlang, F#, FORTRAN, Forth,
Groovy, Idris, Ioke, J, LLVM, Lua, Modula-2, MoonScript, Neko, Nim, Newspeak, OCaml, Objective-C,
Opal, Oz, Perl, Prolog, Pure Data, PureScript, Qbasic, R, Racket, Rebol, Rubinius Bytecode,
Scala, Self, Smalltalk, SuperCollider, Snobol, Tcl, TypeScript,
Ubercode, Unicon, Umple, VB.net / Visual Basic, VimL, Zsh


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
and then I spent another couple of hours finishing / polishing the next day.

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
Python seems like a reasonable language.

Install (I just ran with the version that was pre-installed, 2.7.10)

REPL: `$ python`

[here](http://stackoverflow.com/questions/1006169/how-do-i-look-inside-a-python-object)
are some ways to introspect. In particular, `dir(obj)` returns an array of answers,
`help(obj)` (at least in the REPL) prints a reasonably nice set of docs.




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



### [Bash](https://www.gnu.org/software/bash/)

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



### [C](https://en.wikipedia.org/wiki/C_%28programming_language%29)

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



### [Clojure](http://clojure.org)

Mostly I found it frustrating.

In part because, like all JVM environments,
it just takes so fucking long to load. This was partially mitigated by the trampoline thing,
but still, I have very little patience for such things, and I actively want to avoid
developing such a patience as I consider it harmful. There should be a way to leave the
code loaded up and just reload/compile/run the file, but it wasn't obvious to me how to do it.
I searched for the answer and only found the trampoline thing, which helped a bit,
but then behaviour differed when I'd run with the trampoline vs without it.

This was generally a problem, something would work in the REPL but not the real program.
The print thing wouldn't work in the real program unless I flushed the pipe.
The sh command caused the real program to freeze.
Docs didn't bring this stuff up, or if it did, not in an obvious place (ie the examples).

There seems to be too many collection types / independent methods for each.

It also won't let you nest lambdas within each other, which seems bizarre to me,
given that it's a functional language. I'm guessing there's some second way to make a lambda.

Clearly, I am cranky right now. Likely in part due to this,
and in part due to other shit that has nothing to do with Clojure.
But while that is true, don't let it soften the criticism of the absurd startup time.


Run annoyingly slowly:

```sh
$ lein run
```

Run slowly:

```sh
$ export LEIN_FAST_TRAMPOLINE=y
$ lein trampoline run "$@"
```

There's some whole packaging thing that you can use to get an executable
jar file, but it's kind of complicated. I don't remember how I did it,
just that I tried a lot of shit until it workd.


### [JavaScript](https://github.com/tc39)

This one was legitimately fun to write, and I'm satisfied with my solution,
which often doesn't happen. Even to the point of considering adding unit tests.

I used Node 4.2.1

Install node

```sh
$ brew install node
```

Run

```sh
$ node ./fullpath
```

Repl

```sh
$ node
```


### [PHP](http://php.net)

On the whole, I enjoyed PHP. I think the `<?php ... ?>` thing is kind of dumb,
but whatever. I like that closures have to declare what variables they are going
to enclose. Not sure how I feel about the passing by reference, it was used
reasonably in setting the child process' pipes, but a better approach would still
be to pass a function in. The toplevel function thing is kind of dumb to me,
eg all the methods that begin with `array_` because they aren't namespaced.
It would be nice to have a better array literal syntax. The arrays as hash maps
seems like a hack to me. The interfaces to some of the functions was inconsistent,
eg `array_filter($array, $callback)` vs `array_map($callback, $array)`.
It was pretty fast, which was nice (faster than JS, for example,
though common lisp was about twice as fast). I like their community's large
set of examples in the docs, though they did get in the way when searching
for the function I wanted. Maybe putting the function list on the left on the
website would alleviate that?


Docs: [http://php.net/manual/en/index.php](http://php.net/manual/en/index.php)

Install: idk, I already had php 5.5.34 on my computer

Run

```sh
$ php ./fullpath
```

Explore

```sh
$ php -r '$a = array("a", "", "b", "-h", "c"); echo var_dump($a);'
```

Useful tips:

* The whole thing is run inside an erb-like script, you can swtich into php like this: `<?php Print "Hello" ?>`
* Arrays are really hash maps
* `var_dump($argv)` prints out arrays


### [Haskell](https://www.haskell.org/)

On the whole I enjoyed it. Would be nice if the prelude had a bit more in it,
I switched to using Cabal because I didn't have a way to split a string.

Build in raw haskell:

```
$ ghc -o fullpath fullpath.hs
```

Interpret raw haskell:

```
$ runhaskell fullpath.hs
```

REPL

```
$ ghci
```

Cabal is the package manager, you can start a new cabal project with
(paraphrasing [this](https://wiki.haskell.org/How_to_write_a_Haskell_program#Structure_of_a_simple_project)):

```
$ cabal update
$ cabal init
$ cabal sanxbox init
$ cabal install -j    # this one builds it, but there's got to be a better way than this, it just took forever every single time >.<
```

Then add dependencies to the file `MyProj.cabal` in the `build-depends` key.


### [Haxe](http://haxe.org)

This was pretty fun, and pretty quick. Haxe is a pretty cool language.
The semicolons are annoying as fuck, I constantly forgot to put them in.
I **really** like the `using` feature, which takes functions and makes
them methods on their first argument. A nice way to provide a minimal interface
on the object, but allow it to be expanded on as if it had a larger interface,
but without actually polluting the object.

I compiled the backend to C++ since that compiles down to an executable,
saving me from having to deal with shebangs or that ridiculous JVM hack I'm using.
C++ is also very fast, so after compiling, it takes very little time to run.

Download the haxe installer (mine is v3.3): https://haxe.org/download/

Set up haxelib (you only have to do this once), which is their package manager:

```sh
$ haxelib setup         # hit enter to accept the default location
```

Download the C++ backend:

```sh
$ haxelib install hxcpp # install the c++ backend
```

Compile for C++

```sh
$ haxe -main Fullpath -cpp cpp
$ cpp/Fullpath a b c
```

Run in interpreted mode (unfortunately, doesn't work well with our command-line args):

```sh
$ haxe --interp -main Fullpath
```

REPL:

```sh
$ haxelib install ihx # interactive hax
$ haxelib run ihx
```

### [Fish](https://fishshell.com)

This was fun and went pretty smoothly.
I'm not super stoked about the conditionals, but they're not terrible, either.

Install:

```sh
$ brew install fish
```

REPL:

```sh
$ fish
```


### [Rust](https://www.rust-lang.org)

Painful. This required a lot of focus, so any little thing (eg the dogs feeling antsy) took a big toll.
Eventually I had to go read the Rust book, which did do a good job of explaining the issues I was hitting,
and the error messages are trying really hard to be helpful, which I absolutely love.
But still, if the language wasn't so opaque, it wouldn't need to rely so heavily on documentation
(really, documentation is a code smell because it's a crutch you lean on when the UX doesn't make it
obvious what you need to do -- if Rust were better in some way that IDK what it is, then I wouldn't
need to fallback to docs so heavily). The docs site is pretty good, but there seems to be a lot of duplicate
entries. Also, I swear the types kept shifting out from underneath me. The pattern matching is annoying.

It's a bit verbose, the syntax is kind of annoying, I'm not smart enough, apparently, to understand
the type system (even after having read about it, and often times knowing what I wanted to say, I either
couldn't figure out how to say it, or didn't understand why it didn't like my code,
or what some annotation I might add would wind up doing). Opening the process and writing to its standard
input was incredibly difficult to figure out, and led to like a core dump or something for a long time.
Here's an example of the difficulty of the type system, I wanted to take the output value of `spawn`,
which is defined [here](https://github.com/rust-lang/rust/blob/3c5a0fa45b5e2786b6e64e27f48cd129e7aefdbd/src/libstd/process.rs#L459),
and pass it to a function. Spawn's signature looks like this: `pub fn spawn(&mut self) -> io::Result<Child>`,
but I just could not figure out how to get the function to take that `io::Result<Child>`.
It would say things like: `expected type \`&mut std::result::Result<&mut std::process::Child, std::io::Error>\`, found type \`std::result::Result<_, _>\``
I know I got `Child` to expand to the correct value, but they kept wanting two values for `io::Result`,
or I would think they were considering it to be some other class, IDK.
In the end, it meant I had to skip that step of the refactoring I was doing.

It's a way better language than C, and probably worth just gritting my teeth
and bearing through the vertical learning curve so that I don't ever have to write C again.

The compiler is fast, and it starts up very fast, both of these are of extreme importance,
so huge props on that one, that goes a long way (allows me to just try out 10 different things
in the span of a minute, encourages me to run my code often, etc).

Go was way easier to learn, but I feel like their type system is a bit gimped, and I have no desire to
constantly reimplement map/filter/reduce/etc, Rust seems promising in these areas. I suspect Rust is a
better language than Go, but that learning curve is harsh, I think there's more to
be done with it, but working with it is just exhausting.

Install:

```sh
$ brew install rust
```

Build:

```sh
$ rustc fullpath.rs -o ./fullpath
```

Build with cargo:

```sh
$ cargo build
```

(Read here if yoou want to know more: https://doc.rust-lang.org/book/getting-started.html)


REPL: Didn't try to find one


### [C Sharp](http://www.mono-project.com)

Overall, this was a good experience. Nothing stood out as wonderful or horrible,
but the whole thing took about 2 hours, I never got really stuck, nothing in the language
really annoyed me. I didn't figure out the rules for when to use capital letters and
when not to. To some extent, it seemed like whatever I guessed would work most of the time.
I feel like there's some nice support for functional collection methods (map, reduce, etc)
that would have been cool to play with, but didn't try that hard to figure them out sicne
I mostly fetl like just getting through it.


Install

```sh
$ brew install mono
```


`hello_world`.cs:

```cs
using System;

public class HelloWorld {
    static public void Main () {
        Console.WriteLine ("Hello Mono World");
    }
}
```


```sh
# compile
$ mcs hello_world.cs

# run
$ mono hello_world.exe
```


### [Crystal](https://crystal-lang.org)

This went smoothly and was a lot of fun.
The binary executes quickly, which is nice.
The docs were hard to get the information I wanted from,
but once I cloned the repo, was able to quickly find the things I was looking for.
Their tests were really helpful for figuring out how to use some of the less obvious features.
I wouldn't mind writing more crystal in the future :)


Install

```sh
$ brew install crystal-lang
```


`hello_world.cr`:

```cs
puts "hello world"
```

Interpret:

```sh
$ crystal run hello_world.cr
```

Compile:

```sh
$ crystal build hello_world.cr --release
```


### [Julia](http://julialang.org/)

This went super smoothly, super quickly, and I enjoyed it!
Alltogether, I think it was about an hour from figuring out how to install it to getting it running.
That's a really short rampup time, IMO.
Biggest hurdle was I'd get the args backwards to things like `map`,
and the error message wasn't very helpful.
I didn't find the docs to be super helpful, but the REPL was really nice,
the `?` form of the repl was good enough most of the time.
I was able to look at their tests to figure out things like the `readandwrite` function.
I was pretty tired as I started this, I think I even dozed off for 5ish minutes in the middle of it,
so for me to succeed while doing it is a good sign.

Downside is it's a bit slow to startup, Cucumber reports the tests for Julia took 11.8 seconds,
Crystal took 0.8s by comparison. I mention that one because is Julia's most obvious competitor
from my perspective (they're very similar in my brain, at least, though that's probably unfair
to both languages since I put about 0 effort into understanding what makes them special).
C took 0.8s, as well.


Install

```sh
$ brew cask install julia
```


`hello_world.jl`:

```cs
println("hello world")
```


Interpret:

```sh
$ julia hello_world.jl
```


REPL

```sh
$ julia
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
