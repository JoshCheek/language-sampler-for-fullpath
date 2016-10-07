[Mirah](http://www.mirah.org/)
==============================

You can install like this:

```sh
$ jruby -S gem install mirah
```

But there's also a jar you can download, which is quicker and removes the dependency on jruby.
In that case, do:

```sh
$ java -jar mirah-0.2.1.jar fullpath.mirah
$ java FullpathTopLevel xyz 123
```

Run

```sh
$ chruby-exec jruby-9.1.2.0 -- ruby -S mirah fullpath.mirah
```

Thoughts
--------

I really can't deal with the JVM's startup time :(

This went shockingly smoothly. I have no fucking clue what I'm doing,
didn't figure out how to read the source code, just sort of guessed a
lot by shifting between what I'd expect for Java and what I'd expect
for Ruby. The [examples](https://github.com/mirah/mirah/tree/722bb8c939866280b56fb5cc24421a895d40c6d1/examples)
were also helpful.

I got pretty stuck trying ot figure out how to get access to ARGV.
It would be really nice if they included it in the examples.
Also, probably delete [https://github.com/mirah/mirah/blob/722bb8c939866280b56fb5cc24421a895d40c6d1/TODO.md#end](https://github.com/mirah/mirah/blob/722bb8c939866280b56fb5cc24421a895d40c6d1/TODO.md#end).

Downloading the jar wound up being the easiest way to go, far better than
executing like 3 processes or whatever (chruby in order to set jruby,
jruby in order to invoke mirah, and then finally mirah). Not only is that
confusing, but it just takes an ungodly amount of time. Using the jar directly
wound up being simpler and faster. Also, I think I fucked up when I was
using jruby and wound up running my mirah code with JRuby instead of Mirah,
which led to confusion when it didn't work after I'd already seen it work.

Anyway, I enjoyed it :)
