[Mirah](http://www.mirah.org/)
==============================

Install

```sh
$ jruby -S gem install mirah
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

I am stuck right now b/c I don't know how to get ARGV. I generally think
Java's way of passing it as an arg to some main function is the right
way to go. But in this case, it means that there isn't (AFAIK) anything
in the enviornment I can get it off of, and since mirah doesn't set it,
I don't know what to do now.

Downloading the jar wound up being the easiest way to go, far better than
executing like 3 processes or whatever (chruby in order to set jruby,
jruby in order to invoke mirah, and then finally mirah). Not only is that
confusing, but it just takes an ungodly amount of time. Using the jar directly
wound up being simpler and faster. Also, I think I fucked up when I was
using jruby and wound up running my mirah code with JRuby instead of Mirah >.<
(this is the best explanation I have for why I was able to access ARGV earlier)

Anyway, I enjoyed it, but it seems abandoned at this point. Still, I'm hoping
there's some way to access ARGV, b/c otherwise it's maybe unreasonable
to include it in the list.
