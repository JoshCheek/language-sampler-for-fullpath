[IO](http://iolanguage.org/)
============================

Repl
----

```
$ io
```


Interpret
---------

```
$ io fullpath.io
```


Thoughts
--------

I really enjoy this language, it makes me reconsider
possibilities more than most others.

It took me a really long time to figure out how to
work with strings. Apparently you have to call `asMutable`
before you can do anything with it.

The `not(true)` returning `nil` was really annoying.
I eventually discovered there is a `true not` which
returns the right thing. (Actually, as I write this,
I realize what's happening, I'm invoking `not` on the
Lobby, eg `Object clone not`) That is ridiculous,
it should have blown up then with an error saying that
I gave it 1 arg instead of 0 -.-

It took me like 45 minutes to figure out how to run
a system command. I did have to clone down their source
code to eventually get it figured out. And there was
a misleading comment in the method I needed, saying
to not use it. Also, I'm not super stoked about the result,
specifically my code has feature envy.

The introspection is nice, but I seemed to have difficulty
at times getting the info I wanted. Not entirely sure why
(I was using `protos` and `slotNames`).

The docs were okay, I struggled from something, not sure what,
maybe signal vs noise ratio.

On the whole, it was a good experience,
a bit of polish would have gone a long way, though.
