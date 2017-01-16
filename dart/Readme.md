[Dart](https://www.dartlang.org/)
==============================

You can install like this:

```sh
$ brew tap dart-lang/dart
$ brew install dart
```

Run

```sh
$ dart somefile.dart
```


Thoughts
--------

Moderately impressed. I'd be okay if I had to write Dart every day. Might even
get to enjoying it eventually.

For now, the semicolons were the most painful part.
They slapped me in the face every few minutes and sometimes multiple times a minute.
Frankly, mandating semicolons feels misanthropic to me. Like what's the point?
It doesn't save the language dev any effort, it probably takes more effort because
they have to go add all the errors saying "hey, you forgot to put a semicolon here"
which means they know where the semicolons go and added unnecessary errors just
to trip me up and clutter my code.

I like the toplevel main function.

I like that I can put whatever classes in that file that I want to (unlike Java).

I didn't read anything about Dart when I started, but I eventually inferred that
it had optional types (b/c I realized I hadn't been using them and it wasn't
getting mad at me for it). I've never played with optional types before and I
have to say that I'm a fan of them. Meaning I can choose to not use them, in
which case they stay out of my way (because my experience with type systems is
that they're mostly just in the way constantly). So I can choose that flexibility
and I can choose to let part of my code run even though another part would explode.
I personally use that feature constantly, when I was giving Haxe a serious shot,
I wound up hating the type system because it couldn't facilitate that use case
so my code would just be in a state of not-running for several days. That's very
problematic because it means I can't get to the point of trying the thing I'm
thinking until after I convert the entire rest of the program to expect it to work
in the new way. But IDK what I want the new way to be, so I want to experiment in
isolation until that point and then I'll put in the work to make the program correct
again after I figure out what it should be. So Dart's optional types let me omit
them and have this ability. At the same time, types can be nice, there's a lot
of errors where it's like "that's a bug, you should have exploded helpfully!"
and the reason that the language didn't was because it has no clue what I wanted
the thing to be. So types let me get better feedback when errors occur, but it's
at the cost of flexibility. The optional types here let me choose between the two
based on my assessment of whether flexibilty or confidence and helpful errors
are more important.

I wish they'd paid more attention to Ruby because I think they got the multiline
strings wrong (which Ruby did twice before eventually introducing the `<<~` literal.
Here, you can see the help screen is super awkward. I even moved it out of the class
at one point so that it wouldn't start out indented, but then figured it shouldn't
do that because it means you can't override it in a subclass.

Their interface for calling functions was impressively consistent, parens seem to
just be an alias for invoking the method named `call`, because my Fullpath object
is able to be treated exactly like a function. Hey, ECMAScript, pay attention!

I like that they took ECMAScript's fat arrow, it's one of the best things that
ECMAScript has done to their language (homage to CoffeeScript <3). I like that
`() {}` is also function syntax. In fact, I kind of love that about it!
It allows for simple consistency across different syntaxes, eg when you realize
that, then you immediately think to try swapping the method body with a fat arrow
and you're rewarded with it working correctly!

The "generative constructor" was super cool, I get to omit the translation of
params to ivars by placing the ivar where the parameter name would have been.
And it was smart enough to let me do this with some of the params and not others.
Unfortunately, this feature is broken for other functions.
I am curious what else you could place in the params.

The `new` keyword is a mistake. I wish languages would stop copying misfeatures.
Look at the `call` method where it maps the stream several times. It has to do
`.map((bytes) => new String.fromCharCodes(bytes))` instead of
`.map(new String.fromCharCodes)` because `new` is a keyword instead of a function.
There are other problems with `new` keywords than this, but you can't see them
in the code here.

I didn't expect to be a fan of the string interpolation, but if you look at
the `path` method, that's very elegant, I really like it!
Ruby has this, too, but it never feels very natural and no one knows about it
so I feel guilty using it.  Bash has it, but bash's handling of strings is so
problematic that I'm hesitant to give them any credit for it.

There is some weird thing with lists where they can be either generative
or fixed-length. I think this should not be baked into the list class itself.
It means that some of the methods just explode on some of its values, b/c
they're for one type but being used on another. Frankly the list class does not
seem very well thought out, there was no obvious way to do a flat map, so I had
to make the `flatten` function. But that function is an obnoxious three lines
for something that should be a thin wrapper over some other function.
The method I really wanted here was Ruby's `each_with_object`, but I would have
settled for `reduce`. Unfortunately, `reduce` borders on being unusable because
it does not receive an inital value. They accomplish this by making the first
element the initial value. But that means you can't reduce to a different type
than your elements, eg I would hae wanted to do this:

```dart
lists.reduce((all, crnt) => all.addAll(crnt), [])
```

But that means the value we're reducing it to is a list while the elements are
strings. You can see in the [type signature](https://api.dartlang.org/stable/1.21.1/dart-core/Iterable/reduce.html)
that this is not allowed. Possibly I could bypass this by omitting the typing
info here. I didn't try that because of the second problem with this, I have
to have an item in the list to use it, but I have no idea if there are any
items in the list or not. So even if I could get around it by subverting the
types, I still can't in practice.

Statements are plague on programming languages. I don't understand why languages
implement this, it must surely be more effort because now anything you call
might not have a return value so instead of being able to compose any code
with any other code, now you have to check whether it's a statement or an expression.
For me as an end-user, it means that I have to use `() {}` style functions if
the body is an `if` statement, even if I want to use fat-arrow functions to get
rid of the curly braces. Also, if I refactor the body to introduce a statement
like `if`, then I may have to swap a fat arrow for `() {}` style for no reason
other than the body happens to now, arbitrarily be a "statement" instead of an
"expression". It's strange how pervasive it is.

The startup time was very good. If you've read any of the other things I've
written here, you know that matters a ton to me, so it was outright pleasurable
to have my code just instantly execute, way too many languages are moving away
from that these days. I'd frankly take a shittier language if it had a faster
startup time. Startup time means I can try 10 things in the span of a minute
and that I stay constantly engaged rather than sitting around waiting while
my mind drifts off.

So, altogether, I really like Dart. It felt a lot like Elixir to me, TBH.
But I did find it more enjoyable to write Dart than Elixir (probably due
to the startup time and not trying to use their package manager).
It was fast, intuitive, and despite a few warts, it had a lot of really nice
features and their consistency in functions felt really nice :)
