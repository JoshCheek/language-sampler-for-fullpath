Swift
=====

Build:

```sh
swiftc -o fullpath -sdk "$(xcrun --show-sdk-path --sdk macosx)" Fullpath.swift
```

Interpret:

```sh
swift Fullpath.swift a b c
```


Get info
--------

Start typing in a playground, it will try to autocomplete which will give
you a list of what you can do.

Also, you can search Xcode's docs, which works okay, as well.
Between the two of these I was able to figure out how to get command line args.


Thoughts
--------

Xcode is really impressive! I didn't have internet for some amount of time I was working
on this, and I was able to use the type based suggestions, the playground, and the
error hints to figure out a lot of it.

On the whole, I liked it quite a bit. It did sporadically take a while to execute,
but was usually pretty good. Wish I didn't have to import Foundation and Darwin
in order to get access to things that feel like they should be rather small dependencies
(argv, for example, feels like a small dependency, but Foundation feels like a big dependency).

the function syntax seems unnecessarily verbose, like what's with the underscore space?
And why do I need a `return` keyword?

It would be nice to not need all the curly braces, eg this code:

```swift
for arg in args {
  if isCopy(arg) {
    copyOutput = true
  } else if isHelp(arg) {
    printHelp  = true
  } else if arg != "" {
    paths.append(arg)
  }
}
```

Seems like it should be able to be written like this:

```swift
for arg in args
  if isCopy(arg)
    copyOutput = true
  else if isHelp(arg)
    printHelp  = true
  else if arg != ""
    paths.append(arg)
```

I had a fun insight on this one that I could just always make the pipe
and print to it, and then choose to invoke the process or not, regardless
of whether I'd written to the pipe.

The keywords seemed strang, maybe they make sense, but they felt at the time
like they were probably Objective-C cruft.

The Array vs ArraySlice was confusing, I somehow got it into a state where I
was able to guess correctly, but never really stopped to understand it.

Needing to get the data from a string seems pointlessly confusing. I definitely
got tripped up on this the last time I tried to write Swift.

Probably took a couple of hours, which is more than it probably should have,
but still not bad, and I mostly spent my time being impressed with Xcode.

ALSO: If you like Xcode's Swift playground, that's basically what
[SiB](https://github.com/JoshCheek/seeing_is_believing/) is for Ruby ;)
