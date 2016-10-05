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
