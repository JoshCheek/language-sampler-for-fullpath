Fortran
=======

* Apparently Fortran programs have to begin with 6 leading spaces b/c they used to be written on punch cards or smth.
* Functions are called procedures
* A list of intrinsic (builtin) [procedures](https://gcc.gnu.org/onlinedocs/gfortran/Intrinsic-Procedures.html#Intrinsic-Procedures)
* [Cheatsheet](http://www.training.prace-ri.eu/uploads/tx_pracetmo/AdvFTN_handout.pdf)

Sigh, this language is miserable. It can't handle things that it doesn't
know the size of. So I just made the strings 2048 chars long, hoping that
would be enough, but AFAIK, there's nothing restricting a path to this length.

Then, I needed to read the paths in from stdin, which it can't handle b/c
it doesn't know how long the input line is going to be. So I had to read it
in a character at a time, checking each one to see if it's a newline or end of input,
and copying it into an array of strings to build up the current input line.

Then it can't deal with the fact that I don't know how many paths are going
to be on standard input. So I had to keep two pointers, one containing the values
read in so far, and another that I would allocate a new array of strings for
every one that I read in. Then I'd copy all the existing ones over, add the one
I'd just read in, free the memory for the old one, and update the pointer to
the newly allocated one.

Then it has no way to pass standard input to a program that it's run,
so I have to invoke echo and pass the string to print as argv, using the
shell to pipe it into the real program.

But that doesn't work when I have multiple paths, because I don't have
a single string I'm printing, instead I'm printing bits as I iterate
over the list of paths. So I tried dynamically allocating a giant string
and copying them (again, char by char) into that string as I was printing,
but, b/c I'd set each path to 2048 chars, I had more buffer than chars in it,
and I apparently can't trim the buffer because it was dynamically allocated.

So now, I have to iterate over every fucking path to count how big it is so
that I can exactly calculate the length I'm going to print, meaning the whole
"just allocate more space than you need" approach fucked me in the end anyway.

This is not a good language. It's also super slow when it should be at least as fast as C.
Its only redeeming quality is that it compiles fast.
