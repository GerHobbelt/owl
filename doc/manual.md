Owl Lisp is a simple purely functional programming language. It is essentially
R7RS Scheme, apart from having only immmutable data structures and relying on
multithreading for some core operations. This document describes the language,
its implementation and builtin libraries. 

# LISP

LISP (or Lisp) is short for LISt Processor. It is a mathematical formalism for
reasoning about computations invented by John McCarthy at MIT in 1958. Although
the system started out as a purely theoretical construct, it soon ended up
being implemented in practice.

One of the main features of Lisp is the syntax, or to be more precise, the
lack of it. As the name suggests, Lisp is indeed good for processing lists.
Symbols are also a primitive data type. List structures containing symbols and
other primitive data types are colled S-expressions. These have a simple
textual representation. A symbol is just the sequence of letters in its name,
and a list is just a sequence of other things separated by spaces and delimited
by parenthesis. A key feature of Lisp is that programs themselves are just Lisp
data structures. Therefore a separate syntax is not needed for the programming
language - it is just the textual representation of Lisp data.

The initial definition of Lisp in "Lisp 1.5 programmer's manual" contained a
definition of Lisp itself in itself. This evaluation function was the only
thing that needed to be implemented to turn the initial version of Lisp from
theory to practice.

There have been many major dialects of Lisp, and countless minor ones. These
days compiler construction and parsing are usually taught towards the end of
computer science curriculum, if at all. Parsers and compilers, the building
blocks of programming language implementations, are considered to be dark magic
only a few select pupils and devout lone programmers are fit to approach. This
has not always been the case. In a few programming language families it has
been customary to start, not end, studies by building a small version of the
language you are studying. This approach favored languages which had a powerful
small core of features, on top of which you could build the rest of the system.
Forth and Lisp are common examples of such languages.

Common Lisp and Scheme are some of the most widely used major Lisp dialects.
Scheme was also developed at MIT. It took a step towards λ-calculus in its
semantics by relying on one namespace and lexical scoping. It was initially
also an attempt to explore the actor model of computation, but the actor part
was dropped after it turned out to be equivalent to functions in
single-threaded context. While it has good support for functional programming,
it also allows one define and use other programming paradigms. One of the key
concepts added in Scheme were continuations, which allow the state of the
program to be captured and restored. This makes it possible define e.g.
resumable error handling and co-operative multithreading as library functions.
There have at the time of writing been 7 revisions of the Scheme language
standard.


## Owl Lisp

Owl Lisp is essentially a small step from the multi-paradigm world of Scheme
into a purely functional one. Scheme programs are usually mainly functional
with some mutations sprinkled sparingly where needed. In Owl, functional
programming style is not just encouraged - it is all you have. Owl also brought
back the actor-model -style operation, but this time by using
continuation-based threads to make them useufl. Apart from being based on
continuations, the threading is modeled after that in Erlang.

The goal has not at any point been to become an ultimate Lisp and take over
the world. Ïn fact, this has been an anti-goal. The goal has been to remain
simple while incrementally growing only portable features required to enable
building the kinds of programs it is actually used for. While this is a
somewhat circular definition, it has worked surprisingly well. Owl is shaped by
minimalism and practical applications, not by what seem like cool and powerful
language features.




## Installation

To install system-wide to /usr
```
   $ make
   $ sudo make install
```

Alternatively you can try it out with
```
   $ make
   $ cp bin/ol /somewhere/convenient
   $ /somewhere/convenient/ol
   You see a prompt
   >
```


## Files

   bin/ol      - the owl interpreter/compiler
   c/ovm.c     - the virtual machine / shared owl lisp runtime
   owl/*.scm   - implementation of owl repl and compiler
   fasl/*.fasl - bytecode images for bin/vm used during boot
   bin/vm      - plain VM used during boot
   c/ol.c      - combined VM and REPL heap image


## Usage

Owl can be used either interactively, or to interpret code from files,
or compile programs to fasl-images or c-files. The difference between
an owl program and a plain script is that the program should just have
a function of one argument as the last value, which will be called with
the command line argument list when the program is executed.

In addition to being a regular interpreter, owl also tries to make it
easy to compile programs for different platforms. Owl programs can be
compiled with ol to C-files, which can be compiled to standalone binaries
without needing any owl-specific support files or libraries. The C files
also work on 32- and 64-bit systems, and compile as such at least on
Linux, OpenBSD, and macOS or can be cross-compiled to Windows executables
with MinGW.

For example, to build a hello world program:
```
  $ echo '(lambda (args) (print "Hello, world!"))' > hello.scm
  $ ol -o hello.c hello.scm
  $ gcc -o hello hello.c
  $ ./hello
  Hello, world!
```

Or simply:
```
  $ echo '(λ (args) (print "Hello, world!"))' \
     | ol -x c | gcc -x c -o hello - && ./hello
```

Parts of the compiled programs can be translated to C, instead of being
simply fasl-encoded, to increase speed. Using the --native flag compiles
most of the bytecode to C, and --usual-suspects compiles typically used
functions. To make programs run faster, one can use for example:

```
  $ ol -O2 -o test.c test.scm && gcc -O2 -o test test.c
```

# Libraries

Libraries are named by lists of symbols. For example `(owl lazy)` is
a library name. `ol` comes preloaded with many libraries, some of which
are loaded by default to REPL. If you want to use exported definitions from a
builtin library `(owl lazy)`, you can do so by issuing `(import (owl lazy))`.

Notice that `import` is not a function. It is one of the special forms used
by the REPL to handle management of visible definitions in your sessions. The
syntax is the same as in imports within a library.

If you try to import a library which is not currently loaded, say `(my test)`,
Owl would try to look for library definition from the file "my/test.scm". If
it is available and contains definition of a library called `(my test)`, it will
be loaded and added to your REPL.

You can get a listing of the currently loaded libraries with `,libraries` command.
Many of them are mainly needed for the implementation of the underlying system.
Some of the likely useful ones are documented below.
