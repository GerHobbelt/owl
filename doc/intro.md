(work in progress)

Owl Lisp is a purely functional dialect of the Scheme programming language. It
is essentially equivalent to the R7RS version of Scheme, but follows its
tradition of simplicity and orthogonality even further at the expense of some
standards compatibility. The most obvious change is the removal of all mutable
data structures and assignments.

Simplicity and flexibility are similarly important goals in the implementation.
Owl runs on top of a tiny portable standalone virtual machine written
in C. Programs can be interpreted and compiled to standalone binaries.

Though it may not initially seem that way, the main reason for developing owl 
has been to write actual programs. For example the text editor used to type 
this document, and most of the tools that build this site, are owl programs.


#index

# Getting Started

## Requirements

It should be easy to get owl up and running on most somewhat POSIX-compliant
systems, such as Linux, any BSD. You should have #{make} and a working C-compiler
installed. For example in Debian-based Linux distributions you can use:

   $ sudo apt-get install gcc

You may also need #{git} and #{make} if you download the sources from git or
want to build the full source package.


## Building

The easiest option is to download the current precompiled C-version of #{ol},
and compile with with a C-compiler. Ol is the standalone repl and compiler,
which also has the builtin libraries described in this manual.

   $ curl https://haltp.org/files/ol-0.1.23.c.gz \
      | gzip -d \
      | gcc -x c -O2 -o ol -
   $ ./ol
   You see a prompt.
   > (cons 1 (list 2))
   '(1 2)
   > ,quit
   bye bye _o/~

This version of ol is compiled with no C-code optimizations, so the resulting
C-code is small and takes very little time and resources to compile.
Alternatively you can download all of the sources and make a traditional
install.

   $ git clone https://gitlab.com/owl-lisp/owl.git
   $ cd owl-lisp
   $ make


## Installation

If you just built ol, you can use it from wherever convenient. Usually it is
convenient to put such binaries to your home directory under bin/ -directory.

You can install owl and the manual pages with #{sudo make install} after building
the sources or a release tarball.


## Testing Operation

When started, owl greets the user is ready to proceed evaluating terms given to
it. This is the REPL, or read-eval-print -loop familiar from many modern
programming languages.

   $ ol
   You see a prompt.
   > (+ 1 2)
   3
   > (reverse (list 1 2 3))
   '(3 2 1)
   >

You can exit owl by pressing Ctrl-d, denoting end of input in UNIX, asking the
REPL to exit via #{,quit}, or by asking the thread scheduler to stop everything with
#{(halt 1)}.

Compiler mode can be tested for example by doing

   $ echo '(lambda (args) (print "hello world") 0)' \
      | ol  -x c -o - \
      | gcc -x c -o test -
   $ ./test
   hello world



