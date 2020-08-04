Owl Lisp is a simple programming language. It is essentially equivalent to the
Scheme programming language developed at MIT, but follows its tradition of
simplicity and orthogonality even further at the expense of some standards
compatibility. The most obvious change is the removal of all mutable data
structures and assignments.

Simplicity and flexibility are similarly important goals in the implementation.
Owl runs on top of a tiny (20-40KB) portable standalone virtual machine written
in C. Programs can be typed to REPL interactively, run from script files,
compiled to bytecode for running on the plain VM, or compiled via C to
standalone executables. The C output is standalone, so programs can be shipped
as C-files.

Currently Owl powers for example the text editor used to write this page, the
shell the editor is running on top of, and the tools used to generate this web
page. Some programs are available via package managers on various platforms.
Efficiency of these actual programs is the main speed criteria. This makes
features such as startup time and memory efficiency more relevant than
synthetic benchmarks. There is room for optimization, for currently simplicity
is a more relevant goal than speed.

#index

# Getting Started

## Requirements

## Building

Owl consists of a single binary #{ol}, which contains the REPL, compiler and
builtin libraries. Releases have a precompiled #{ol.c} file, which can be
downloaded and used as follows:

   $ curl https://haltp.org/files/ol-0.1.23.c.gz \
      | gzip -d \
      | gcc -x c -O2 -o ol -
   $ ./ol
   You see a prompt.
   > (cons 1 (list 2))
   '(1 2)
   > ,quit
   bye bye _o/~

This version takes very little time to compile with a C-compiler, because the 
code is not optimized.

Alternatively you can download all of the sources and make a traditional install.

   $ git clone https://gitlab.com/owl-lisp/owl.git
   $ cd owl-lisp
   $ make


## Installation

You can install owl and the manual pages with #{sudo make install} after building 
the sources or a release tarball.

Alternatively you can just build ol and copy it somewhere convenient. 


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

