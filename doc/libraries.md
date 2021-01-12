# Libraries

It is possible to write fairly large programs using just definitions, but they
tend to become rather messy and hard to maintain. Programming languages usually
allow encapsulating related definitions into a library or a module, which has
a well defined interface or interfaces. In Owl these are called libraries.
R7RS Scheme happened to defined modules almost exactly equally to Owl libraries,
so the naming was converted to match that of R7RS.

A library consists of a listing of other libraries it depends on, a listing
of values it intends to expose, and the
actual definitions of the values. A simple library can be defined and imported
for use as follows:

   > (define-library (my test)
        (import
           (owl base)) ;; required
        (export
           barrow
           barrow?)
        (begin
           (define barrow (cdr '(wheel barrow)))
           (define barrow?
              (lambda (x)
                 (eq? x barrow)))))
   ;; Library (my test) added
   > (import (my test))
   ;; Imported (my test)
   > barrow
   '(barrow)
   > (barrow? barrow)
   #true

Owl contains a number of builtin libraries. Some of them are general purpose
ones and others were mainly needed to implement the repl and compiler. A
listing of readily available libraries can be shown with the #{,libraries} REPL
command. They are also available in the #{*libraries*} variable, which is
an association list of library names and the values of the library. There is 
nothing magical about libraries - they are just values like everything else. 

Libraries can be defined anywhere at toplevel whether using the REPL or loading
files. There is however a naming and loading convention, which makes it easier
to load libraries. If a library #{(foo bar baz)} is to be imported and one is
not already loaded, then Owl will automatically attempt to load it from
#{foo/bar/baz.scm}.


If you know the name of the value you would like to import, but don't know the
library, then you can search for if using #{,find}.

   > ,find stat
   ,find stat
   current toplevel: *state*, type-thread-state
      (owl base): type-thread-state
      (owl sys): stat
      (owl core): type-thread-state
   > (import (owl sys))
   ;; Imported (owl sys)
   > (assoc 'size (stat "Makefile" #t))
   '(size . 3869)

