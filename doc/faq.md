# FAQ

#*Q: Where can I get help?*: #'You can stop by at #owl-lisp on libera.chat',
file tickets to #[https://gitlab.com/owl-lisp/owl,gitlab] or send email to
aki.helin@iki.fi.

#*Q: How can I use third party libraries?*: Grab them to source directory and
include them. #{(import (foo bar))} attempts to load #{./foo/bar.scm}, which
should have #{(define-library (foo bar) ...)}

#*Q: The error messages suck.*: True. Current best practice in Owl programs is
not to make errors.

#*Q: Why is it not called a Scheme?*: I don't want people filing issues about
#{set-car!} not working.

#*Q: Is this the last question?*: No, that one was already asked some years ago.

#project #owl #post
