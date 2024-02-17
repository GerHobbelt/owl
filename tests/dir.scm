(import (owl sys))
(print (filter (string->regex "m/^dir\\.scm$/") (dir->list "tests")))
