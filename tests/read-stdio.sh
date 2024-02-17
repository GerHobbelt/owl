#!/bin/sh

echo "(foo (bar baz) 42)" | $@ -e '(import (scheme read)) (cadr (read))'

echo "foo (bar baz) 42" | $@ -e '(import (scheme read)) (read-all)'
