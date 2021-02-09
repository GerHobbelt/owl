#!/bin/sh

# below does not work in background subshell
# $@ -e '(import (scheme base)) (char-ready?)'
echo a | $@ -e '(import (scheme base)) (delay 100) (char-ready?)'

