#!/bin/sh

$@ -e '(import (scheme base)) (char-ready?)'
echo a | $@ -e '(import (scheme base)) (char-ready?)'

