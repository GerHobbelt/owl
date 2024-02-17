#!/bin/sh

ECHO=$(which echo)

$@ -e '(import (owl sys)) (print "ok") (kill (getpid) sigkill) (print "fail") (halt 0)' 2>/dev/null && echo multifail

true
