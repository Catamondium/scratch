#!/bin/bash
# parameter expansion
# https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html

echo ${1#*:} # print 1st param with shortest '*:' case omitted
echo ${1%:*} # print 1st param with shortest ':*' case omitted
