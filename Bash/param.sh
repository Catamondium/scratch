#!/bin/bash
# parameter expansion
# https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html

# '#' match beginning pattern (match from beginning)
echo ${1#*:} # print 1st param with shortest '*:' case omitted
# '%' match trailing pattern  (match from end)
echo ${1%:*} # print 1st param with shortest ':*' case omitted
