#!/bin/bash
#GNU bash, version 4.3.48(1)-release (x86_64-pc-linux-gnu)

: '
# Function testing
function gettime () {
	response=`addtime $1 $2`
	echo -e "gettime: $*\n$response"
	return 0
}

start=3:30
length=60
gettime $start $length
gettime 0:00 60
echo "Returned: $?"
'

# Boolean/conditionals testing
bool=$true
num="10"
str="cat"
if [ $bool -eq $true ] && [ $num -gt 0 ] && [ "$str" = "cat" ]; then
	echo "Correct"
else
	echo "Error"
fi

: '
# Array testing
arr=(`seq 5 -1 1`)
echo -e "Array:\t${arr[*]}\nlength:\t$arr"

# Input testing
i=0
for v in "$@"; do
	echo -e "loop: $v\t index $i"
	((i++))
done

echo "back to main"
echo "$# params"
'

