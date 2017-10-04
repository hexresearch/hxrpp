#!/bin/sh
tmp=`mktemp`
tmpvg=`mktemp`
valgrind --tool=memcheck --leak-check=yes ./test-suite $1 2>$tmpvg | sed -E 's/(#[[:digit:]]+l)/#PTR#/g' | sed 's/[ \t]*$//' > $tmp
diff t/baseline/$1-baseline $tmp
status=$?
grep 'LEAK SUMMARY' $tmpvg > /dev/null
mem=$?
echo -n "TEST " $1 $2
#[ $mem -n 0 ] && echo "FOUND MEMORY LEAK"
if [ $status -eq 0 -a $mem -eq 1 ]; then
	echo " OK"
else
	echo -n " FAIL"
	if [ $mem -eq 0 ]; then
		echo -n " (LEAK)"
	fi
	echo ""
fi
rm $tmp
rm $tmpvg
