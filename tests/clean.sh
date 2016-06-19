for i in $(echo `ls | grep testcase`) ; do
    if [ -e "$i/$i.ende" ]; then
	rm -f "$i/$i.bc"
    fi
done
