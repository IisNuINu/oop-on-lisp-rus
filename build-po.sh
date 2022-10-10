#!/bin/sh

IN_DIR="./en"
PO_DIR="./po"

#ls ${IN_DIR}/*.html

for name in `ls ${IN_DIR}/*.md`
do
     echo "find: $name"
     #echo $name | sed -n 's/\/(.*)\.html/1~/p' | cat
     to_name=`echo $name | sed -ne "s/^.*\/\([^./]*\)\.md/\1/;p"`
     txt2po -i $name -o ${PO_DIR}/${to_name}.po
done

