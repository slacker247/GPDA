#! /bin/sh

# Adding quotation marks at the beginning and at the end of each line
echo "Adding quotation marks..."
sed '
s/^/"/g
s/$/"/g' $1 > x

# Separating each line or record with a blank line
echo "Separating lines..."
sed '
s/$/\
/g' x > x.tmp

# Substituting each blank line with an end record identifier --ER--
echo "Adding ER identifiers..."
sed '
s/^$/--ER--/g' x.tmp > x

# Putting each field of the record in one line
echo "Making 1-line records..."
sed  '
s/:/\
/g' x > x.tmp

mv x.tmp $1.out

rm x
