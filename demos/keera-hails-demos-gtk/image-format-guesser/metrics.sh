#!/bin/bash
FILES=$(find src/ | grep '.hs$')
NLINES=0
NHLINTMSGS=0
for i in $FILES; do
 # hlint $i
 NHLINTMSGS_FILE=$(hlint $i | grep $i | wc -l)
 NHLINTMSGS=$((NHLINTMSGS + NHLINTMSGS_FILE))
 # echo Number of hlint messages for \"$i\": $NHLINTMSGS_FILE
 NLINES_FILE=$(grep -ve '^\s*\(--.*\)\?$' $i | wc -l) ;
 NLINES=$((NLINES + NLINES_FILE)) ;
 # echo Number of lines for \"$i\": $NLINES_FILE
done
echo Total number of hlint messages: $NHLINTMSGS
echo Total number of lines: $NLINES
