DIR=$1
for f in $(find $DIR -name '*.pdf'); do
    # if F is a .pdf file
    echo $f
    [ -f "$f" ] || continue
    [ ${f: -4} == ".pdf" ] || continue
    pdfcrop $f
    mv ${f%.pdf}-crop.pdf $f
done