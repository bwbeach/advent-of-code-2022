function go 
{
    cp main2.hs $1.hs
    cat $1.txt | \
        sed -e 's/:/ =/' | \
        sed -e '/[0-9]/s/=/_ =/' | \
        sed -e '/=.*[a-z]/s/\([a-z][a-z]*\)/\1 x/g' | \
        sed -e 's/^humn.*/humn x = x/' | \
        sed -e '/^root/s/[-+*/]/==/' | \
        sed -e 's|/|`div`|' \
        >> $1.hs
    ghc $1.hs
    ./$1
}

go test
go input
