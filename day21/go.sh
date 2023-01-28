function go 
{
    cp main.hs $1.hs
    cat $1.txt | sed -e 's/:/ =/' | sed -e 's|/|`div`|' >> $1.hs
    ghc $1.hs
    ./$1
}

go test
go input
