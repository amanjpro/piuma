# merger.sed: merges two separate lines together

{
N;
s/\n/ /
}


s/^ *//
s/\([0-9][0-9]\)* *,/\1, /
