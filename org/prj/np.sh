#!/bin/bash

function np() {
    local tbl=({0..9} {A..H} {J..N} {P..Z} {a..k} {m..z});
    # echo "tbl=${tbl[@]}"
    # echo "len_tbl=${#tbl[@]}"
    read -rs pass
    # echo "pass=$pass"
    alfa=`echo $pass | sha256sum | tr -d " -" | tr a-z A-Z`
    # echo "alfa=$alfa"
    beta=`bc <<< "obase=58;ibase=16;${alfa^^}" | sed -z 's/\\\\\n/ /g'`
    # echo "beta=$beta"
    read -ra gamma <<< "$beta"
    # echo "gamma=${gamma[@]}"
    for idx in "${gamma[@]}" ;
    do
        printf %s ${tbl[ "16#$idx" ]};
    done;
    echo
}

np
