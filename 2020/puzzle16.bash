#!/bin/bash
RANGE1_ST=()
RANGE1_END=()
RANGE2_ST=()
RANGE2_END=()

add_to_arrs() {
    RANGE1_ST+=("$1")
    RANGE1_END+=("$2")
    RANGE2_ST+=("$3")
    RANGE2_END+=("$4")
}

# $1: the number to check, $2: the index into the arrays
check_num() {
    if [[ "$1" -ge ${RANGE1_ST["$2"]} && "$1" -le ${RANGE1_END["$2"]}
       || "$1" -ge ${RANGE2_ST["$2"]} && "$1" -le ${RANGE2_END["$2"]} ]]; then
        echo 0
    else
        echo 1
    fi
}

PARSING_HEADER=true
ERRORS=()
while read LINE; do
    if [[ "$PARSING_HEADER" = true ]]; then
        if [[ -z "$LINE" ]]; then PARSING_HEADER=false; fi
        #echo "parsing header: ""$LINE"
        add_to_arrs $(echo "$LINE" | sed 's/[^0-9\-]/ /g' | sed 's/-/ /g')
    else
        if [[ "$LINE" == "your ticket:" ]]; then
            read dummy; read dummy; read dummy; continue
        fi
        echo "parsing numbers: ""$LINE"
        for NUM in $(sed 's/,/\n/g' <<< "$LINE"); do
            VALID=false
            for ((i = 0; i < ${#RANGE1_ST[@]}; i++)); do
                if [[ $(check_num $NUM $i) == 0 ]]; then VALID=true; fi
            done
            if [[ $VALID = false ]]; then
                ERRORS+=("$NUM")
            fi
        done
    fi
done < input16.txt

SUM=0
for NUM in ${ERRORS[*]}; do
    (( SUM += $NUM ))
done
echo "$SUM"
