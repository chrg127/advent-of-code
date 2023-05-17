#!/bin/bash
NUM=0
PASSP=

check() {
    case "$1" in
        "byr") if [[ ${#2} -eq 4 && "$2" -gt 1919 && "$2" -lt 2021 ]]; then echo 1; else echo 0; fi;;
        "iyr") if [[ "${#2}" -eq 4 && "$2" -gt 2009 && "$2" -lt 2021 ]]; then echo 1; else echo 0; fi;;
        "eyr") if [[ ${#2} -eq 4 && "$2" -gt 2019 && "$2" -lt 2031 ]]; then echo 1; else echo 0; fi;;
        "hgt")
            NUM=$(cut -b -$(( ${#2} - 2 )) <<< "$2")
            case "$(rev <<< $(cut -b -2 <<< $(rev <<< "$2")))" in
                "cm") if [[ "$NUM" -gt 149 && "$NUM" -lt 194 ]]; then echo 1; else echo 0; fi;;
                "in") if [[ "$NUM" -gt 58 && "$NUM" -lt 77 ]]; then echo 1; else echo 0; fi;;
            esac;;
        "hcl")
            if [[ $(cut -b 1 <<< "$2") == '#' ]]; then
                NUM=$(cut -b 2- <<< "$2")
                if [[ "$NUM" =~ ^[0-9A-Fa-f]{1,}$ ]]; then echo 1; return; fi
            fi
            echo 0;;
        "ecl")
            case "$2" in
                "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth") echo 1;;
                *) echo 0;;
            esac;;
        "pid") if [[ ${#2} -eq 9 ]]; then echo 1; else echo 0; fi;;
        "cid") echo 1;;
    esac
}

parse() {
    while IFS=' ' read -ra fields; do
        if [[ "${#fields[@]}" -eq 8 ]]; then
            for i in "${fields[@]}"; do
                RES=$(check $(cut -b -3 <<< "$i") $(cut -b 5- <<< "$i"))
                if [[ "$RES" -eq 0 ]]; then
                    echo "INVALID: " "$PASSP" "reason:" "$i"
                    return
                fi
            done
        elif [[ "${#fields[@]}" -eq 7 ]]; then
            for i in "${fields[@]}"; do
                if [[ $(cut -b -3 <<< "$i") == "cid" ]]; then echo "INVALID: " "$PASSP" "reason: missing field"; return; fi
                RES=$(check $(cut -b -3 <<< "$i") $(cut -b 5- <<< "$i"))
                if [[ "$RES" -eq 0 ]]; then
                    echo "INVALID: " "$PASSP" "reason:" "$i"
                    return
                fi
            done
        else echo "INVALID: " "$PASSP" "reason: missing fields"; return; fi
    done <<< "$PASSP"
    echo "VALID: " "$PASSP"
    (( NUM+=1 ))
}

while read -r line; do
    if [ -z "$line" ]; then
        parse "$PASSP"
        PASSP=
    else
        PASSP=""$PASSP" "$line""
    fi
done < input4.txt
parse "$PASSP" # last line because there's no newline after it
echo "$NUM"
