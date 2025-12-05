count=1
for file in *; do
    if [[ -f "$file" ]]; then
        extension="${file##*.}"
        newfile="$(printf '%03d' "$count").$extension"
        formatted_count="$(printf '%03d' "$count")"

        if [[ $extension == "csv" ]]; then

            awk -F, -v OFS=, -v count="$formatted_count" '
                NR==1 {print $0,"participant_n"; next}   # add new header
                {print $0,count}                         # add value to every row
            ' "$file" > "$newfile"

            ((count++))
        fi
    fi
done

