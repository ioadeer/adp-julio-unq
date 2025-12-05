count=1
for file in *; do
    if [[ -f "$file" ]]; then
        extension="${file##*.}"
        newfile="$(printf '%03d' "$count").$extension"
        formatted_count="$(printf '%03d' "$count")"

        if [[ $extension == "csv" ]]; then

            awk -F, -v OFS=, -v count="$formatted_count" '
                NR==1 { gsub(/\r/, ""); print $0,"participant_n"; next }
                { gsub(/\r/, ""); print $0,count }
            ' "$file" > "$newfile"

            ((count++))
        fi
    fi
done
