#!/bin/bash

# aca vamos a contar el archivo y vamos a agregar otro archivo que vincule los
# numeros con los apellidos
# cortesia de chat gpt

count=1
for file in *; do
    if [[ -f "$file" ]]; then
        extension="${file##*.}"
        newname="$(printf '%03d' "$count").$extension"
        if [[ $extension == "csv" ]]; then
            awk -F, 'NR > 1 && NR<45{$1="$(printf '%03d' "$count")";}1' OFS=, $file 
            mv -- "$file" "$newname"
            let count++
        fi
    fi
done

