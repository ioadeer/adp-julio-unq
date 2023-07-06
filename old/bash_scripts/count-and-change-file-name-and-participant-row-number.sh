#!/bin/bash

# aca usamos el numero de la cuenta del archivo para poner en las filas de n de participante 
# numeros con los apellidos
# cortesia de chat gpt

count=1
for file in *; do
    if [[ -f "$file" ]]; then
        extension="${file##*.}"
        newfile="test_$(printf '%03d' "$count").$extension"
        formatted_count="$(printf '%03d' "$count")"
        if [[ $extension == "csv" ]]; then
            awk -F, 'NR > 1 && NR<45{$1=count;}1' count="${formatted_count}" OFS=, $file >> $newfile
            let count++
        fi
    fi
done

