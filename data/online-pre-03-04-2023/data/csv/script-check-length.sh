# Este script es para contar cuantas lineas hay en el csv.
# consideramos validos 44 lineas ya que son 30 respuestas (6*5)

for f in *.csv; do
  if [ $(wc -l "$f" | awk '{print $1;}') -eq 44 ]
  then
    echo "$f is valid ... moving to valid directory"
    mv "$f" ./valid 
  else
    echo "$f is not valid ... moving to not_valid directory"
    mv "$f" ./not_valid 
  fi
done

