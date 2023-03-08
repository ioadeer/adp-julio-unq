for f in *.csv; do
  mv "$f" \
    "$(awk -F '[._]' '{ si = sprintf("%03s", substr($1,3));
                          print $1 "." $7 }' <<<"$f")"
done

