set xdata time
set timefmt "%s"
set format x "%m/%d"
set terminal pdf size 10in,7.5in
set output "promises-wc.pdf"
set xlabel "date"
set ylabel "words"
set title "promises word counts by author"
set datafile missing "?"
plot 'promises-wc.data' using 1:($2) title "sophical", 'promises-wc.data' using 1:($3) title "ekate", 'promises-wc.data' using 1:($4) title "brians", 'promises-wc.data' using 1:($5) title "nhinchen"
