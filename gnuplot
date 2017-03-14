set ytics nomirror autofreq tc lt 1
set y2tics autofreq tc lt 2

plot "output.dat" using 2:3 linetype 1 with linespoints ti "request time in us", "output.dat" using 2:4 axes x1y2 with linespoints ti "# of failed connections"
pause -1 "Hit any key to continue"
