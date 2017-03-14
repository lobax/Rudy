#!/usr/bin/python
import sys, math, os

if len(sys.argv) != 2:
	print("usage: calculate_speedup <word count>")
	sys.exit(1)

wordcount = int(sys.argv[1])

if wordcount < 1:
	print("error: word count must be a positive integer")
	sys.exit(1)

fh = open("graph-" + str(wordcount) + ".dat", "r")
lines = fh.readlines()
lines.append("0;0")

previous_cores = 1
execution_times = []

sequential = 0

result = open("speedup-" + str(wordcount) + ".dat", "w")

for line in lines:
	line = line.split(";")
	
	cores = int(line[0])
	execution_time = float(line[1])
	
	if cores == previous_cores:
		execution_times.append(execution_time)
	
	else:
		if len(execution_times) % 2 == 0:
			median = (execution_times[int((len(execution_times) / 2) - 1)] + execution_times[int(len(execution_times) / 2)]) / 2
		
		else:
			median = execution_times[math.floor(len(execution_times) / 2)]
		
		if previous_cores == 1:
			sequential = median
			result.write("1;1\n")
		
		else:
			result.write(str(previous_cores) + ";" + str(sequential/median) + "\n")
		
		execution_times = [execution_time]
	
	if cores != 0:
		previous_cores = cores

result.close()

os.system("echo 'set datafile separator \";\"; set yrange [0:]; set xrange [0.5:" + str(previous_cores) + ".5]; set xtics 1; set ytics 1; set terminal png truecolor enhanced font \"Tahoma,10\"; set ylabel \"speedup\"; set xlabel \"number of threads\"; set output \"speedup-" + str(wordcount) + ".png\"; set key off; plot \"speedup-" + str(wordcount) + ".dat\" with points pointsize 2 linewidth 2' | gnuplot")