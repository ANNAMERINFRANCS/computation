reset
n=20 
max=1. #max value
min=0.
width=(max-min)/n
#function used to map a value to the intervals
set term png
set output "uniform.png"
hist(x,width)=width*floor(x/width)+width/2.0
set xrange [min:max]
set yrange [0:]
#to put an empty boundary around the
#data inside an autoscaled graph.
set offset graph 0.05,0.05,0.05,0.0
set xtics min,(max-min)/5,max
set boxwidth width*0.9
set style fill solid 0.5 #fillstyle
set tics out nomirror
set xlabel "x"
set ylabel "Frequency"
#count and plot
plot "uniformno.dat" u (hist($1,width)):(1.0) smooth freq w boxes lc rgb"red" notitle
