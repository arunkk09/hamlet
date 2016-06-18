#usage: ds dr fur nr rgvar nl seed skewp
#Vary skewp
ds=4
dr=4
nr=200
fur=1.0
rgvar=1.0
nl=1000
for skewp in 0.0 0.01 0.05 0.1 0.25 0.5 0.75 0.9 0.95 0.99 1.0
	do echo "$ds $dr $fur $nr $rgvar $nl $1 $skewp"
	time python gendata-onexr.py $ds $dr $fur $nr $rgvar $nl $1 $skewp
done
