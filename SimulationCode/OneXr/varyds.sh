#usage: ds dr fur nr rgvar nl seed
#Vary ds
dr=4
nr=40
fur=1.0
rgvar=1.0
nl=1000
skewp=0.1
for ds in 1 2 4 6 8 10
	do echo "$ds $dr $fur $nr $rgvar $nl $1 $skewp"
	time python gendata-onexr.py $ds $dr $fur $nr $rgvar $nl $1 $skewp
done
