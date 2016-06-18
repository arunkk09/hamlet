#usage: ds dr fur nr rgvar nl seed
#Vary nr
ds=4
dr=4
fur=1.0
rgvar=1.0
nl=1000
skewp=0.1
echo "$ds $dr 0.0625 1 $rgvar $nl $1 $skewp"
time python gendata-onexr.py $ds $dr 0.0625 1 $rgvar $nl $1 $skewp
echo "$ds $dr 0.25 4 $rgvar $nl $1 $skewp"
time python gendata-onexr.py $ds $dr 0.25 4 $rgvar $nl $1 $skewp
echo "$ds $dr 0.5 10 $rgvar $nl $1 $skewp"
time python gendata-onexr.py $ds $dr 0.5 10 $rgvar $nl $1 $skewp
for nr in 40 100 400 1000
	do echo "$ds $dr $fur $nr $rgvar $nl $1 $skewp"
	time python gendata-onexr.py $ds $dr $fur $nr $rgvar $nl $1 $skewp
done
