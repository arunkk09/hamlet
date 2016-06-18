#usage: ds dr fur nr rgvar nl seed
#Vary nl
ds=4
dr=4
fur=1.0
nr=40
rgvar=1.0
skewp=0.1
for nl in 100 250 500 1000 2000 4000 8000 16000
	do echo "$ds $dr $fur $nr $rgvar $nl $1 $skewp"
	time python gendata-onexr.py $ds $dr $fur $nr $rgvar $nl $1 $skewp
done
