#usage: ds dr fur nr rgvar nl seed
#Vary nl
ds=2
dr=4
fur=1.0
nr=40
rgvar=1.0
for nl in 500 1000 2000 4000 8000 16000
	do echo "$ds $dr $fur $nr $rgvar $nl $1"
	time python gendata-xsfk.py $ds $dr $fur $nr $rgvar $nl $1
done
