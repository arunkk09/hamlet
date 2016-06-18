#usage: ds dr fur nr rgvar nl seed
#Vary dr
ds=2
nr=100
fur=1.0
rgvar=1.0
nl=1000
for dr in 1 2 4 6
	do echo "$ds $dr $fur $nr $rgvar $nl $1"
	time python gendata-xsfk.py $ds $dr $fur $nr $rgvar $nl $1
done
echo "$ds 8 0.3905 $nr $rgvar $nl $1"
time python gendata-xsfk.py $ds 8 0.3905 $nr $rgvar $nl $1
echo "$ds 10 0.0976 $nr $rgvar $nl $1"
time python gendata-xsfk.py $ds 10 0.0976 $nr $rgvar $nl $1
