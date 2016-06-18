for seed in {1..100}
	do echo seed $seed
	time sh varyds.sh $seed > mc100out/varyds-$seed.out 2> mc100err/varyds-$seed.err
	time sh varydr.sh $seed > mc100out/varydr-$seed.out 2> mc100err/varydr-$seed.err
	time sh varynr.sh $seed > mc100out/varynr-$seed.out 2> mc100err/varynr-$seed.err
	time sh varynl.sh $seed > mc100out/varynl-$seed.out 2> mc100err/varynl-$seed.err
done
