#Copyright 2016 Arun Kumar
#
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.

import sys, imp, math, random
from cStringIO import StringIO
#from itertools import chain, combinations
sys.path.append("../")
import naivebayes

VERBOSE = False
#VERBOSE = True

def powerset(lst):
	return reduce(lambda result, x: result + [subset + [x] for subset in result], lst, [[]])

#sample nl examples from cumprob distr given
def sampledist(cumprobs, nl) :
	counts = [0 for _ in cumprobs] #used for counting num occur of each rv value
	for i in range(nl) :
		rv = random.random()
		lind = 0
		uind = len(cumprobs)-1
		while (1) :	#binary search 
			mind = int(math.ceil(float(uind+lind)/2))
			if (rv == cumprobs[mind]) : #check a few lower ones due to ties in probs
				tmpind = mind
				while((tmpind < len(cumprobs)) and (rv == cumprobs[tmpind])) :
					tmpind = tmpind + 1
				counts[tmpind+1] = counts[tmpind+1] + 1	#count it for the highest index+1
				#note that rv is [0,1); lower bound counted to next interval; last p is 1
				break
			elif (rv < cumprobs[mind]) :
				uind = mind
			else :
				lind = mind
			if (lind == (uind-1)) : #found the interval!
				if (cumprobs[lind] == cumprobs[uind]) : #ties need to resolved upwards
					tmpind = uind
					while ((tmpind < len(cumprobs)) and (rv == cumprobs[tmpind])) :
						tmpind = tmpind + 1
					counts[tmpind+1] = counts[tmpind+1] + 1	#count it for the highest index+1
				elif (rv < cumprobs[lind]) :
					counts[0] = counts[0] + 1 #0 wss not there in cumprobs
				else : 
					counts[uind] = counts[uind] + 1 #count it for the uind's interval
				break
	return counts

#def powerset(iterable):
#	xs = list(iterable)
#	# note we return an iterator rather than a list
#	return chain.from_iterable( combinations(xs,n) for n in range(len(xs)+1) )

#def gradloss(w, tuples) :
#	ndims = len(tuples[0]) - 2
#	rndims = range(ndims)
#	grad = [0 for _ in rndims]
#	for dat in tuples :
#	return (loss, grad)

def main() :
	# parameters from arguments
	if(len(sys.argv) < 8) :
		print >> sys.stderr, 'usage: ds dr fur nr rgvar nl seed'
		sys.exit(2)
	#fur: fraction of domain of xr contained in R (in terms of number of unique values)
	#rgvar: variance of the gaussian distr that assigns rids to unique xr vals
	#nl: large single sample of data drawn from the true prob distr
	#more parameters will be added later
	ds = int(sys.argv[1])
	dr = int(sys.argv[2])
	fur = float(sys.argv[3])
	nr = int(sys.argv[4])
	rgvar = float(sys.argv[5])
	nl = int(sys.argv[6])
	seed = int(sys.argv[7])
	d = ds + dr #Y excluded
	numD = 100 #default
	print 'ds', ds, 'dr', dr, 'fur', fur, 'nr', nr, 'nl', nl, 'seed', seed, 'numD', numD 
	random.seed(seed)

	#Now, the TPD is over Y,XS,XR; the steps are as follows:
	#1. Generate a TPD like before over for JP; ensure H(Y|XJP) = 0, i.e., no Bayes noise
	#2. Marginalize the TPD to obtain P[XR] (obviously it will still sum to 1)
	#3. Sample nr times w repl from P[XR] to generate tuples of R; assign RIDs - dom(FK)
	#4. Obtain the invmap from XR values appearing in R to sets of RID values
	#5. Create a new TPD' wherein all entries with XR not in R are pushed to 0; normalize it
	#6. Sample nl times fmor TPD' to get massivedata; thus, data for JP and IG is done
	#Introduce FK by randomly picking from invmap for each's XR value; JC and CA done too
	tpd = {}
	domxsxr = list(powerset(set(range(ds + dr)))) #list of XS,XR vals using set notations
	lendomxsxr = len(domxsxr)
	for xsxr in domxsxr :
			stup = []
			for i in range(ds + dr) :
				if (i in xsxr) :
					stup = stup + [1]
				else :
					stup = stup + [0]
			yhash = {}
			#for now, H(Y|XJP) = 0, i.e., no bayes noise! assign labels with random prob
			#if 0 :
			tmp = random.random()
			if (tmp < 0.5) :
				yhash[0] = random.random() #1 -> this was replaced to avoid uniform
				yhash[1] = 0
			else :
				yhash[0] = 0
				yhash[1] = random.random() #1 -> the randomness biases cond distr
			#TODO: the following is when H(Y|XJP) > 0, i.e., bayes noise exists
			if 0 :
				for y in range(2) :
					yhash[y] = random.random()
			tpd[tuple(stup)] = yhash
	print '|Dom(XSXR)| =',lendomxsxr,'len(tpd) =',len(tpd)

	# Normalize tpd so that allocated probs sum to 1
	sumall = 0.0
	for k,v in tpd.items() :
		for k2,v2 in v.items() :
			sumall = sumall + v2
	county0 = 0
	county1 = 0
	newsum = 0
	for k,v in tpd.items() :
		if(tpd[k][0] >= tpd[k][1]) :
			county0 = county0 + 1
		else :
			county1 = county1 + 1
		for k2,v2 in v.items() :
			tpd[k][k2] = v2/sumall
			newsum = newsum + tpd[k][k2]
	if VERBOSE :
		print tpd
	print 'newsum =',newsum,'#MAP Y0 =',county0,'#MAP Y1 =',county1

	#2. Marginalize the TPD to obtain P[XR]
	probxr = {}
	domxr = list(powerset(set(range(dr)))) #list of XR vals using set notations
	lendomxr = len(domxr)
	for xr in domxr :
		rtup = []
		for i in range(dr) :
			if (i in xr) :
				rtup = rtup + [1]
			else :
				rtup = rtup + [0]
		probxr[tuple(rtup)] = 0
	print '|Dom(XR)| =',lendomxr,'marginalizing tpd to get P[XR] of len',len(probxr)
	for k,v in tpd.items() :
		for k2,v2 in v.items() :
			probxr[k[ds:(ds + dr)]] = probxr[k[ds:(ds + dr)]] + v2
	if VERBOSE :
		print probxr

	#3. Sample nr times w repl from P[XR] to generate tuples of R; assign RIDs - dom(FK)
	# The algo is simple: generate nr U(0,1) random numbers
	# Create an array with cum probs that yield intervals that map to indexes/keys of tpd
	# For each of nr example, do binary search on prob array to find its interval/index
	# Since some probs could be 0, during search, we need to check a few nearby ones too
	# Maintain a separate array for indexes/keys of tpd to track counts of samples
	# Traverse through the counts array to emit sampled dataset

	cumprobs = []
	#counts = [0 for _ in range(lendomxr)] #used for selecting values of XR in R
	for k,v in probxr.items() :
		if (len(cumprobs) == 0) :
			newp = v
		else :
			newp = cumprobs[len(cumprobs)-1] + v
		cumprobs = cumprobs + [newp]
	print 'len(cumprobs) =',len(cumprobs)
	counts = sampledist(cumprobs, nr)
	print 'len(counts) =',len(counts)
	#4. While constructing R, also, obtain the invmap XR -> {RID} values appearing in R
	cntr = 0
	rdata = {} #hashing on rid to give tuple XR
	invrdata = {} #hashing on xr value to give list of rids; if not in R, list will be empty
	nonempty = 0
	rid = 1 #FK starts from 1
	for k,v in probxr.items() :
		thecnt = counts[cntr]
		invrdata[k] = []
		if(thecnt > 0) :
			nonempty += 1
			for i in range(thecnt) :
				rdata[rid] = list(k)
				invrdata[k] = invrdata[k] + [rid]
				rid = rid + 1
		cntr = cntr + 1
	print 'len(rdata)',len(rdata),'len(invrdata)',len(invrdata),'Count(DISTINCT XR) in R',nonempty
	if VERBOSE :
		print 'rdata:',rdata
		print 'invrdata:',invrdata
		for k,v in probxr.items() :
			print k,'has prob',v,'and so occ cnt',len(invrdata[k])
	
	#5. Create a new TPD' on Y,XS,XR with all XR vals not in R are pushed to 0; normalize it
	numzeroed = 0
	totprobloss = 0
	tpdprime = dict(tpd)
	for k,v in tpdprime.items() :
		valr = k[ds:(ds + dr)]
		if(len(invrdata[valr]) == 0) :
			numzeroed += 1
			for k2,v2 in v.items() : #set all y val probs to 0 for this
				totprobloss += v2
				tpdprime[k][k2] = 0
	print 'numzeroed',numzeroed,'tot prob loss in tpd',totprobloss,'renormalizing tpdprime of len',len(tpdprime)
	# Again, normalize tpd so that allocated probs sum to 1
	sumall = 0.0
	for k,v in tpdprime.items() :
		for k2,v2 in v.items() :
			sumall = sumall + v2
	for k,v in tpdprime.items() :
		for k2,v2 in v.items() :
			tpdprime[k][k2] = v2/sumall
	if VERBOSE :
		print tpdprime

	#6. Sample nl times from TPD' to get massivedata on Y,XS,XR; JP and IG will be done
	# The algo is simple: generate nl U(0,1) random numbers
	# Create an array with cum probs that yield intervals that map to indexes/keys of tpd
	# For each of nl example, do binary search on prob array to find its interval/index
	# Since some probs could be 0, during search, we need to check a few nearby ones too
	# Maintain a separate array for indexes/keys of tpd to track counts of samples
	# Traverse through the counts array to emit sampled dataset

	# Instead of bootstrapping, as Jerry suggested, we simply sample entirely new datasets!
	# Thus, no need to maintain and track indices; create test dataset and set aside
	# In a for loop, create training datasets and use on the fly
	# The sample size is now the training dataset size
	# The test dataset size is set to be 25% of the training dataset size  

	cumprobs = []
	for k,v in tpdprime.items() :
		for k2,v2 in v.items() :
			if (len(cumprobs) == 0) :
				newp = v2
			else :
				newp = cumprobs[len(cumprobs)-1] + v2
			cumprobs = cumprobs + [newp]
	counts = sampledist(cumprobs, int(nl * 0.25))
	print 'len(cumprobs) =',len(cumprobs),'len(counts) =',len(counts)
	cntr = 0
	cnty = {0:0, 1:0}
	testset = [] #list of lists representation of examples with schema Y,XS,FK,XR
	#Introduce FK by randomly picking from invmap for each's XR value; JC and CA done too
	for k,v in tpdprime.items() :
		for k2,v2 in v.items() :
			thecnt = counts[cntr]
			if(thecnt > 0) :
				for i in range(thecnt) :
					sval = k[0:ds]
					rval = k[ds:(ds + dr)]
					#pick FK randomly from invmap for each's XR value; JC and CA done too
					randind = random.randint(0,(len(invrdata[rval]) - 1))
					randfk = invrdata[rval][randind]
					dval = list([k2]) + list(sval) + list([randfk]) + list(rval) #schema Y,XS,FK,XR
					testset = testset + [dval]
					cnty[k2] = cnty[k2] + 1
			cntr = cntr + 1
	if VERBOSE :
		print testset
	
	#TODO: what if nl is such that not all FK from RIDs appears in massivedata?! :-/

	# All average and sumsq metrics are hashed by approach name
	tmpdict = {'JC': 0.0, 'CA': 0.0, 'JP':0.0, 'IG': 0.0}
	avgtrainloss = dict(tmpdict) # avg of (loss of each train set)
	avgtestloss = dict(tmpdict) # avg of (loss of each train set on test set)
	sumsqtrainloss = dict(tmpdict) 	# sum of squares of (loss of each train set)
	stdevtrainloss = dict(tmpdict) 	# stdev of (loss of each train set)
	sumsqtestloss = dict(tmpdict) # sum of squares of (loss of each train set on test set)
	stdevtestloss = dict(tmpdict) # stdev of (loss of each train set on test set)
	avgbias = dict(tmpdict) #B(x) # avg bias on test set using main predictions based on D  
	avgvariance = dict(tmpdict) #V(x)
	avgvaronbias = dict(tmpdict) #V(x) for B(x) = 1
	avgvaronunbias = dict(tmpdict) #V(x) for B(x) = 0
	avgnetvariance = dict(tmpdict) #[1-2B(x)]V(x)
	# To get B, V, and NV, we maintain a new array of map predictions from D on each ex
	# Each hash value is a list of lists; the list is |D| long; each element list is |testset| long
	allpreds = {'JC':[], 'CA':[], 'JP':[], 'IG':[]}
	# Implement Naive Bayes learner and tester; split test error into B, V, NV
	# naivebayes.py provides smoothed trainer and inference/MAP/loss on an example
	# Train NB on each set in D; score each on test set; obtain average loss, B, V, NV
	# Since the NB impl is feature vector-agnostic, we use the same functions
	dims = 1 + ds + dr
	doms = [[0,1]]
	for i in range(ds) :
		doms = doms + [[0,1]]
	doms = doms + [[i for i in range(1,(1 + nr))]]
	for i in range(dr) :
		doms = doms + [[0,1]]
	# We do for each of 4 approaches: JC, CA, JP, and IG
	inds = {'JC':[i for i in range(1 + ds + dr)], 'CA':[i for i in range(1 + ds)], 'JP':[i for i in range(ds)] + [i for i in range(1 + ds,1 + ds + dr)], 'IG':[i for i in range(ds)]}
	apps = ['JC', 'CA', 'JP', 'IG']
	#print 'apps:',apps,'inds:',inds,'doms:',doms
	for ti in range(numD) :
		#generate trainset
		counts = sampledist(cumprobs, nl)
		cntr = 0
		trainseti = [] #list of lists representation of examples with schema Y,XS,FK,XR
		#Introduce FK by randomly picking from invmap for each's XR value; JC and CA done too
		for k,v in tpdprime.items() :
			for k2,v2 in v.items() :
				thecnt = counts[cntr]
				if(thecnt > 0) :
					for i in range(thecnt) :
						sval = k[0:ds]
						rval = k[ds:(ds + dr)]
						#pick FK randomly from invmap for each's XR value; JC and CA done too
						randind = random.randint(0,(len(invrdata[rval]) - 1))
						randfk = invrdata[rval][randind]
						dval = list([k2]) + list(sval) + list([randfk]) + list(rval) #schema Y,XS,FK,XR
						trainseti = trainseti + [dval]
						cnty[k2] = cnty[k2] + 1
				cntr = cntr + 1
		if VERBOSE :
			print 'trainseti ti', ti, 'is', trainseti
		for app in apps :
			#print 'Running approach',app
			modi = naivebayes.trainNB(dims, doms, trainseti, inds[app])
			trainlossi = 0
			for exacti in trainseti :
				trainlossi += naivebayes.lossNBex(dims, doms, modi, exacti, inds[app])				
			thisavgtrainloss = float(trainlossi)/nl
			avgtrainloss[app] += thisavgtrainloss
			sumsqtrainloss[app] += thisavgtrainloss*thisavgtrainloss
			testlossi = 0
			thispreds = []
			for exi in testset :
				mapi = naivebayes.mapNBex(dims, doms, modi, exi, inds[app])
				thispreds = thispreds + [mapi[0]]
				if (mapi[0] != exi[0]) :
					testlossi = testlossi + 1
			thisavgtestloss = float(testlossi)/len(testset)
			avgtestloss[app] += thisavgtestloss 
			sumsqtestloss[app] += thisavgtestloss*thisavgtestloss
			allpreds[app] = allpreds[app] + [thispreds]
	
	print 'Approaches run in order:', apps,' with schema of output (1 line per approach):'
	print 'avgtrlossonD stdtrlossonD avgtestlossonD stdtestlossonD avgbias avgvar avgvonunb avgvonb avgnetvar'	
	for app in apps :
		(avgtestloss[app], avgtrainloss[app]) = (float(avgtestloss[app])/numD, float(avgtrainloss[app])/numD)
		vartest = float(sumsqtestloss[app])/numD - avgtestloss[app]*avgtestloss[app]
		if (vartest < 0) :
			vartest  = 0
		stdevtestloss[app] = math.sqrt(vartest)
		vartrain = float(sumsqtrainloss[app])/numD - avgtrainloss[app]*avgtrainloss[app]
		if (vartrain < 0) :
			vartrain  = 0
		stdevtrainloss[app] = math.sqrt(vartrain)
		#print '\tavg train loss for app',app,'across D =',avgtrainloss[app],'stdev =',stdevtrainloss[app],'and test loss across D =',avgtestloss[app],'stdev =',stdevtestloss[app]
		# Obtain the main predictions for each test example by looking across |D| MAPs (given an each approach)
		ymains = []
		for testi in range(len(testset)) :
			thisbias = 0
			thisvar = 0
			countyi = {} #counts for each value of y being the MAP
			for y in doms[0] :
				countyi[y] = 0
			for Di in range(numD) :
				thisymap = allpreds[app][Di][testi]
				countyi[thisymap] = countyi[thisymap] + 1
			ymi = doms[0][0] #get ym by init to some random value, and then get max
			for k,v in countyi.items() :
				if (v > countyi[ymi]) :
					ymi = k
			ymains = ymains + [ymi]
			if (ymi != testset[testi][0]) : #discrete bias 
				thisbias = 1
			for Di in range(numD) :
				if(allpreds[app][Di][testi] != ymains[testi]) :
					thisvar += 1
			thisvar = float(thisvar)/numD
			thisnetvar = (1 - 2 * thisbias) * thisvar
			avgbias[app] += thisbias
			avgvariance[app] += thisvar	#variance is avg loss of D samples over ymain
			avgnetvariance[app] += thisnetvar
			if (thisbias == 1) :
				avgvaronbias[app] += thisvar
			else :
				avgvaronunbias[app] += thisvar
	
		avgbias[app] = float(avgbias[app])/len(testset)
		avgvariance[app] = avgvariance[app]/len(testset)
		avgnetvariance[app] = avgnetvariance[app]/len(testset)
		avgvaronbias[app] = avgvaronbias[app]/len(testset)
		avgvaronunbias[app] = avgvaronunbias[app]/len(testset)
		print avgtrainloss[app], stdevtrainloss[app], avgtestloss[app], stdevtestloss[app], avgbias[app], avgvariance[app], avgvaronunbias[app], avgvaronbias[app], avgnetvariance[app]
		#print 'testset:',testset
		#print 'allpreds:',allpreds

main()
