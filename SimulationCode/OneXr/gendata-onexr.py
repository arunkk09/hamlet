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

def main() :
	# parameters from arguments
	if(len(sys.argv) < 9) :
		print >> sys.stderr, 'usage: ds dr fur nr rgvar nl seed skewp'
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
	skewp = float(sys.argv[8])
	d = ds + dr #Y excluded
	numD = 100 #default
	print 'ds', ds, 'dr', dr, 'fur', fur, 'nr', nr, 'nl', nl, 'seed', seed, 'skewp', skewp, 'numD', numD 
	random.seed(seed)

	#Here, Y is influenced only by the first feature Xr! We vary a "probability skew" (p)
	#We use p = P[Y=0|Xr=0] = P[Y=1|Xr=1]; p=1.0 means Xr is perfectly predictive of Y!
	#All other features are random noise (XS, and other Xr)
	#The sample generation steps are as follows:
	#1. Sample nr examples with all values of XR features chosen uniformly randomly
	#2. Sample nl examples with all values of XS and FK features chosen uniformly randomly
	#3. For each of nl examples, choose Y based on first Xr value and p vs (1-p) prob
	#Data for everything is basically done!
	
	#1. Sample nr examples with all values of XR features chosen uniformly randomly
	rtuples = {}
	for rid in range(1, 1 + nr) :
		rtuples[rid] = [random.randint(0,1) for i in range(dr)]
	if VERBOSE :
		print 'rtuples:',rtuples

	# Instead of bootstrapping, as Jerry suggested, we simply sample entirely new datasets!
	# Thus, no need to maintain and track indices; create test dataset and set aside
	# In a for loop, create training datasets and use on the fly
	# The sample size is now the training dataset size
	# The test dataset size is set to be 25% of the training dataset size  
	
	#2. Sample nl examples with all values of XS and FK features chosen uniformly randomly
	#3. For each of nl examples, choose Y based on first Xr value and p vs (1-p) prob
	testset = [] #list of lists representation of examples with schema Y,XS,FK,XR
	cnty0 = 0
	for dataid in range(int(nl * 0.25)) :
		randfk = random.randint(1,nr)
		thisxr = rtuples[randfk][0]
		tmprv = random.random()
		thisy = 1
		if(((tmprv < skewp) and (thisxr == 0)) or ((tmprv < (1 - skewp)) and (thisxr == 1))):
			thisy = 0
			cnty0 += 1
		dval = [thisy] + [random.randint(0,1) for i in range(ds)] + [randfk] + rtuples[randfk] #schema Y,XS,FK,XR
		testset = testset + [dval]
	print 'len(testset)',len(testset),'cnty0',cnty0
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
		trainseti = [] #list of lists representation of examples with schema Y,XS,FK,XR
		cnty0 = 0
		for dataid in range(nl) :
			randfk = random.randint(1,nr)
			thisxr = rtuples[randfk][0]
			tmprv = random.random()
			thisy = 1
			if(((tmprv < skewp) and (thisxr == 0)) or ((tmprv < (1 - skewp)) and (thisxr == 1))):
				thisy = 0
				cnty0 += 1
			dval = [thisy] + [random.randint(0,1) for i in range(ds)] + [randfk] + rtuples[randfk] #schema Y,XS,FK,XR
			trainseti = trainseti + [dval]
		if VERBOSE :
			print 'len(trainseti)',len(trainseti),'cnty0',cnty0,'trainseti:',trainseti
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
