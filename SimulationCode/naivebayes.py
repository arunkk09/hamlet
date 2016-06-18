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

VERBOSE = False

#Implementation is agnostic to S and R; instead, a list of domain values is given
#Schema of doms is a list of lists containing domains of any of Y,XS,FK,XR
#Inds is a list that mentions the indices of the feature vector in X=[XS FK XR] to be used
#JC will have all indices (0 to ds+dr); CA (0 to ds); JP (0 to ds-1 ds+1 ds+dr); IG (0 to ds-1)
def trainNB(d, doms, tdata, inds) :
	#Schema for NB model: a list of dicts with P(Y) as first entry of list
	#dict of P(Y) contains Y hashed to prob
	#ith entry of list is a dict corresp to ith feature in order (any of): XS, FK, XR
	#dict of P(Xi|Y) contains Y hashed to a dict that contains Xi hashed to prob
	ret = []
	initdictY = {}
	for y in doms[0] :
		initdictY[y] = 0
	ret = [initdictY.copy()]
	#for i in range(d) : #d includes XS,FK,XR or any other feature vector
	for i in inds : #inds could include any of XS,FK,XR
		initdictYX = {}
		initdictX = {}
		for x in doms[(1 + i)] :
			initdictX[x] = 0
		for y in doms[0] :
			initdictYX[y] = dict(initdictX)
		ret = ret + [dict(initdictYX)]
	#print 'initret=',ret
	#start counting frequencies of Y and (Y,Xi) using tdata
	for t in tdata :
		#print 't =',t
		#Schema of labeled example: Y,XS,FK,XR as a list
		#print 'ret[',0,'][',t[0],'] =',ret[0][t[0]]
		ret[0][t[0]] = ret[0][t[0]] + 1 #update count of Y
		#print 'ret[',0,'][',t[0],'] becomes',ret[0][t[0]]
		#for i in range(d) : #update counts of (Y,Xi) for all features
		cntr = 1
		for i in inds : #update counts of (Y,Xi) for all features
			#print 'i =',i,'ret[',(1 + i),'][',t[0],'][',t[(1 + i)],'] =',ret[(1 + i)][t[0]][t[(1 + i)]]
			ret[cntr][t[0]][t[(1 + i)]] = ret[cntr][t[0]][t[(1 + i)]] + 1
			cntr = cntr + 1 #ret contains only chosen features, but t and doms have all of JC
			#print 'i =',i,'ret[',(1 + i),'][',t[0],'][',t[(1 + i)],'] becomes',ret[(1 + i)][t[0]][t[(1 + i)]]
	#print 'retbefpseu=',ret
	#add pseudocounts of 1 for each Y,feature value (Laplacian/additive smoothing)
	for y in doms[0] :
		#for i in range(d) :
		cntr = 1
		for i in inds :
			for xi in doms[(1 + i)] :
				ret[cntr][y][xi] = float(ret[cntr][y][xi] + 1)/(ret[0][y] + len(doms[(1 + i)]))
			cntr = cntr + 1
	#normalize priors
	for y in doms[0] :
		ret[0][y] = float(ret[0][y])/len(tdata)
	return ret

def inferNBex(d, doms, mod, ex, inds, label=True) :
	#Schema of mod is same as training output: P(Y) as hash Y->p, P(Xi|Y) as double hash Y->Xi->p
	#schema of ex is same as labeled training example: Y,XS,FK,XR if label is True, else no Y 
	ret = {}
	for y in doms[0] :
		ret[y] = 0 #marginal distr of Y given feature vector of ex
	for y in doms[0] :
		ret[y] = mod[0][y]
		#for i in range(d) :
		cntr = 1
		for i in inds :
			if (label == True) :
				ret[y] = ret[y] * mod[cntr][y][ex[(1 + i)]]
			else :
				ret[y] = ret[y] * mod[cntr][y][ex[i]]
			cntr = cntr + 1
	#normalize the return marginal probs of Y
	sumy = sum(ret.values())
	for k,v in ret.items() :
		ret[k] = v/sumy
	return ret

def mapNBex(d, doms, mod, ex, inds) :
	#ex has to be labeled; infer on it and obtain MAP estimate
	infer = inferNBex(d, doms, mod, ex, inds)
	maxkv = [0,infer[0]]
	for k,v in infer.items() :
		if(v > maxkv[1]) :
			maxkv = [k,v]
	return maxkv

def lossNBex(d, doms, mod, ex, inds) :
	#ex has to be labeled; get MAP on it and compute 0/1 loss
	mapnb = mapNBex(d, doms, mod, ex, inds)
	if (mapnb[0] == ex[0]) :
		return 0
	else :
		return 1

def main() :
	if(len(sys.argv) < 3) :
		print >> sys.stderr, 'usage: ds dr l0'
		sys.exit(0)
	ds = int(sys.argv[1])
	dr = int(sys.argv[2])
	l0 = int(sys.argv[3])
	#k = int(sys.argv[4]) #default 2
	#l = int(sys.argv[5]) #default 2
	ds = 2
	dr = 2
	l0 = 4
	trainset = [[0,0,1,1,0,0],[1,1,1,2,0,0],[0,0,1,3,0,1],[1,1,1,4,1,1]] #list of lists representation
	doms = [[0,1]]
	for i in range(ds) :
		doms = doms + [[0,1]]
	doms = doms + [[i for i in range(1,(1 + l0))]]
	for i in range(dr) :
		doms = doms + [[0,1]]
	d = 1 + ds + dr
	print 'doms:',doms
	apps = ['JC', 'CA', 'JP', 'IG']
	inds = [[i for i in range(1 + ds + dr)], [i for i in range(1 + ds)], [i for i in range(ds)] + [i for i in range(1 + ds,1 + ds + dr)], [i for i in range(ds)]]
	print 'inds(JC,CA,JP,IG):',inds
	for i in range(4) :
		print 'Running approach',apps[i]
		NBmodel = trainNB(d, doms, trainset, inds[i])
		print '\t',NBmodel

		testset = [[0,1,0,1,0,0],[1,1,1,2,0,0],[0,1,1,2,0,0],[0,0,1,3,0,1]]
		for ex in testset :
			infer = inferNBex(d, doms, NBmodel, ex, inds[i])
			mapnb = mapNBex(d, doms, NBmodel, ex, inds[i])
			lossnb = lossNBex(d, doms, NBmodel, ex, inds[i])
			print '\tOn ex',ex,'NB with',apps[i],'infers',infer,'MAP',mapnb,'loss',lossnb

#main()
