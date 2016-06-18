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

source("myNB.R")

geterr <- function(fulltab, errormetric, nexamples, classes=5) {
        fullacc = -1000;
        if(errormetric == 'RMSE') {
			w = fulltab + t(fulltab);
			fsum = 0;
			for(c in 1:(classes - 1)) {
				er = c*c;
				for(l in 1:(classes - c)) {
					fsum = fsum + w[l, (l + c)] * er
				}
			}
			fullacc = sqrt(fsum/nexamples)
            fullacc = -fullacc; #rmse sign is inverted to ensure the max is selected
        }
        else if(errormetric == '01') {
            fullacc = sum(diag(fulltab))/nexamples;
        }
        else {
            print ("Unrecognized error metric:")
			print(errormetric)
        }
        return (fullacc);
}

#attributes is a vector of feature names in increasing order of relevance to be used for creating nested subsets of features for evaluation
myfilter <- function(attributes, targetcolname, traindata, testdata, errormetric, classes=5) {
	numfeats = length(attributes);
	if(numfeats == 0)
		stop("attributes not specified!");
	
	#get all nested subsets of topK - drop features from the head
	newfeatvecs = list(attributes)
	for (i in 1:(numfeats - 1)) {
		newfeatvecs = append(newfeatvecs, list(attributes[-(1:i)]))
	}
	print("number of nested subsets to evaluate:")
	print(length(newfeatvecs))
	#evaluate each set and obtain accuracies
	acclist = rep(0, length(newfeatvecs));
	for (f in 1:length(newfeatvecs)) {
		tpt = proc.time()
		thisnb = myNBlog(traindata[,newfeatvecs[[f]]], traindata[,targetcolname]);
		newtab = table(predict.myNBlog(thisnb, testdata[,newfeatvecs[[f]]]), testdata[,targetcolname], dnn=list('predicted','actual'));
		newacc = geterr(newtab, errormetric, nrow(testdata), classes);
		acclist[f] = newacc; #R vector indices start from 1
		print("runtime taken for this set")
		print(proc.time() - tpt)
		print("computed accuracy of nested subset:")
		print(newacc)
		print(newfeatvecs[[f]])
	}
	bestacc = max(acclist);
	bestind = which.max(acclist);
	bestvec = newfeatvecs[[bestind]];
	print("best accuracy and feature set overall:")
	print(bestacc)
	print(bestvec)
	return(bestvec)
}

#attributes is a vector of feature names in increasing order of relevance to be used for creating nested subsets of features for evaluation
#this version uses predictfs.myNBlog for scoring if crr > 1.5 for an attribute table
#since fl provides speedups only when tuple ratio > 300, it is never used on the real data
#this version also manages the tricky task of ensuring feature-table mappings
#it is assumed that all feature names are unique across all base table schemas
#training is always over denormalized data; only scoring might be factorized
#the listRs are the "pruned" subset of R's based on the FK values present in testS
#denormtest is a pre-done full join of all base tables for testing, with the same schema as traindata
#namesS does not include Y though
#myfilter <- function(attributes, targetcol, traindata, testdata, errormetric, classes=5) {
myfilterwithfs <- function(attributes, traindata, targetcolname, namesS, listFKs, listRs, isFKforbidden, denormtest, errormetric, classes=5) {
	numfeats = length(attributes);
	if(numfeats == 0)
		stop("attributes not specified!");
	
	#get a list of lists with the names of features in each Ri (excluding PK/FK)
	lfks = length(listFKs)
	featRis = list(setdiff(names(as.data.frame(listRs[1])), listFKs[1]))
	if(lfks > 1) {
		for (i in 2:lfks) {
			featRis <- append(featRis, list(setdiff(names(as.data.frame(listRs[i])), listFKs[i])))
		}
	}
	
	#get all nested subsets of topK - drop features from the head
	newfeatvecs = list(attributes)
	for (i in 1:(numfeats - 1)) {
		newfeatvecs = append(newfeatvecs, list(attributes[-(1:i)]))
	}
	print("number of nested subsets to evaluate:")
	print(length(newfeatvecs))
	#evaluate each set and obtain accuracies
	acclist = rep(0, length(newfeatvecs));
	for (f in 1:length(newfeatvecs)) {
		thisnb = myNBlog(traindata[,newfeatvecs[[f]]], traindata[,targetcolname]);
		#newtab = table(predict.myNBlog(thisnb, testdata[,newfeatvecs[f,]]), testdata[,targetcol], dnn=list('predicted','actual'));
		newtab = dsorfs(newfeatvecs[[f]], thisnb, denormtest, targetcolname, namesS, listFKs, listRs, featRis, isFKforbidden);

		newacc = geterr(newtab, errormetric, nrow(denormtest), classes);
		acclist[f] = newacc; #R vector indices start from 1
		print("computed accuracy of nested subset:")
		print(newacc)
		print(newfeatvecs[[f]])
	}
	bestacc = max(acclist);
	bestind = which.max(acclist);
	bestvec = newfeatvecs[[bestind]];
	print("best accuracy and feature set overall:")
	print(bestacc)
	print(bestvec)
	return(bestvec)
}
