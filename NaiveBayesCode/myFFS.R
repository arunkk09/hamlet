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

#attributes is a vector of feature names that is the top of the lattice explored in a forward way
#in both traindata and testdata, col 1 is example id, and col 4 is target in Yelp (it was 2 for Walmart!)
#this version also include rmse for error metric; it is either '01' or 'RMSE'
myffs <- function(attributes, targetcol, traindata, testdata, errormetric, classes=5) {
	featvec = attributes;
	numfeats = length(featvec);
	if(numfeats == 0)
		stop("attributes not specified!");
	
	#start FFS; create a vector for accuracy info after adding each of remaining features
	bestacc = -1000;		
	bestvec = c(); 		#FFS starts with null feat vec
	remfeats = featvec 	#remaining features to be added at next level
	counter = length(featvec);
	repeat {
		if(counter == 0) {
			print("all features added; no more parents; FFS stopped")
			break();
		}
		print("remaining features to add")
		print(remfeats)
		#obtain all parent sets
		newfeatvecs = matrix(append(bestvec, remfeats[1]),1);
		if(counter >= 2) {
			for (i in 2:counter) {
				newfeatvecs = rbind(newfeatvecs, append(bestvec, remfeats[i]));
			}
		}
		print("FFS found number of parents:")
		print(nrow(newfeatvecs))
		print("newfeatvecs")
		print(newfeatvecs)
		#evaluate each new parent set and obtain accuracies
		acclist = rep(0, nrow(newfeatvecs));
		for (f in 1:nrow(newfeatvecs)) {
			thisnb = myNBlog(traindata[,newfeatvecs[f,]], traindata[,targetcol]);
			newtab = table(predict.myNBlog(thisnb, testdata[,newfeatvecs[f,]]), testdata[,targetcol], dnn=list('predicted','actual'));
			newacc = geterr(newtab, errormetric, nrow(testdata), classes);
			acclist[f] = newacc; #R vector indices start from 1
			print("computed accuracy of parent set:")
			print(newacc)
			print(newfeatvecs[f,])
		}
		print("FFS at level:")
		print(length(bestvec) + 1)
		print("accuracy of parents:")
		print(acclist)
		maxparentacc = max(acclist);
		if(bestacc < maxparentacc) {
			#found a more accurate parent set; switch to it and continue ffs
			bestind = which.max(acclist);
			bestacc = maxparentacc;
			bestvec = newfeatvecs[bestind,];
			remfeats = remfeats[-bestind];
			counter = counter - 1;
			print("found a more accurate parent:")
			print(bestacc)
			print(bestvec)
		}
		else {
			print("no parent is more accurate; FFS stopped")
			break();
		}
	}
	print("best accuracy and feature set overall:")
	print(bestacc)
	print(bestvec)	
	return(bestvec)
}

#this version invokes dsorfs to automatically pick between DS and FS for each feature set explored
#myffswithfs <- function(attributes, targetcol, traindata, testdata, errormetric) {
myffswithfs <- function(traindata, targetcolname, namesS, listFKs, listRs, isFKforbidden, denormtest, errormetric, classes=5) {
	featvec = setdiff(names(traindata), targetcolname) #traindata is assumed to have only X features and Y (forbidden FKs are pre-dropped)
	numfeats = length(featvec);
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

	#start FFS; create a vector for accuracy info after adding each of remaining features
	bestacc = -1000;		
	bestvec = c(); 		#FFS starts with null feat vec
	remfeats = featvec 	#remaining features to be added at next level
	counter = length(featvec);
	repeat {
		if(counter == 0) {
			print("all features added; no more parents; FFS stopped")
			break();
		}
		print("remaining features to add")
		print(remfeats)
		#obtain all parent sets
		newfeatvecs = matrix(append(bestvec, remfeats[1]),1);
		if(counter >= 2) {
			for (i in 2:counter) {
				newfeatvecs = rbind(newfeatvecs, append(bestvec, remfeats[i]));
			}
		}
		print("FFS found number of parents:")
		print(nrow(newfeatvecs))
		print("newfeatvecs")
		print(newfeatvecs)
		#evaluate each new parent set and obtain accuracies
		acclist = rep(0, nrow(newfeatvecs));
		for (f in 1:nrow(newfeatvecs)) {
			thisnb = myNBlog(traindata[,newfeatvecs[f,]], traindata[,targetcolname]);
			#newtab = table(predict.myNBlog(thisnb, testdata[,newfeatvecs[f,]]), testdata[,targetcol], dnn=list('predicted','actual'));
			newtab = dsorfs(newfeatvecs[f,], thisnb, denormtest, targetcolname, namesS, listFKs, listRs, featRis, isFKforbidden);
			
			newacc = geterr(newtab, errormetric, nrow(denormtest), classes);
			acclist[f] = newacc; #R vector indices start from 1
			print("computed accuracy of parent set:")
			print(newacc)
			print(newfeatvecs[f,])
		}
		print("FFS at level:")
		print(length(bestvec) + 1)
		print("accuracy of parents:")
		print(acclist)
		maxparentacc = max(acclist);
		if(bestacc < maxparentacc) {
			#found a more accurate parent set; switch to it and continue ffs
			bestind = which.max(acclist);
			bestacc = maxparentacc;
			bestvec = newfeatvecs[bestind,];
			remfeats = remfeats[-bestind];
			counter = counter - 1;
			print("found a more accurate parent:")
			print(bestacc)
			print(bestvec)
		}
		else {
			print("no parent is more accurate; FFS stopped")
			break();
		}
	}
	print("best accuracy and feature set overall:")
	print(bestacc)
	print(bestvec)	
	return(bestvec)
}
