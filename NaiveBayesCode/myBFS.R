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

#attributes is a vector of feature names to be used as starting node in feature lattice
#in both traindata and testdata, col 1 is example id, and col 2 is target
#this version also include rmse for error metric; it is either '01' or 'RMSE'
mybfs <- function(attributes, targetcol, traindata, testdata, errormetric, classes=5) {
	featvec = attributes;
	numfeats = length(featvec);
	if(numfeats == 0)
		stop("attributes not specified!");
	
	#train once with full featvec
	#targetcol = 4; #was 2 for walmart; so it caused a bug on yelp; i have made it an argument!
	fullnb = myNBlog(traindata[,featvec], traindata[,targetcol]);
	
	#get accuracy on test with full featvec
	fulltab = table(predict.myNBlog(fullnb, testdata[,featvec]), testdata[,targetcol], dnn=list('predicted','actual'));
	fullacc = geterr(fulltab, errormetric, nrow(testdata), classes);
	print("full feature set accuracy:")
	print(fullacc)
	
	#start BFS; create a vector for accuracy info after dropping each feature in attributes 
	bestacc = fullacc;
	bestvec = featvec;
	repeat {
		numfeats = length(bestvec);
		if(numfeats <= 1) {
			print("only one feature left; no more children; BFS stopped")
			break();
		}
		#obtain all children sets
		newfeatvecs = matrix(bestvec[-1], 1, length(bestvec[-1]));
		for (i in 2:numfeats) {
			newfeatvecs = rbind(newfeatvecs, bestvec[-i])
		}
		print("BFS found number of children:")
		print(nrow(newfeatvecs))
		#evaluate each new child set and obtain accuracies
		acclist = rep(0, nrow(newfeatvecs));
		for (f in 1:nrow(newfeatvecs)) {
			thisnb = myNBlog(traindata[,newfeatvecs[f,]], traindata[,targetcol]);
			newtab = table(predict.myNBlog(thisnb, testdata[,newfeatvecs[f,]]), testdata[,targetcol], dnn=list('predicted','actual'));
			newacc = geterr(newtab, errormetric, nrow(testdata), classes);
			acclist[f] = newacc; #R vector indices start from 1
			print("computed accuracy of child set:")
			print(newacc)
			print(newfeatvecs[f,])
		}
		print("completed all children at level:")
		print(length(attributes) - length(bestvec) + 1)
		print("accuracy of all children:")
		print(acclist)
		maxchildacc = max(acclist);
		if(bestacc < maxchildacc) {
			#found a more accurate child set; switch to it and continue bfs
			bestacc = maxchildacc;
			bestvec = newfeatvecs[which.max(acclist),];
			print("found a more accurate child:")
			print(bestacc)
			print(bestvec)
		}
		else {
			print("no child is more accurate; BFS stopped")
			break();
		}
	}
	print("best accuracy and feature set overall:")
	print(bestacc)
	print(bestvec)	
	return(bestvec)
}

#this version uses predictfs.myNBlog for scoring if crr > 1.5 for an attribute table
#since fl provides speedups only when tuple ratio > 300, it is never used on the real data
#this version also manages the tricky task of ensuring feature-table mappings
#it is assumed that all feature names are unique across all base table schemas
#training is always over denormalized data; only scoring might be factorized
#the listRs are the "pruned" subset of R's based on the FK values present in testS
#denormtest is a pre-done full join of all base tables for testing, with the same schema as traindata
#namesS does not include Y though
#mybfswithfs <- function(attributes, targetcol, traindata, testdata, errormetric) {
mybfswithfs <- function(traindata, targetcolname, namesS, listFKs, listRs, isFKforbidden, denormtest, errormetric, classes=5) {
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

	#train once with full featvec
	thisfvec = featvec
	thisnb = myNBlog(traindata[,thisfvec], traindata[,targetcolname]);
	
	#get accuracy on test with current X (now it is full featvec) - this might have to use fs depending on ratios
	#fulltab = table(predict.myNBlog(fullnb, testdata[,featvec]), testdata[,targetcol], dnn=list('predicted','actual'));
	thistab = dsorfs(thisfvec, thisnb, denormtest, targetcolname, namesS, listFKs, listRs, featRis, isFKforbidden);

	fullacc = geterr(thistab, errormetric, nrow(denormtest), classes);
	print("full feature set accuracy:")
	print(fullacc)
	
	#start BFS; create a vector for accuracy info after dropping each feature in attributes 
	bestacc = fullacc;
	bestvec = featvec;
	repeat {
		numfeats = length(bestvec);
		if(numfeats <= 1) {
			print("only one feature left; no more children; BFS stopped")
			break();
		}
		#obtain all children sets
		newfeatvecs = matrix(bestvec[-1], 1, length(bestvec[-1]));
		for (i in 2:numfeats) {
			newfeatvecs = rbind(newfeatvecs, bestvec[-i])
		}
		print("BFS found number of children:")
		print(nrow(newfeatvecs))
		#evaluate each new child set and obtain accuracies
		acclist = rep(0, nrow(newfeatvecs));
		for (f in 1:nrow(newfeatvecs)) {
			thisnb = myNBlog(traindata[,newfeatvecs[f,]], traindata[,targetcolname]);
			#newtab = table(predict.myNBlog(thisnb, testdata[,newfeatvecs[f,]]), testdata[,targetcol], dnn=list('predicted','actual'));
			newtab = dsorfs(newfeatvecs[f,], thisnb, denormtest, targetcolname, namesS, listFKs, listRs, featRis, isFKforbidden);

			newacc = geterr(newtab, errormetric, nrow(denormtest), classes);
			acclist[f] = newacc; #R vector indices start from 1
			print("computed accuracy of child set:")
			print(newacc)
			print(newfeatvecs[f,])
		}
		print("completed all children at level:")
		print(length(featvec) - length(bestvec) + 1)
		print("accuracy of all children:")
		print(acclist)
		maxchildacc = max(acclist);
		if(bestacc < maxchildacc) {
			#found a more accurate child set; switch to it and continue bfs
			bestacc = maxchildacc;
			bestvec = newfeatvecs[which.max(acclist),];
			print("found a more accurate child:")
			print(bestacc)
			print(bestvec)
		}
		else {
			print("no child is more accurate; BFS stopped")
			break();
		}
	}
	print("best accuracy and feature set overall:")
	print(bestacc)
	print(bestvec)	
	return(bestvec)
}
