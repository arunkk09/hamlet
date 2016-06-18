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

#x is assumed to be a clean data frame with all factor variables
#this version precomputes all log probabilities!
myNBlog <- function(x, y, laplace = 1, threshold = 0.001, eps = 0) {
	call <- match.call()
	logest <- function(var) {
		tab <- table(y, var)
		prob <- (tab + laplace) / (rowSums(tab) + laplace * nlevels(var))
		prob[prob <= eps] <- threshold
		return (log(prob))
	}
	logypt = log(table(y))
	logcpts = lapply(as.data.frame(x), logest)
	return (structure(list(logypt = logypt, logcpts = logcpts, levels = levels(y), call = call), class = "myNB"))
}


#newdata is assumed to have the exact same schema as traindata for myNB object
#this differs from predict.myNB in that it does not invoke log() at all!
predict.myNBlog <- function(object, newdata, threshold = 0.001, eps = 0, ...) {
	#pt = proc.time()
	attribs <- names(newdata)
	newdata <- data.matrix(newdata);
	L <- matrix(rep(object$logypt, nrow(newdata)),nrow=length(object$levels), byrow=FALSE);
	#print (proc.time() - pt)
	for(v in 1:ncol(newdata)) {
		for(co in 1:nrow(newdata)) {
			nd <- newdata[co,v];
			#print("v"); print(v); print("var"); print(attribs[v]); print("co"); print(co); print("nd"); print(nd)
			#prob <- object$cpts[[v]][,nd];
			#prob[prob <= eps] <- threshold;
			#print("log prob"); print(log(prob))
			#L[,co] <- L[,co] + log(prob)
			L[,co] <- L[,co] + object$logcpts[[v]][,nd];
		}
	}
	#pt = proc.time()
	FL <- factor(object$levels[apply(L, 2, which.max)], levels=object$levels)
	#print (proc.time() - pt)
	return (FL)
}


#assumed that the factor levels match in the training set and the base tables
#factorized scoring with S, a list of FK attrib names in S, a list of Ri data frames in same order with same PK names as FKs
#the attrib names are assumed to be exactly the same as in traindata for myNB object
#there is no cost model used here - so it factorizes fully across all given Ris
#this differs from predictfs.myNB in that it does not invoke log() at all!
#isFKforbidden is a list of True/False values that indicate whether FKi is forbidden or not
predictfs.myNBlog <- function(object, newdata, listFKs, listRs, isFKforbidden, threshold = 0.001, eps = 0, ...) {
	#for each Ri, we precompute the logsum probs component from features in Ri and store it in a 2-col data frame keyed by FK value
	#so we get sum(log(prob for all XRij and FKi)) and store it first as presumi (i=1:k for Ri tables); if FKi is forbidden, its logprob is not sought 
	#the overall flow is as follows: logsum = logy + sum(logprob for all XSj only) + presum1 for FKi + ... + presumk for FK k
	#note that since logcpts already have the log probs, there is no need to invoke the log function here!
	#pt = proc.time();
	lfks <- length(listFKs);
	lfkinds <- 1:length(listFKs);
	ndomy <- length(object$levels)
	presumi <- function(i) {
		pkRi <- listFKs[i];
		Ridf <- as.data.frame(listRs[i]);
		attribsRi <- names(Ridf);
		newRi <- data.matrix(Ridf);
		pkRiind <- match(pkRi, attribsRi) #assuming all feature names are unique
		nonpkinds <- setdiff(1:ncol(newRi), pkRiind)
		getlevels <- function(v) {
			return(levels(Ridf[[v]]))
		}
		alllevels <- sapply(1:ncol(newRi), getlevels); #we can still get the levels; just do not use it later if FK is forbidden
		cptindxri <- rep(0,length(attribsRi)); #integer indices into object cpts for attribsRi features
		for(tt in 1:length(cptindxri)) {
			cptindxri[tt] <- match(c(attribsRi[tt]),names(object$logcpts))
		}
		#preRi <- matrix(rep(0, ndomy * nrow(Ridf)),nrow=length(object$levels), byrow=TRUE); #this caused a bug because nrow(R) < |Dom(FK)| due to reducing R minor optmzn
		preRi <- matrix(rep(0, ndomy * nlevels(Ridf[,pkRiind])),nrow=length(object$levels), byrow=TRUE);
		for (co in 1:nrow(newRi)) {
			preprobv <- function(v) {
				rd <- newRi[co,v]; #if FK was forbidden, it will never be looked at
				return(object$logcpts[[cptindxri[v]]][,alllevels[[v]][rd]])
			}
			#preRi[,co] <- apply(log(sapply(seq_along(attribsRi), preprobv)), 1, sum)
			#a major bug arose due to an implicit assumption that order of fks and order of levels as factors are the same! but they are not! it has been fixed now using pkRi as below:
			#preRi[,newRi[co,pkRiind]] <- apply(log(sapply(seq_along(attribsRi), preprobv)), 1, sum)
			if(isFKforbidden[i] == FALSE) {
				preRi[,newRi[co,pkRiind]] <- apply(sapply(seq_along(attribsRi), preprobv), 1, sum) #this caused a bug when fk was forbidden because nonpkinds should be used!
			}
			else {
				preRi[,newRi[co,pkRiind]] <- apply(sapply(nonpkinds, preprobv), 1, sum) #FKi forbidden; do not look for its logprob - nonpkinds ensures that
			}
		}
		return (preRi)
	}
	#presumi1 = presumi(1)
	#presums <- sapply(1:length(listFKs), presumi)
	presums <- list(presumi(1))
	if(lfks > 1) {
		for (i in 2:lfks) {
			presums <- append(presums,list(presumi(i)))
		}
	}
	#print(proc.time() - pt)
	#print(presums)
	#pt = proc.time()
	#need to use only XSj since logprob for FKi is present in presumi
	attribsS <- setdiff(names(newdata), listFKs);
cptindx <- rep(0,length(attribsS)); #integer indices into object cpts for attribS features
	for(i in 1:length(cptindx)) {
		cptindx[i] <- match(c(attribsS[i]),names(object$logcpts))
	}
	ndindx <- rep(0,length(attribsS)); #integer indices into newdata columns for attribS features
	for(i in 1:length(ndindx)) {
		ndindx[i] <- match(c(attribsS[i]),names(newdata))
	}
	newdata <- data.matrix(newdata);
    L <- matrix(rep(object$logypt, nrow(newdata)),nrow=length(object$levels), byrow=FALSE);
	#print(proc.time() - pt)
	#pt = proc.time()
if(length(attribsS) > 0) {
	for(v in 1:length(attribsS)) {
		for(co in 1:nrow(newdata)) {
			nd <- newdata[co,ndindx[v]];
			#prob <- object$cpts[[cptindx[v]]][,nd];
			#prob[prob <= eps] <- threshold;
			#print("v"); print(v); print("var"); print(attribsS[v]); 
			#print("co"); print(co);	print("log prob"); print(log(prob))
			#L[,co] <- L[,co] + log(prob)
			L[,co] <- L[,co] + object$logcpts[[cptindx[v]]][,nd]
		}
	}
}
	for(co in 1:nrow(newdata)) {
		fksum <- rep(0.0, length(object$levels))
		for (k in lfkinds) {
			fksum <- fksum + presums[[k]][,newdata[co,listFKs[k]]]
		}
		#print("newdata[co,listFKs[k]]"); print(newdata[co,listFKs[k]])
		#print("presums[[k]][..]"); print(presums[[k]][,newdata[co,listFKs[k]]])
		L[,co] <- L[,co] + fksum
	}
	#print(proc.time() - pt)
	#pt = proc.time()
	FL <- factor(object$levels[apply(L, 2, which.max)], levels=object$levels)
	#print(proc.time() - pt)
	return (FL)
}

#this invokes either denorm scoring or factorized scoring based on input featvec and normalized schema given
dsorfs <- function(thisfvec, thisnb, denormtest, targetcolname, namesS, listFKs, listRs, featRis, isFKforbidden) {
	alpha = 1.5 #CRR threshold
	effXS = setdiff(intersect(thisfvec, namesS), listFKs) #XS present in X (FKs excluded)
	leneffXS = length(effXS)
	#get list of Ris that pass the threshold; they will use fs, rest will use ds
	efflistRs = list()
	efflistFKs = list()
	efflistforbs = list()
	denormSfeats = thisfvec #the full input feature set without Y
	for (i in 1:length(listFKs)) {
		effXRi = intersect(thisfvec, featRis[[i]]) #XRi present in X (PK/FK excluded)
		ratioRi = length(effXRi) * 1.0 / (1.0 + leneffXS)
		if(ratioRi >= alpha) { #fs for this
			print("fs will be used for Ri with PK+feats")
			print(union(listFKs[i], effXRi))
			efflistRs = append(efflistRs, list(as.data.frame(listRs[i])[,union(listFKs[i], effXRi)]))
			efflistFKs = append(efflistFKs, list(listFKs[i]))
			#note that the "forbidden" is overloaded here to exploit FK as physical connectors even if there are absent in thisfvec
			#so, FK will always be included in denormSfeats as long as some XRs it refers to are present in thisfvec and they need to be factorized 
			#if the FK is originally forbidden, it has to be included in denormSfeats but as forbidden again (it is guaranteed not to occur in thisfvec anyway)
			#but if the FK was NOT forbidden originally, it will be labeled forbidden now if it is NOT present in thisfvec; otherwise, it will remain not forbidden
			if(length(intersect(thisfvec, listFKs[i])) == 0) { #FK not present in thisfvec; forbid it
				efflistforbs = append(efflistforbs, TRUE)
			}
			else { #FK present in thisfvec; use original value for forbidden
				efflistforbs = append(efflistforbs, isFKforbidden[i])
			}
			denormSfeats = union(setdiff(denormSfeats, featRis[[i]]), listFKs[i]) #these XRis will be factorized; but add the FK to help factorize
		}
	}
	if(length(efflistRs) > 0) { #at least 1 R table exists to factorize over
		print("running predictfs with efflistFKS")
		print(efflistFKs)
		print("denormSfeats")
		print(denormSfeats)
		print("efflistforbs")
		print(efflistforbs)
		tmp = as.data.frame(denormtest[,denormSfeats])
		names(tmp) = denormSfeats
		thistab = table(predictfs.myNBlog(thisnb, tmp, as.character(efflistFKs), efflistRs, efflistforbs), denormtest[,targetcolname], dnn=list('predicted','actual'));
	}
	else { #nothing to be factorized; just do denorm
		print("running denorm scoring")
		tmp = as.data.frame(denormtest[,thisfvec])
		names(tmp) = thisfvec
		thistab = table(predict.myNBlog(thisnb, tmp), denormtest[,targetcolname], dnn=list('predicted','actual'));
	}
	return(thistab)
}
