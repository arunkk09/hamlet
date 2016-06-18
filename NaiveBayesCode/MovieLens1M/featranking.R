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

source("myNB.R");
source("allentropyinfogain.R")
source("myfilter.R")
options(width=190)

MLfull=read.csv("MLall.csv");
MLtrain=read.csv("MLtrain.csv");
MLtest=read.csv("MLtest.csv");
MLhold=read.csv("MLhold.csv");

MLtrain$movieid = factor(MLtrain$movieid, levels=levels(MLfull$movieid));
MLtrain$userid = factor(MLtrain$userid, levels=levels(MLfull$userid));
MLtrain$zipcode = factor(MLtrain$zipcode, levels=levels(MLfull$zipcode));

MLtest$movieid = factor(MLtest$movieid, levels=levels(MLfull$movieid));
MLtest$userid = factor(MLtest$userid, levels=levels(MLfull$userid));
MLtest$zipcode = factor(MLtest$zipcode, levels=levels(MLfull$zipcode));

MLhold$movieid = factor(MLhold$movieid, levels=levels(MLfull$movieid));
MLhold$userid = factor(MLhold$userid, levels=levels(MLfull$userid));
MLhold$zipcode = factor(MLhold$zipcode, levels=levels(MLfull$zipcode));

allfeats = names(MLhold);
allfeatsjc = allfeats;
allfeatsjc = allfeatsjc[-3]; #rating
allfeatsca = c("userid", "movieid");
userfeats = c("gender","age","occupation","zipcode")
allfeatsjcnomovie = c("userid","movieid",userfeats)
allfeatsjcnouser = setdiff(allfeatsjc, userfeats);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnomovie)
print(allfeatsjcnouser)

print("Ranking JC features by Mutual Information on MLtrain")
pt = proc.time(); yrinfogain = information.gain(rating ~. , MLtrain); proc.time() - pt
yrinfogain = yrinfogain/log(2.0); #since it uses e as default for log
print(t(yrinfogain))

#entropy(tabulate(MLtrain$ddst),unit="log2") #outputs correct entropy
print("Entropy of JC features on MLtrain")
tabent <- function(y) {
	return(entropy(tabulate(y),unit="log2"))
}
pt = proc.time(); yrentropies = sapply(MLtrain[,-3], tabent); proc.time() - pt
print(yrentropies)

print("Information Gain Ratio of JC features on MLtrain")
yrigrs = yrentropies
pt = proc.time(); 
for(i in 1:length(yrigrs)) {
	yrigrs[i] = 1.0*yrinfogain[i,]/yrentropies[i]
}
proc.time() - pt
print(yrigrs)

sortedfeatsmi = row.names(yrinfogain)[order(yrinfogain)];
sortedfeatsigr = names(sort(yrigrs));

print("JC features sorted by MI")
print(sortedfeatsmi)
print("JC features sorted by IGR")
print(sortedfeatsigr)

sfmica = intersect(sortedfeatsmi, allfeatsca)
sfigrca = intersect(sortedfeatsigr, allfeatsca)
sfmijcnomovie = intersect(sortedfeatsmi, allfeatsjcnomovie)
sfigrjcnomovie = intersect(sortedfeatsigr, allfeatsjcnomovie)
sfmijcnouser = intersect(sortedfeatsmi, allfeatsjcnouser)
sfigrjcnouser = intersect(sortedfeatsigr, allfeatsjcnouser)

print("CA features sorted by MI")
print(sfmica)
print("CA features sorted by IGR")
print(sfigrca)
print("JCnomovie features sorted by MI")
print(sfmijcnomovie)
print("JCnomovie features sorted by IGR")
print(sfigrjcnomovie)
print("JCnouser features sorted by MI")
print(sfmijcnouser)
print("JCnouser features sorted by IGR")
print(sfigrjcnouser)

print("Running Filter-MI for JC on 5 classes")
pt = proc.time();
outset = myfilter(sortedfeatsmi, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for JC on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JC")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JC on 5 classes")
pt = proc.time();
outset = myfilter(sortedfeatsigr, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for JC on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JC")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running Filter-MI for CA on 5 classes")
pt = proc.time();
outset = myfilter(sfmica, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for CA on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for CA")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for CA on 5 classes")
pt = proc.time();
outset = myfilter(sfigrca, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for CA on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for CA")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running Filter-MI for JCnomovie on 5 classes")
pt = proc.time();
outset = myfilter(sfmijcnomovie, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for JCnomovie on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnomovie")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JCnomovie on 5 classes")
pt = proc.time();
outset = myfilter(sfigrjcnomovie, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnomovie on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnomovie")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running Filter-MI for JCnouser on 5 classes")
pt = proc.time();
outset = myfilter(sfmijcnouser, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for JCnouser on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JCnouser on 5 classes")
pt = proc.time();
outset = myfilter(sfigrjcnouser, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnouser on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

