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
source("myBFS.R");
source("myFFS.R");
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
allfeatsjcnofk = setdiff(allfeatsjc, allfeatsca);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnofk)

print("Running FFS for JCnofk on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnofk, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnofk on 5 classes")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnofk on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnofk, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnofk on 5 classes")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

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

sfmijcnofk = intersect(sortedfeatsmi, allfeatsca)
sfigrjcnofk = intersect(sortedfeatsigr, allfeatsca)

print("JCnofk features sorted by MI")
print(sfmijcnofk)
print("JCnofk features sorted by IGR")
print(sfigrjcnofk)

print("Running Filter-MI for JCnofk on 5 classes")
pt = proc.time();
outset = myfilter(sfmijcnofk, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for JCnofk on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JCnofk on 5 classes")
pt = proc.time();
outset = myfilter(sfigrjcnofk, "rating", MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnofk on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

