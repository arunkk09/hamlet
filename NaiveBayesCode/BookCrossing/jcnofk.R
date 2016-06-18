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
source("myFFS.R");
source("myBFS.R");
source("allentropyinfogain.R")
source("myfilter.R")
options(width=190)

BCtrain=read.csv("BCtrain.csv");
BCtest=read.csv("BCtest.csv");
BChold=read.csv("BChold.csv");
BCfull=rbind(BCtrain,BCtest,BChold) 

#userid,bookid,rating,titlewords,authorwords,year,publisher,country,age

BCtrain$bookid = factor(BCtrain$bookid, levels=levels(BCfull$bookid));
BCtrain$userid = factor(BCtrain$userid, levels=levels(BCfull$userid));
BCtrain$country = factor(BCtrain$country, levels=levels(BCfull$country));
BCtrain$publisher = factor(BCtrain$publisher, levels=levels(BCfull$publisher));
BCtrain$year = factor(BCtrain$year, levels=levels(BCfull$year));

BCtest$bookid = factor(BCtest$bookid, levels=levels(BCfull$bookid));
BCtest$userid = factor(BCtest$userid, levels=levels(BCfull$userid));
BCtest$country = factor(BCtest$country, levels=levels(BCfull$country));
BCtest$publisher = factor(BCtest$publisher, levels=levels(BCfull$publisher));
BCtest$year = factor(BCtest$year, levels=levels(BCfull$year));

BChold$bookid = factor(BChold$bookid, levels=levels(BCfull$bookid));
BChold$userid = factor(BChold$userid, levels=levels(BCfull$userid));
BChold$country = factor(BChold$country, levels=levels(BCfull$country));
BChold$publisher = factor(BChold$publisher, levels=levels(BCfull$publisher));
BChold$year = factor(BChold$year, levels=levels(BCfull$year));

allfeats = names(BChold);
allfeatsjc = allfeats;
allfeatsjc = allfeatsjc[-3]; #rating
allfeatsca = c("userid", "bookid");
allfeatsjcnofk = setdiff(allfeatsjc, allfeatsca);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnofk)

print("Running FFS for JCnofk on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnofk, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnofk on 5 classes")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running BFS for JCnofk on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnofk, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnofk on 5 classes")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Ranking JC features by Mutual Information on BCtrain")
pt = proc.time(); yrinfogain = information.gain(rating ~. , BCtrain); proc.time() - pt
yrinfogain = yrinfogain/log(2.0); #since it uses e as default for log
print(t(yrinfogain))

#entropy(tabulate(BCtrain$ddst),unit="log2") #outputs correct entropy
print("Entropy of JC features on BCtrain")
tabent <- function(y) {
        return(entropy(tabulate(y),unit="log2"))
}
pt = proc.time(); yrentropies = sapply(BCtrain[,-3], tabent); proc.time() - pt
print(yrentropies)

print("Information Gain Ratio of JC features on BCtrain")
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

sfmijcnofk = intersect(sortedfeatsmi, allfeatsjcnofk)
sfigrjcnofk = intersect(sortedfeatsigr, allfeatsjcnofk)

print("JCnofk features sorted by MI")
print(sfmijcnofk)
print("JCnofk features sorted by IGR")
print(sfigrjcnofk)

print("Running Filter-MI for JCnofk on 5 classes")
pt = proc.time();
outset = myfilter(sfmijcnofk, "rating", BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for JCnofk on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JCnofk on 5 classes")
pt = proc.time();
outset = myfilter(sfigrjcnofk, "rating", BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnofk on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,"rating"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,"rating"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

