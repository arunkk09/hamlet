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

LFtrain=read.csv("LFsub2train.csv");
LFtest=read.csv("LFsub2test.csv");
LFhold=read.csv("LFsub2hold.csv");
LFfull=rbind(LFtrain,LFtest,LFhold) 

LFtrain$artistid = factor(LFtrain$artistid, levels=levels(LFfull$artistid));
LFtrain$userid = factor(LFtrain$userid, levels=levels(LFfull$userid));
LFtrain$country = factor(LFtrain$country, levels=levels(LFfull$country));

LFtest$artistid = factor(LFtest$artistid, levels=levels(LFfull$artistid));
LFtest$userid = factor(LFtest$userid, levels=levels(LFfull$userid));
LFtest$country = factor(LFtest$country, levels=levels(LFfull$country));

LFhold$artistid = factor(LFhold$artistid, levels=levels(LFfull$artistid));
LFhold$userid = factor(LFhold$userid, levels=levels(LFfull$userid));
LFhold$country = factor(LFhold$country, levels=levels(LFfull$country));

allfeats = names(LFhold);
allfeatsjc = allfeats;
allfeatsjc = allfeatsjc[-3]; #plays
allfeatsca = c("userid", "artistid");
allfeatsjcnofk = setdiff(allfeatsjc, allfeatsca);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnofk)

print("Running FFS for JCnofk on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnofk, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnofk on 5 classes")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnofk on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnofk, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnofk on 5 classes")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Ranking JC features by Mutual Information on LFtrain")
pt = proc.time(); yrinfogain = information.gain(plays ~. , LFtrain); proc.time() - pt
yrinfogain = yrinfogain/log(2.0); #since it uses e as default for log
print(t(yrinfogain))

#entropy(tabulate(LFtrain$ddst),unit="log2") #outputs correct entropy
print("Entropy of JC features on LFtrain")
tabent <- function(y) {
        return(entropy(tabulate(y),unit="log2"))
}
pt = proc.time(); yrentropies = sapply(LFtrain[,-3], tabent); proc.time() - pt
print(yrentropies)

print("Information Gain Ratio of JC features on LFtrain")
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

print("JCnofk features sorted by IGR")
print(sfigrjcnofk)

print("Running Filter-MI for JCnofk on 5 classes")
pt = proc.time();
outset = myfilter(sfmijcnofk, "plays", LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-MI for JCnofk on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,"plays"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,"plays"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JCnofk on 5 classes")
pt = proc.time();
outset = myfilter(sfigrjcnofk, "plays", LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnofk on 5 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,"plays"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,"plays"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnofk")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)


