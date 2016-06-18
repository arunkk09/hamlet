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

source("../../myNB.R")
source("../../myBFS.R");
source("../../myFFS.R");

WTtrain=read.csv("WTtrain.csv");
WTtest=read.csv("WTtest.csv");
WThold=read.csv("WThold.csv");
WTtrain=WTtrain[,-1] #get rid of sid
WTtest=WTtest[,-1]
WThold=WThold[,-1]
#WTfull=rbind(WTtrain,WTtest,WThold);

allfeats = names(WThold);
allfeatsjc = allfeats;
allfeatsjc = allfeatsjc[-1]; #weekly_sales (target)
allfeatsca = c("dept", "store", "purchaseid")
allfeatsjcnostores = allfeats;
allfeatsjcnostores = allfeatsjcnostores[-1];
allfeatsjcnostores = allfeatsjcnostores[-4];
allfeatsjcnostores = allfeatsjcnostores[-4];
allfeatsjcnofeatures = allfeats[2:6];

print(allfeats)
print(allfeatsca)
print(allfeatsjc)
print(allfeatsjcnostores)
print(allfeatsjcnofeatures)

print("Running FFS for CA on 7 classes")
pt = proc.time();
outset = myffs(allfeatsca, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for CA on 7 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

print("Running FFS for JC on 7 classes")
pt = proc.time();
outset = myffs(allfeatsjc, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JC on 7 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

print("Running FFS for JCnostores on 7 classes")
pt = proc.time();
outset = myffs(allfeatsjcnostores, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnostores on 7 classes")
print("Hold out validation for JCnostores")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnostores")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

print("Running FFS for JCnofeatures on 7 classes")
pt = proc.time();
outset = myffs(allfeatsjcnofeatures, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnofeatures on 7 classes")
print("Hold out validation for JCnofeatures")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnofeatures")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)


print("Running BFS for CA on 7 classes")
pt = proc.time();
outset = mybfs(allfeatsca, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for CA on 7 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

print("Running BFS for JC on 7 classes")
pt = proc.time();
outset = mybfs(allfeatsjc, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JC on 7 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

print("Running BFS for JCnostores on 7 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnostores, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnostores on 7 classes")
print("Hold out validation for JCnostores")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnostores")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

print("Running BFS for JCnofeatures on 7 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnofeatures, 1, WTtrain, WTtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnofeatures on 7 classes")
print("Hold out validation for JCnofeatures")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnofeatures")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)
