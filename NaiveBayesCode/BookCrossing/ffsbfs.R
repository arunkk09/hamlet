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
userfeats = c("country","age")
allfeatsjcnobook = c("userid","bookid",userfeats)
allfeatsjcnouser = setdiff(allfeatsjc, userfeats);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnobook)
print(allfeatsjcnouser)

print("Running FFS for CA on 5 classes")
pt = proc.time();
outset = myffs(allfeatsca, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running FFS for JC on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjc, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running FFS for JCnobook on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnobook, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnobook on 5 classes")
print("Hold out validation for JCnobook")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnobook")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running FFS for JCnouser on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnouser, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running BFS for CA on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsca, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running BFS for JC on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjc, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running BFS for JCnobook on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnobook, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnobook on 5 classes")
print("Hold out validation for JCnobook")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnobook")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

print("Running BFS for JCnouser on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnouser, 3, BCtrain, BCtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(BCtrain[,outset], BCtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, BChold[,outset]), BChold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(BChold), nrow(outsettab))
print(acc)

