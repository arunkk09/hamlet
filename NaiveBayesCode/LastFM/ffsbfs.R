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
#userid,gender,age,country,year
userfeats = c("gender","age","country","year")
allfeatsjcnoartist = c("userid","artistid",userfeats)
#artistid,numnamewords,numscrobbles,numlistens,numtoplistens,rock,electronic,indie,pop,hiphop,metal,punk,folk,rap,hardcore,dance,instrumental,jazz,british,industrial
allfeatsjcnouser = setdiff(allfeatsjc, userfeats);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnoartist)
print(allfeatsjcnouser)

print("Running FFS for CA on 5 classes")
pt = proc.time();
outset = myffs(allfeatsca, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running FFS for JC on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjc, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running FFS for JCnoartist on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnoartist, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnoartist on 5 classes")
print("Hold out validation for JCnoartist")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnoartist")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running FFS for JCnouser on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnouser, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)


print("Running BFS for CA on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsca, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running BFS for JC on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjc, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnoartist on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnoartist, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnoartist on 5 classes")
print("Hold out validation for JCnoartist")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnoartist")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnouser on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnouser, 3, LFtrain, LFtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(LFtrain[,outset], LFtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, LFhold[,outset]), LFhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(LFhold), nrow(outsettab))
print(acc)

