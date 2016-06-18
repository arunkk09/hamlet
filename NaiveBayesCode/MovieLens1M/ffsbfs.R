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
#"userid","gender","age","occupation","zipcode"
userfeats = c("gender","age","occupation","zipcode")
allfeatsjcnomovie = c("userid","movieid",userfeats)
#"namewords","namepar","year","action","adventure","animation","childrens","comedy","crime","documentary","drama","fantasy","filmnoir","horror","musical","mystery","romance","scifi","thriller","war","western"
allfeatsjcnouser = setdiff(allfeatsjc, userfeats);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnomovie)
print(allfeatsjcnouser)

print("Running FFS for CA on 5 classes")
pt = proc.time();
outset = myffs(allfeatsca, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running FFS for JC on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjc, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running FFS for JCnomovie on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnomovie, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnomovie on 5 classes")
print("Hold out validation for JCnomovie")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnomovie")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running FFS for JCnouser on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnouser, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)


print("Running BFS for CA on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsca, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running BFS for JC on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjc, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnomovie on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnomovie, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnomovie on 5 classes")
print("Hold out validation for JCnomovie")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnomovie")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnouser on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnouser, 3, MLtrain, MLtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(MLtrain[,outset], MLtrain[,3], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, MLhold[,outset]), MLhold[,3], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(MLhold), nrow(outsettab))
print(acc)

