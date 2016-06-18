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

YRtrain=read.csv("YRtrain.csv");
YRtest=read.csv("YRtest.csv");
YRhold=read.csv("YRhold.csv");
YRtrain=YRtrain[,-1] #get rid of reviewid
YRtest=YRtest[,-1]
YRhold=YRhold[,-1]
YRfull=rbind(YRtrain,YRtest,YRhold);

YRfull$userid = factor(YRfull$userid)
YRfull$businessid = factor(YRfull$businessid)
YRfull$stars = factor(YRfull$stars)
YRfull$city = factor(YRfull$city)
YRfull$state = factor(YRfull$state)
YRfull$ustars = factor(YRfull$ustars)
YRfull$ureviewcnt = factor(YRfull$ureviewcnt)
YRfull$vuseful = factor(YRfull$vuseful)
YRfull$vfunny = factor(YRfull$vfunny)
YRfull$vcool = factor(YRfull$vcool)
YRfull$latitude = factor(YRfull$latitude)
YRfull$longitude = factor(YRfull$longitude)
YRfull$bstars = factor(YRfull$bstars)
YRfull$breviewcnt = factor(YRfull$breviewcnt)
YRfull$wday1 = factor(YRfull$wday1)
YRfull$wday2 = factor(YRfull$wday2)
YRfull$wday3 = factor(YRfull$wday3)
YRfull$wday4 = factor(YRfull$wday4)
YRfull$wday5 = factor(YRfull$wday5)
YRfull$wend1 = factor(YRfull$wend1)
YRfull$wend2 = factor(YRfull$wend2)
YRfull$wend3 = factor(YRfull$wend3)
YRfull$wend4 = factor(YRfull$wend4)
YRfull$wend5 = factor(YRfull$wend5)

YRtrain$userid = factor(YRtrain$userid, levels=levels(YRfull$userid));
YRtrain$businessid = factor(YRtrain$businessid, levels=levels(YRfull$businessid));
YRtrain$stars = factor(YRtrain$stars, levels=levels(YRfull$stars));
YRtrain$city = factor(YRtrain$city, levels=levels(YRfull$city));
YRtrain$state = factor(YRtrain$state, levels=levels(YRfull$state));
YRtrain$ustars = factor(YRtrain$ustars, levels=levels(YRfull$ustars));
YRtrain$ureviewcnt = factor(YRtrain$ureviewcnt, levels=levels(YRfull$ureviewcnt));
YRtrain$vuseful = factor(YRtrain$vuseful, levels=levels(YRfull$vuseful));
YRtrain$vfunny = factor(YRtrain$vfunny, levels=levels(YRfull$vfunny));
YRtrain$vcool = factor(YRtrain$vcool, levels=levels(YRfull$vcool));
YRtrain$latitude = factor(YRtrain$latitude, levels=levels(YRfull$latitude));
YRtrain$longitude = factor(YRtrain$longitude, levels=levels(YRfull$longitude));
YRtrain$bstars = factor(YRtrain$bstars, levels=levels(YRfull$bstars));
YRtrain$breviewcnt = factor(YRtrain$breviewcnt, levels=levels(YRfull$breviewcnt));
YRtrain$wday1 = factor(YRtrain$wday1, levels=levels(YRfull$wday1));
YRtrain$wday2 = factor(YRtrain$wday2, levels=levels(YRfull$wday2));
YRtrain$wday3 = factor(YRtrain$wday3, levels=levels(YRfull$wday3));
YRtrain$wday4 = factor(YRtrain$wday4, levels=levels(YRfull$wday4));
YRtrain$wday5 = factor(YRtrain$wday5, levels=levels(YRfull$wday5));
YRtrain$wend1 = factor(YRtrain$wend1, levels=levels(YRfull$wend1));
YRtrain$wend2 = factor(YRtrain$wend2, levels=levels(YRfull$wend2));
YRtrain$wend3 = factor(YRtrain$wend3, levels=levels(YRfull$wend3));
YRtrain$wend4 = factor(YRtrain$wend4, levels=levels(YRfull$wend4));
YRtrain$wend5 = factor(YRtrain$wend5, levels=levels(YRfull$wend5));

YRtest$userid = factor(YRtest$userid, levels=levels(YRfull$userid));
YRtest$businessid = factor(YRtest$businessid, levels=levels(YRfull$businessid));
YRtest$stars = factor(YRtest$stars, levels=levels(YRfull$stars));
YRtest$city = factor(YRtest$city, levels=levels(YRfull$city));
YRtest$state = factor(YRtest$state, levels=levels(YRfull$state));
YRtest$ustars = factor(YRtest$ustars, levels=levels(YRfull$ustars));
YRtest$ureviewcnt = factor(YRtest$ureviewcnt, levels=levels(YRfull$ureviewcnt));
YRtest$vuseful = factor(YRtest$vuseful, levels=levels(YRfull$vuseful));
YRtest$vfunny = factor(YRtest$vfunny, levels=levels(YRfull$vfunny));
YRtest$vcool = factor(YRtest$vcool, levels=levels(YRfull$vcool));
YRtest$latitude = factor(YRtest$latitude, levels=levels(YRfull$latitude));
YRtest$longitude = factor(YRtest$longitude, levels=levels(YRfull$longitude));
YRtest$bstars = factor(YRtest$bstars, levels=levels(YRfull$bstars));
YRtest$breviewcnt = factor(YRtest$breviewcnt, levels=levels(YRfull$breviewcnt));
YRtest$wday1 = factor(YRtest$wday1, levels=levels(YRfull$wday1));
YRtest$wday2 = factor(YRtest$wday2, levels=levels(YRfull$wday2));
YRtest$wday3 = factor(YRtest$wday3, levels=levels(YRfull$wday3));
YRtest$wday4 = factor(YRtest$wday4, levels=levels(YRfull$wday4));
YRtest$wday5 = factor(YRtest$wday5, levels=levels(YRfull$wday5));
YRtest$wend1 = factor(YRtest$wend1, levels=levels(YRfull$wend1));
YRtest$wend2 = factor(YRtest$wend2, levels=levels(YRfull$wend2));
YRtest$wend3 = factor(YRtest$wend3, levels=levels(YRfull$wend3));
YRtest$wend4 = factor(YRtest$wend4, levels=levels(YRfull$wend4));
YRtest$wend5 = factor(YRtest$wend5, levels=levels(YRfull$wend5));

YRhold$userid = factor(YRhold$userid, levels=levels(YRfull$userid));
YRhold$businessid = factor(YRhold$businessid, levels=levels(YRfull$businessid));
YRhold$stars = factor(YRhold$stars, levels=levels(YRfull$stars));
YRhold$city = factor(YRhold$city, levels=levels(YRfull$city));
YRhold$state = factor(YRhold$state, levels=levels(YRfull$state));
YRhold$ustars = factor(YRhold$ustars, levels=levels(YRfull$ustars));
YRhold$ureviewcnt = factor(YRhold$ureviewcnt, levels=levels(YRfull$ureviewcnt));
YRhold$vuseful = factor(YRhold$vuseful, levels=levels(YRfull$vuseful));
YRhold$vfunny = factor(YRhold$vfunny, levels=levels(YRfull$vfunny));
YRhold$vcool = factor(YRhold$vcool, levels=levels(YRfull$vcool));
YRhold$latitude = factor(YRhold$latitude, levels=levels(YRfull$latitude));
YRhold$longitude = factor(YRhold$longitude, levels=levels(YRfull$longitude));
YRhold$bstars = factor(YRhold$bstars, levels=levels(YRfull$bstars));
YRhold$breviewcnt = factor(YRhold$breviewcnt, levels=levels(YRfull$breviewcnt));
YRhold$wday1 = factor(YRhold$wday1, levels=levels(YRfull$wday1));
YRhold$wday2 = factor(YRhold$wday2, levels=levels(YRfull$wday2));
YRhold$wday3 = factor(YRhold$wday3, levels=levels(YRfull$wday3));
YRhold$wday4 = factor(YRhold$wday4, levels=levels(YRfull$wday4));
YRhold$wday5 = factor(YRhold$wday5, levels=levels(YRfull$wday5));
YRhold$wend1 = factor(YRhold$wend1, levels=levels(YRfull$wend1));
YRhold$wend2 = factor(YRhold$wend2, levels=levels(YRfull$wend2));
YRhold$wend3 = factor(YRhold$wend3, levels=levels(YRfull$wend3));
YRhold$wend4 = factor(YRhold$wend4, levels=levels(YRfull$wend4));
YRhold$wend5 = factor(YRhold$wend5, levels=levels(YRfull$wend5));

allfeats = names(YRhold);
allfeatsjc = allfeats;
allfeatsjc = allfeatsjc[-1]; #stars
allfeatsca = c("userid", "businessid");
#"userid","reviewcnt","stars","vuseful","vfunny","vcool","gender"
userfeats = c("ureviewcnt","ustars","vuseful","vfunny","vcool","gender")
allfeatsjcnobusiness = c("userid","businessid",userfeats)
#"businessid","city","state","latitude","longitude","bstars","breviewcnt","open",wends,wdays,cats
allfeatsjcnouser = setdiff(allfeatsjc, userfeats);

print(allfeats)
print(allfeatsjc)
print(allfeatsca)
print(allfeatsjcnobusiness)
print(allfeatsjcnouser)

print("Running FFS for CA on 5 classes")
pt = proc.time();
outset = myffs(allfeatsca, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)

print("Running FFS for JC on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjc, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)

print("Running FFS for JCnobusiness on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnobusiness, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnobusiness on 5 classes")
print("Hold out validation for JCnobusiness")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnobusiness")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)

print("Running FFS for JCnouser on 5 classes")
pt = proc.time();
outset = myffs(allfeatsjcnouser, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished FFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)



print("Running BFS for CA on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsca, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for CA on 5 classes")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)

print("Running BFS for JC on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjc, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JC on 5 classes")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnobusiness on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnobusiness, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnobusiness on 5 classes")
print("Hold out validation for JCnobusiness")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnobusiness")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)

print("Running BFS for JCnouser on 5 classes")
pt = proc.time();
outset = mybfs(allfeatsjcnouser, 1, YRtrain, YRtest, 'RMSE');
print(proc.time() - pt)
print("Finished BFS for JCnouser on 5 classes")
print("Hold out validation for JCnouser")
pt = proc.time(); outsettr = myNBlog(YRtrain[,outset], YRtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, YRhold[,outset]), YRhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnouser")
acc = geterr(outsettab, 'RMSE', nrow(YRhold), nrow(outsettab))
print(acc)
