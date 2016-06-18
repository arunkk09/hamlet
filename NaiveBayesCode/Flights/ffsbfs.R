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

OFtrain = read.csv("OFtrain.csv");
OFtest = read.csv("OFtest.csv");
OFhold = read.csv("OFhold.csv");
OFfull = rbind(OFtrain, OFtest, OFhold);
OFfull$airlineid = factor(OFfull$airlineid);
OFfull$sairportid = factor(OFfull$sairportid);
OFfull$dairportid = factor(OFfull$dairportid);
OFfull$stimezone = factor(OFfull$stimezone);
OFfull$dtimezone = factor(OFfull$dtimezone);
OFfull$slatitude = factor(OFfull$slatitude);
OFfull$dlatitude = factor(OFfull$dlatitude);
OFfull$slongitude = factor(OFfull$slongitude);
OFfull$dlongitude = factor(OFfull$dlongitude);
OFfull$name1 = factor(OFfull$name1);
OFfull$scity = factor(OFfull$scity);
OFfull$dcity = factor(OFfull$dcity);
OFfull$scountry = factor(OFfull$scountry);
OFfull$dcountry = factor(OFfull$dcountry);
OFfull$sdst = factor(OFfull$sdst);
OFfull$ddst = factor(OFfull$ddst);
OFfull$acountry = factor(OFfull$acountry);

OFtrain$airlineid = factor(OFtrain$airlineid, levels=levels(OFfull$airlineid));
OFtrain$sairportid = factor(OFtrain$sairportid, levels=levels(OFfull$sairportid));
OFtrain$dairportid = factor(OFtrain$dairportid, levels=levels(OFfull$dairportid));
OFtrain$stimezone = factor(OFtrain$stimezone, levels=levels(OFfull$stimezone));
OFtrain$dtimezone = factor(OFtrain$dtimezone, levels=levels(OFfull$dtimezone));
OFtrain$slatitude = factor(OFtrain$slatitude, levels=levels(OFfull$slatitude));
OFtrain$dlatitude = factor(OFtrain$dlatitude, levels=levels(OFfull$dlatitude));
OFtrain$slongitude = factor(OFtrain$slongitude, levels=levels(OFfull$slongitude));
OFtrain$dlongitude = factor(OFtrain$dlongitude, levels=levels(OFfull$dlongitude));
OFtrain$name1 = factor(OFtrain$name1, levels=levels(OFfull$name1));
OFtrain$scity = factor(OFtrain$scity, levels=levels(OFfull$scity));
OFtrain$dcity = factor(OFtrain$dcity, levels=levels(OFfull$dcity));
OFtrain$scountry = factor(OFtrain$scountry, levels=levels(OFfull$scountry));
OFtrain$dcountry = factor(OFtrain$dcountry, levels=levels(OFfull$dcountry));
OFtrain$sdst = factor(OFtrain$sdst, levels=levels(OFfull$sdst));
OFtrain$ddst = factor(OFtrain$ddst, levels=levels(OFfull$ddst));
OFtrain$acountry = factor(OFtrain$acountry, levels=levels(OFfull$acountry));

OFtest$airlineid = factor(OFtest$airlineid, levels=levels(OFfull$airlineid));
OFtest$sairportid = factor(OFtest$sairportid, levels=levels(OFfull$sairportid));
OFtest$dairportid = factor(OFtest$dairportid, levels=levels(OFfull$dairportid));
OFtest$stimezone = factor(OFtest$stimezone, levels=levels(OFfull$stimezone));
OFtest$dtimezone = factor(OFtest$dtimezone, levels=levels(OFfull$dtimezone));
OFtest$slatitude = factor(OFtest$slatitude, levels=levels(OFfull$slatitude));
OFtest$dlatitude = factor(OFtest$dlatitude, levels=levels(OFfull$dlatitude));
OFtest$slongitude = factor(OFtest$slongitude, levels=levels(OFfull$slongitude));
OFtest$dlongitude = factor(OFtest$dlongitude, levels=levels(OFfull$dlongitude));
OFtest$name1 = factor(OFtest$name1, levels=levels(OFfull$name1));
OFtest$scity = factor(OFtest$scity, levels=levels(OFfull$scity));
OFtest$dcity = factor(OFtest$dcity, levels=levels(OFfull$dcity));
OFtest$scountry = factor(OFtest$scountry, levels=levels(OFfull$scountry));
OFtest$dcountry = factor(OFtest$dcountry, levels=levels(OFfull$dcountry));
OFtest$sdst = factor(OFtest$sdst, levels=levels(OFfull$sdst));
OFtest$ddst = factor(OFtest$ddst, levels=levels(OFfull$ddst));
OFtest$acountry = factor(OFtest$acountry, levels=levels(OFfull$acountry));

OFhold$airlineid = factor(OFhold$airlineid, levels=levels(OFfull$airlineid));
OFhold$sairportid = factor(OFhold$sairportid, levels=levels(OFfull$sairportid));
OFhold$dairportid = factor(OFhold$dairportid, levels=levels(OFfull$dairportid));
OFhold$stimezone = factor(OFhold$stimezone, levels=levels(OFfull$stimezone));
OFhold$dtimezone = factor(OFhold$dtimezone, levels=levels(OFfull$dtimezone));
OFhold$slatitude = factor(OFhold$slatitude, levels=levels(OFfull$slatitude));
OFhold$dlatitude = factor(OFhold$dlatitude, levels=levels(OFfull$dlatitude));
OFhold$slongitude = factor(OFhold$slongitude, levels=levels(OFfull$slongitude));
OFhold$dlongitude = factor(OFhold$dlongitude, levels=levels(OFfull$dlongitude));
OFhold$name1 = factor(OFhold$name1, levels=levels(OFfull$name1));
OFhold$scity = factor(OFhold$scity, levels=levels(OFfull$scity));
OFhold$dcity = factor(OFhold$dcity, levels=levels(OFfull$dcity));
OFhold$scountry = factor(OFhold$scountry, levels=levels(OFfull$scountry));
OFhold$dcountry = factor(OFhold$dcountry, levels=levels(OFfull$dcountry));
OFhold$sdst = factor(OFhold$sdst, levels=levels(OFfull$sdst));
OFhold$ddst = factor(OFhold$ddst, levels=levels(OFfull$ddst));
OFhold$acountry = factor(OFhold$acountry, levels=levels(OFfull$acountry));

allfeats = names(OFtrain);
allfeatsjc = allfeats;
allfeatsjc = allfeatsjc[-1];
airlinefs = c("country", "name1", "name4", "active", "name2")
sairportfs = c("scity", "scountry", "sdst", "stimezone", "slongitude", "slatitude")
dairportfs = c("dcity", "dcountry", "ddst", "dtimezone", "dlongitude", "dlatitude")
allfeatsca = setdiff(setdiff(setdiff(allfeatsjc, airlinefs), sairportfs), dairportfs)
allfeatsjcnor1 = setdiff(allfeatsjc, airlinefs)
allfeatsjcnor2 = setdiff(allfeatsjc, sairportfs)
allfeatsjcnor3 = setdiff(allfeatsjc, dairportfs)
allfeatsjcnor1r2 = setdiff(setdiff(allfeatsjc, airlinefs), sairportfs)
allfeatsjcnor1r3 = setdiff(setdiff(allfeatsjc, airlinefs), dairportfs)
allfeatsjcnor2r3 = setdiff(setdiff(allfeatsjc, sairportfs), dairportfs)

#allfeatsca = c("airlineid","sairportid","dairportid","eq8","eq17","eq22","eq2","eq1","eq19","eq20","eq28","eq46","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","eq15","eq31","eq5");
#allfeatsjcnor1 = c("airlineid","sairportid","dairportid","scity","dcity","dcountry","scountry","ddst","sdst","stimezone","dtimezone","dlongitude","slongitude","eq8","eq17","eq22","eq2","eq1","eq19","eq20","eq28","slatitude","dlatitude","eq46","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","eq15","eq31","eq5")
#allfeatsjcnor2 = c("airlineid","sairportid","dairportid","dcity","country","dcountry","ddst","dtimezone","dlongitude","eq8","eq17","eq22","name1","eq2","eq1","eq19","eq20","eq28","dlatitude","eq46","name4","active","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","name2","eq15","eq31","eq5")
#allfeatsjcnor3 = c("airlineid","sairportid","dairportid","scity","country","scountry","sdst","stimezone","slongitude","eq8","eq17","eq22","name1","eq2","eq1","eq19","eq20","eq28","slatitude","eq46","name4","active","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","name2","eq15","eq31","eq5")
#allfeatsjcnor1r2 = c("airlineid","sairportid","dairportid","dcity","dcountry","ddst","dtimezone","dlongitude","eq8","eq17","eq22","eq2","eq1","eq19","eq20","eq28","dlatitude","eq46","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","eq15","eq31","eq5")
#allfeatsjcnor1r3 = c("airlineid","sairportid","dairportid","scity","scountry","sdst","stimezone","slongitude","eq8","eq17","eq22","eq2","eq1","eq19","eq20","eq28","slatitude","eq46","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","eq15","eq31","eq5")
#allfeatsjcnor2r3 = c("airlineid","sairportid","dairportid","country","eq8","eq17","eq22","name1","eq2","eq1","eq19","eq20","eq28","eq46","name4","active","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","name2","eq15","eq31","eq5")

print("CA features")
print(allfeatsca)
print("JC features")
print(allfeatsjc)
print("JCnor1 features")
print(allfeatsjcnor1)
print("JCnor2 features")
print(allfeatsjcnor2)
print("JCnor3 features")
print(allfeatsjcnor3)
print("JCnor1r2 features")
print(allfeatsjcnor1r2)
print("JCnor1r3 features")
print(allfeatsjcnor1r3)
print("JCnor2r3 features")
print(allfeatsjcnor2r3)

print("Running FFS for CA with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsca, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for CA with top40 with 01")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JC with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjc, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JC with top40 with 01")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JCnor1 with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjcnor1, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnor1 with top40 with 01")
print("Hold out validation for JCnor1")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnor1")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JCnor2 with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjcnor2, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnor2 with top40 with 01")
print("Hold out validation for JCnor2")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnor2")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JCnor3 with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjcnor3, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnor3 with top40 with 01")
print("Hold out validation for JCnor3")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnor3")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JCnor1r2 with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjcnor1r2, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnor1r2 with top40 with 01")
print("Hold out validation for JCnor1r2")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnor1r2")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JCnor1r3 with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjcnor1r3, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnor1r3 with top40 with 01")
print("Hold out validation for JCnor1r3")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnor1r3")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running FFS for JCnor2r3 with top40 with 01")
pt = proc.time();
outset = myffs(allfeatsjcnor2r3, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnor2r3 with top40 with 01")
print("Hold out validation for JCnor2r3")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnor2r3")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)



print("Running BFS for CA with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsca, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for CA with top40 with 01")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JC with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjc, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JC with top40 with 01")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JCnor1 with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnor1, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnor1 with top40 with 01")
print("Hold out validation for JCnor1")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnor1")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JCnor2 with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnor2, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnor2 with top40 with 01")
print("Hold out validation for JCnor2")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnor2")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JCnor3 with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnor3, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnor3 with top40 with 01")
print("Hold out validation for JCnor3")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnor3")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JCnor1r2 with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnor1r2, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnor1r2 with top40 with 01")
print("Hold out validation for JCnor1r2")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnor1r2")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JCnor1r3 with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnor1r3, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnor1r3 with top40 with 01")
print("Hold out validation for JCnor1r3")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnor1r3")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running BFS for JCnor2r3 with top40 with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnor2r3, 1, OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnor2r3 with top40 with 01")
print("Hold out validation for JCnor2r3")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,1], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,1], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnor2r3")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

