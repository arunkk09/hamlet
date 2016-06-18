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
OFtrain = read.csv("OFtrain.csv");
OFtest = read.csv("OFtest.csv");
OFhold = read.csv("OFhold.csv");

allfeatsjc = names(OFtrain);

OFfull = rbind(OFtrain, OFtest, OFhold);
OFfull$airlineid = factor(OFfull$airlineid);
OFfull$name1 = factor(OFfull$name1);
OFfull$sairportid = factor(OFfull$sairportid);
OFfull$dairportid = factor(OFfull$dairportid);
OFfull$stimezone = factor(OFfull$stimezone);
OFfull$dtimezone = factor(OFfull$dtimezone);
OFfull$slatitude = factor(OFfull$slatitude);
OFfull$dlatitude = factor(OFfull$dlatitude);
OFfull$slongitude = factor(OFfull$slongitude);
OFfull$dlongitude = factor(OFfull$dlongitude);
OFfull$scity = factor(OFfull$scity);
OFfull$dcity = factor(OFfull$dcity);
OFfull$scountry = factor(OFfull$scountry);
OFfull$dcountry = factor(OFfull$dcountry);
OFfull$sdst = factor(OFfull$sdst);
OFfull$ddst = factor(OFfull$ddst);
OFfull$acountry = factor(OFfull$acountry);

OFtrain$airlineid = factor(OFtrain$airlineid, levels=levels(OFfull$airlineid));
OFtrain$name1 = factor(OFtrain$name1, levels=levels(OFfull$name1));
OFtrain$sairportid = factor(OFtrain$sairportid, levels=levels(OFfull$sairportid));
OFtrain$dairportid = factor(OFtrain$dairportid, levels=levels(OFfull$dairportid));
OFtrain$stimezone = factor(OFtrain$stimezone, levels=levels(OFfull$stimezone));
OFtrain$dtimezone = factor(OFtrain$dtimezone, levels=levels(OFfull$dtimezone));
OFtrain$slatitude = factor(OFtrain$slatitude, levels=levels(OFfull$slatitude));
OFtrain$dlatitude = factor(OFtrain$dlatitude, levels=levels(OFfull$dlatitude));
OFtrain$slongitude = factor(OFtrain$slongitude, levels=levels(OFfull$slongitude));
OFtrain$dlongitude = factor(OFtrain$dlongitude, levels=levels(OFfull$dlongitude));
OFtrain$scity = factor(OFtrain$scity, levels=levels(OFfull$scity));
OFtrain$dcity = factor(OFtrain$dcity, levels=levels(OFfull$dcity));
OFtrain$scountry = factor(OFtrain$scountry, levels=levels(OFfull$scountry));
OFtrain$dcountry = factor(OFtrain$dcountry, levels=levels(OFfull$dcountry));
OFtrain$sdst = factor(OFtrain$sdst, levels=levels(OFfull$sdst));
OFtrain$ddst = factor(OFtrain$ddst, levels=levels(OFfull$ddst));
OFtrain$acountry = factor(OFtrain$acountry, levels=levels(OFfull$acountry));

OFtest$airlineid = factor(OFtest$airlineid, levels=levels(OFfull$airlineid));
OFtest$name1 = factor(OFtest$name1, levels=levels(OFfull$name1));
OFtest$sairportid = factor(OFtest$sairportid, levels=levels(OFfull$sairportid));
OFtest$dairportid = factor(OFtest$dairportid, levels=levels(OFfull$dairportid));
OFtest$stimezone = factor(OFtest$stimezone, levels=levels(OFfull$stimezone));
OFtest$dtimezone = factor(OFtest$dtimezone, levels=levels(OFfull$dtimezone));
OFtest$slatitude = factor(OFtest$slatitude, levels=levels(OFfull$slatitude));
OFtest$dlatitude = factor(OFtest$dlatitude, levels=levels(OFfull$dlatitude));
OFtest$slongitude = factor(OFtest$slongitude, levels=levels(OFfull$slongitude));
OFtest$dlongitude = factor(OFtest$dlongitude, levels=levels(OFfull$dlongitude));
OFtest$scity = factor(OFtest$scity, levels=levels(OFfull$scity));
OFtest$dcity = factor(OFtest$dcity, levels=levels(OFfull$dcity));
OFtest$scountry = factor(OFtest$scountry, levels=levels(OFfull$scountry));
OFtest$dcountry = factor(OFtest$dcountry, levels=levels(OFfull$dcountry));
OFtest$sdst = factor(OFtest$sdst, levels=levels(OFfull$sdst));
OFtest$ddst = factor(OFtest$ddst, levels=levels(OFfull$ddst));
OFtest$acountry = factor(OFtest$acountry, levels=levels(OFfull$acountry));

OFhold$airlineid = factor(OFhold$airlineid, levels=levels(OFfull$airlineid));
OFhold$name1 = factor(OFhold$name1, levels=levels(OFfull$name1));
OFhold$sairportid = factor(OFhold$sairportid, levels=levels(OFfull$sairportid));
OFhold$dairportid = factor(OFhold$dairportid, levels=levels(OFfull$dairportid));
OFhold$stimezone = factor(OFhold$stimezone, levels=levels(OFfull$stimezone));
OFhold$dtimezone = factor(OFhold$dtimezone, levels=levels(OFfull$dtimezone));
OFhold$slatitude = factor(OFhold$slatitude, levels=levels(OFfull$slatitude));
OFhold$dlatitude = factor(OFhold$dlatitude, levels=levels(OFfull$dlatitude));
OFhold$slongitude = factor(OFhold$slongitude, levels=levels(OFfull$slongitude));
OFhold$dlongitude = factor(OFhold$dlongitude, levels=levels(OFfull$dlongitude));
OFhold$scity = factor(OFhold$scity, levels=levels(OFfull$scity));
OFhold$dcity = factor(OFhold$dcity, levels=levels(OFfull$dcity));
OFhold$scountry = factor(OFhold$scountry, levels=levels(OFfull$scountry));
OFhold$dcountry = factor(OFhold$dcountry, levels=levels(OFfull$dcountry));
OFhold$sdst = factor(OFhold$sdst, levels=levels(OFfull$sdst));
OFhold$ddst = factor(OFhold$ddst, levels=levels(OFfull$ddst));
OFhold$acountry = factor(OFhold$acountry, levels=levels(OFfull$acountry));

print("JC features")
print(allfeatsjc)
XSfeats = c("eq8","eq17","eq22","eq2","eq1","eq19","eq20","eq28","eq46","eq3","eq71","eq25","eq30","eq4","eq45","eq14","eq12","eq15","eq31","eq5")
airlfeats = c("acountry", "name1", "name4", "active", "name2")
sairportfs = c("scity", "scountry", "sdst", "stimezone", "slongitude", "slatitude")
dairportfs = c("dcity", "dcountry", "ddst", "dtimezone", "dlongitude", "dlatitude")
#airlfeats = setdiff(names(airl), "airlineid")

print("Ranking JC features by Mutual Information on OFtrain")
pt = proc.time(); ofinfogain = information.gain(codeshare ~. , OFtrain); proc.time() - pt
ofinfogain = ofinfogain/log(2.0); #since it uses e as default for log
print(t(ofinfogain))

#entropy(tabulate(OFtrain$ddst),unit="log2") #outputs correct entropy
print("Entropy of JC features on OFtrain")
tabent <- function(y) {
	return(entropy(tabulate(y),unit="log2"))
}
pt = proc.time(); ofentropies = sapply(OFtrain[,-1], tabent); proc.time() - pt
print(ofentropies)

print("Information Gain Ratio of JC features on OFtrain")
ofigrs = ofentropies
pt = proc.time(); 
for(i in 1:length(ofigrs)) {
	ofigrs[i] = 1.0*ofinfogain[i,]/ofentropies[i]
}
proc.time() - pt
print(ofigrs)

sortedfeatsmi = row.names(ofinfogain)[order(ofinfogain)];
sortedfeatsigr = names(sort(ofigrs));

print("JC features sorted by MI")
print(sortedfeatsmi)
print("JC features sorted by IGR")
print(sortedfeatsigr)

print("JCnor1 features sorted by MI")
sortedfeatsminor1 = setdiff(sortedfeatsmi, airlfeats)
print(sortedfeatsminor1)
print("JCnor1 features sorted by IGR")
sortedfeatsigrnor1 = setdiff(sortedfeatsigr, airlfeats)
print(sortedfeatsigrnor1)

print("CA features sorted by MI")
sortedfeatsmica = setdiff(sortedfeatsmi, airlfeats)
sortedfeatsmica = setdiff(sortedfeatsmica, sairportfs)
sortedfeatsmica = setdiff(sortedfeatsmica, dairportfs)
print(sortedfeatsmica)
print("CA features sorted by IGR")
sortedfeatsigrca = setdiff(sortedfeatsigr, airlfeats)
sortedfeatsigrca = setdiff(sortedfeatsigrca, sairportfs)
sortedfeatsigrca = setdiff(sortedfeatsigrca, dairportfs)
print(sortedfeatsigrca)

print("Running Filter-MI for JC with top40 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsmi, "codeshare", OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished Filter-MI for JC with top40 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,"codeshare"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,"codeshare"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JC")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)
print("Running Filter-IGR for JC with top40 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsigr, "codeshare", OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished Filter-IGR for JC with top40 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,"codeshare"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,"codeshare"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JC")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running Filter-MI for JCnor1 with top40 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsminor1, "codeshare", OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished Filter-MI for JCnor1 with top40 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,"codeshare"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,"codeshare"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnor1")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)
print("Running Filter-IGR for JCnor1 with top40 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsigrnor1, "codeshare", OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnor1 with top40 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,"codeshare"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,"codeshare"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnor1")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)

print("Running Filter-MI for CA with top40 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsmica, "codeshare", OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished Filter-MI for CA with top40 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,"codeshare"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,"codeshare"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for CA")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)
print("Running Filter-IGR for CA with top40 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsigrca, "codeshare", OFtrain, OFtest, '01');
print(proc.time() - pt)
print("Finished Filter-IGR for CA with top40 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(OFtrain[,outset], OFtrain[,"codeshare"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, OFhold[,outset]), OFhold[,"codeshare"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for CA")
acc = geterr(outsettab, '01', nrow(OFhold))
print(acc)
