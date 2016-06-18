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

source("myNB.R")
source("myBFS.R");
source("myFFS.R");
source("allentropyinfogain.R")
source("myfilter.R")
options(width=190)

EHtrain = read.csv("EHtrain10re.csv");
EHtest = read.csv("EHtest10re.csv");
EHhold = read.csv("EHhold10re.csv");
EHtrain = EHtrain[,-1]; #get rid of srch_id
EHtest = EHtest[,-1];
EHhold = EHhold[,-1];
EHall = rbind(EHtrain, EHtest, EHhold);

EHtrain$prop_id = factor(EHtrain$prop_id, levels=levels(EHall$prop_id));
EHtrain$prop_country_id = factor(EHtrain$prop_country_id, levels=levels(EHall$prop_country_id));
EHtrain$site_id = factor(EHtrain$site_id, levels=levels(EHall$site_id));
EHtrain$visitor_location_country_id = factor(EHtrain$visitor_location_country_id, levels=levels(EHall$visitor_location_country_id));
EHtrain$srch_destination_id = factor(EHtrain$srch_destination_id, levels=levels(EHall$srch_destination_id));

EHtest$prop_id = factor(EHtest$prop_id, levels=levels(EHall$prop_id));
EHtest$prop_country_id = factor(EHtest$prop_country_id, levels=levels(EHall$prop_country_id));
EHtest$site_id = factor(EHtest$site_id, levels=levels(EHall$site_id));
EHtest$visitor_location_country_id = factor(EHtest$visitor_location_country_id, levels=levels(EHall$visitor_location_country_id));
EHtest$srch_destination_id = factor(EHtest$srch_destination_id, levels=levels(EHall$srch_destination_id));

EHhold$prop_id = factor(EHhold$prop_id, levels=levels(EHall$prop_id));
EHhold$prop_country_id = factor(EHhold$prop_country_id, levels=levels(EHall$prop_country_id));
EHhold$site_id = factor(EHhold$site_id, levels=levels(EHall$site_id));
EHhold$visitor_location_country_id = factor(EHhold$visitor_location_country_id, levels=levels(EHall$visitor_location_country_id));
EHhold$srch_destination_id = factor(EHhold$srch_destination_id, levels=levels(EHall$srch_destination_id));

allfeats = names(EHhold);
allfeatsjc = allfeats[-2]; #removing the target "position"
allfeatsfk = c("prop_id")
allfeatsjcnofk = setdiff(allfeatsjc, allfeatsfk);

print(allfeats)
print(allfeatsjc)
print(allfeatsfk)
print(allfeatsjcnofk)

print("Running FFS for JCnofk with 01")
pt = proc.time();
outset = myffs(allfeatsjcnofk, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnofk with 01")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnofk")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running BFS for JCnofk with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnofk, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnofk with 01")
print("Hold out validation for JCnofk")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnofk")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Ranking JC features by Mutual Information on EHtrain")
pt = proc.time(); ofinfogain = information.gain(position ~. , EHtrain[,-1]); proc.time() - pt
ofinfogain = ofinfogain/log(2.0); #since it uses e as default for log
print(t(ofinfogain))

#entropy(tabulate(EHtrain$ddst),unit="log2") #outputs correct entropy
print("Entropy of JC features on EHtrain")
tabent <- function(y) {
	return(entropy(tabulate(y),unit="log2"))
}
pt = proc.time(); ofentropies = sapply(EHtrain[,-c(1,3)], tabent); proc.time() - pt
print(ofentropies)

print("Information Gain Ratio of JC features on EHtrain")
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

print("JCnofk features sorted by MI")
sortedfeatsminofk = intersect(sortedfeatsmi, allfeatsjcnofk)
print(sortedfeatsminofk)
print("JCnofk features sorted by IGR")
sortedfeatsigrnofk = intersect(sortedfeatsigr, allfeatsjcnofk)
print(sortedfeatsigrnofk)

print("Running Filter-MI for JCnofk with 01")
pt = proc.time();
outset = myfilter(sortedfeatsminofk, "position", EHtrain[,-1], EHtest[,-1], '01');
print(proc.time() - pt)
print("Finished Filter-MI for JCnofk with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnofk")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running Filter-IGR for JCnofk with 01")
pt = proc.time();
outset = myfilter(sortedfeatsigrnofk, "position", EHtrain[,-1], EHtest[,-1], '01');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnofk with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnofk")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

