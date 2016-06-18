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
searches = read.csv("searches_disc10re.csv");
hotels = read.csv("hotels_disc10re.csv");

allfeatsjc = names(EHhold);

searchesu=as.data.frame(unique(EHtest$srch_id))
names(searchesu)=c("srch_id")
searchess=merge(searchesu,searches,by="srch_id")
hotelsu=as.data.frame(unique(EHtest$prop_id))
names(hotelsu)=c("prop_id")
hotelss=merge(hotelsu,hotels,by="prop_id")

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

searchess$site_id = factor(searchess$site_id, levels=levels(EHall$site_id));
searchess$visitor_location_country_id = factor(searchess$visitor_location_country_id, levels=levels(EHall$visitor_location_country_id));
searchess$srch_destination_id = factor(searchess$srch_destination_id, levels=levels(EHall$srch_destination_id));

hotelss$prop_id = factor(hotelss$prop_id, levels=levels(EHall$prop_id));
hotelss$prop_country_id = factor(hotelss$prop_country_id, levels=levels(EHall$prop_country_id));

print("JC features")
print(allfeatsjc)
XSfeats = c("prop_location_score1", "prop_location_score2", "prop_log_historical_price", "price_usd", "promotion_flag", "orig_destination_distance"); 
hfeats = c("prop_country_id","prop_starrating","prop_review_score","prop_brand_bool","count_clicks","avg_bookings_usd","stdev_bookings_usd","count_bookings");

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

print("JCnor1 features sorted by MI")
sortedfeatsminor1 = setdiff(sortedfeatsmi, hfeats)
print(sortedfeatsminor1)
print("JCnor1 features sorted by IGR")
sortedfeatsigrnor1 = setdiff(sortedfeatsigr, hfeats)
print(sortedfeatsigrnor1)


print("Running Filter-MI for JC with 01")
pt = proc.time();
outset = myfilter(sortedfeatsmi, "position", EHtrain[,-1], EHtest[,-1], '01');
print(proc.time() - pt)
print("Finished Filter-MI for JC with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JC")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)
print("Running Filter-IGR for JC with 01")
pt = proc.time();
outset = myfilter(sortedfeatsigr, "position", EHtrain[,-1], EHtest[,-1], '01');
print(proc.time() - pt)
print("Finished Filter-IGR for JC with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JC")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

forb = c(FALSE,TRUE) #srch_id cannot be used as a feature
ldfs = list(hotelss)
ldfs = append(ldfs, list(searchess))
listFKs = c("prop_id", "srch_id")

print("Running Filter-MI for JC with dsorfs opt with 01")
pt = proc.time();
outset = myfilterwithfs(sortedfeatsmi,EHtrain, "position", XSfeats, listFKs, ldfs, forb, EHtest, '01')
print(proc.time() - pt)
print("Finished Filter-MI for JC with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JC with dsorfsopt")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)
print("Running Filter-IGR for JC with dsorfs opt with 01")
pt = proc.time();
outset = myfilterwithfs(sortedfeatsigr,EHtrain, "position", XSfeats, listFKs, ldfs, forb, EHtest, '01')
print(proc.time() - pt)
print("Finished Filter-IGR for JC with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JC with dsorfsopt")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running Filter-MI for JCnor1 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsminor1, "position", EHtrain[,-1], EHtest[,-1], '01');
print(proc.time() - pt)
print("Finished Filter-MI for JCnor1 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnor1")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)
print("Running Filter-IGR for JCnor1 with 01")
pt = proc.time();
outset = myfilter(sortedfeatsigrnor1, "position", EHtrain[,-1], EHtest[,-1], '01');
print(proc.time() - pt)
print("Finished Filter-IGR for JCnor1 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnor1")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

forb = c(TRUE) #srch_id cannot be used as a feature
ldfs = list(searchess)
listFKs = c("srch_id")

print("Running Filter-MI for JCnor1 with dsorfs opt with 01")
pt = proc.time();
outset = myfilterwithfs(sortedfeatsminor1, EHtrain, "position", union("prop_id", XSfeats), listFKs, ldfs, forb, EHtest, '01')
print(proc.time() - pt)
print("Finished Filter-MI for JCnor1 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JCnor1 with dsorfsopt")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)
print("Running Filter-IGR for JCnor1 with dsorfs opt with 01")
pt = proc.time();
outset = myfilterwithfs(sortedfeatsigrnor1, EHtrain, "position", union("prop_id", XSfeats), listFKs, ldfs, forb, EHtest, '01')
print(proc.time() - pt)
print("Finished Filter-IGR for JCnor1 with 01")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,"position"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,"position"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JCnor1 with dsorfsopt")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)
