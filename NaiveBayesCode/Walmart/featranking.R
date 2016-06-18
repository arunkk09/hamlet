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

WTtrain=read.csv("WTtrain.csv");
WTtest=read.csv("WTtest.csv");
WThold=read.csv("WThold.csv");
features = read.csv("features_disc.csv");
stores = read.csv("stores_disc.csv");
WTtrain=WTtrain[,-1] #get rid of sid
WTtest=WTtest[,-1]
WThold=WThold[,-1]
WTfull=rbind(WTtrain,WTtest,WThold);

allfeatsjc = names(WThold);

WTtrain$dept = factor(WTtrain$dept, levels=levels(WTfull$dept));
WTtrain$purchaseid = factor(WTtrain$purchaseid, levels=levels(WTfull$purchaseid));
WTtrain$store = factor(WTtrain$store, levels=levels(WTfull$store));

WTtest$dept = factor(WTtest$dept, levels=levels(WTfull$dept));
WTtest$purchaseid = factor(WTtest$purchaseid, levels=levels(WTfull$purchaseid));
WTtest$store = factor(WTtest$store, levels=levels(WTfull$store));

WThold$dept = factor(WThold$dept, levels=levels(WTfull$dept));
WThold$purchaseid = factor(WThold$purchaseid, levels=levels(WTfull$purchaseid));
WThold$store = factor(WThold$store, levels=levels(WTfull$store));

stores$store = factor(stores$store, levels=levels(WTfull$store));
stores$type = factor(stores$type, levels=levels(WTfull$type));
stores$size = factor(stores$size, levels=levels(WTfull$size));

features$purchaseid = factor(features$purchaseid, levels=levels(WTfull$purchaseid));
features$temperature_avg = factor(features$temperature_avg, levels=levels(WTfull$temperature_avg));
features$temperature_stdev = factor(features$temperature_stdev, levels=levels(WTfull$temperature_stdev));
features$fuel_price_avg = factor(features$fuel_price_avg, levels=levels(WTfull$fuel_price_avg));
features$fuel_price_stdev = factor(features$fuel_price_stdev, levels=levels(WTfull$fuel_price_stdev ));
features$cpi_avg = factor(features$cpi_avg, levels=levels(WTfull$cpi_avg));
features$cpi_stdev = factor(features$cpi_stdev, levels=levels(WTfull$cpi_stdev));
features$unemployment_avg = factor(features$unemployment_avg, levels=levels(WTfull$unemployment_avg));
features$unemployment_stdev = factor(features$unemployment_stdev, levels=levels(WTfull$unemployment_stdev));
features$holidayfreq = factor(features$holidayfreq, levels=levels(WTfull$holidayfreq));

print("JC features")
print(allfeatsjc)
XSfeats = c("dept");

print("Ranking JC features by Mutual Information on WTtrain")
pt = proc.time(); wtinfogain = information.gain(weekly_sales ~. , WTtrain); proc.time() - pt
wtinfogain = wtinfogain/log(2.0); #since it uses e as default for log
print(t(wtinfogain))

#entropy(tabulate(WTtrain$ddst),unit="log2") #outputs correct entropy
print("Entropy of JC features on WTtrain")
tabent <- function(y) {
	return(entropy(tabulate(y),unit="log2"))
}
pt = proc.time(); wtentropies = sapply(WTtrain[,-1], tabent); proc.time() - pt
print(wtentropies)

print("Information Gain Ratio of JC features on WTtrain")
wtigrs = wtentropies
pt = proc.time(); 
for(i in 1:length(wtigrs)) {
	wtigrs[i] = 1.0*wtinfogain[i,]/wtentropies[i]
}
proc.time() - pt
print(wtigrs)

sortedfeatsmi = row.names(wtinfogain)[order(wtinfogain)];
sortedfeatsigr = names(sort(wtigrs));

sortedfeatsmica = intersect(sortedfeatsmi, c("dept", "store", "purchaseid"))
sortedfeatsigrca = intersect(sortedfeatsigr, c("dept", "store", "purchaseid"))

if(FALSE) {
print("JC features sorted by MI")
print(sortedfeatsmi)
print("JC features sorted by IGR")
print(sortedfeatsigr)

print("Running Filter-MI for JC on 7 classes")
pt = proc.time();
outset = myfilter(sortedfeatsmi, "weekly_sales", WTtrain, WTtest, 'RMSE', 7);
print(proc.time() - pt)
print("Finished Filter-MI for JC on 7 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,"weekly_sales"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,"weekly_sales"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JC")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JC on 7 classes")
pt = proc.time();
outset = myfilter(sortedfeatsigr, "weekly_sales", WTtrain, WTtest, 'RMSE', 7);
print(proc.time() - pt)
print("Finished Filter-IGR for JC on 7 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,"weekly_sales"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,"weekly_sales"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JC")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

forb = c(FALSE,FALSE)
ldfs = list(stores)
ldfs = append(ldfs, list(features))
listFKs = c("store", "purchaseid")

print("Running Filter-MI for JC with dsorfs opt on 7 classes")
pt = proc.time();
outset = myfilterwithfs(sortedfeatsmi, WTtrain, "weekly_sales", c(), listFKs, ldfs, forb, WTtest, 'RMSE', 7)
print(proc.time() - pt)
print("Finished Filter-MI for JC on 7 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,"weekly_sales"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,"weekly_sales"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for JC with dsorfsopt")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for JC with dsorfs opt on 7 classes")
pt = proc.time();
outset = myfilterwithfs(sortedfeatsigr, WTtrain, "weekly_sales", c(), listFKs, ldfs, forb, WTtest, 'RMSE', 7)
print(proc.time() - pt)
print("Finished Filter-IGR for JC on 7 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,"weekly_sales"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,"weekly_sales"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for JC with dsorfsopt")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)
}

print("CA features sorted by MI")
print(sortedfeatsmica)
print("CA features sorted by IGR")
print(sortedfeatsigrca)

print("Running Filter-MI for CA on 7 classes")
pt = proc.time();
outset = myfilter(sortedfeatsmica, "weekly_sales", WTtrain, WTtest, 'RMSE', 7);
print(proc.time() - pt)
print("Finished Filter-MI for CA on 7 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,"weekly_sales"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,"weekly_sales"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-MI for CA")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)
print("Running Filter-IGR for CA on 7 classes")
pt = proc.time();
outset = myfilter(sortedfeatsigrca, "weekly_sales", WTtrain, WTtest, 'RMSE', 7);
print(proc.time() - pt)
print("Finished Filter-IGR for CA on 7 classes")
print("Hold out validation")
pt = proc.time(); outsettr = myNBlog(WTtrain[,outset], WTtrain[,"weekly_sales"], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, WThold[,outset]), WThold[,"weekly_sales"], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of Filter-IGR for CA")
acc = geterr(outsettab, 'RMSE', nrow(WThold), nrow(outsettab))
print(acc)

