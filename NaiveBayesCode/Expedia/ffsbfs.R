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
allfeatsca = c("prop_id", "prop_location_score1", "prop_location_score2", "prop_log_historical_price", "price_usd", "promotion_flag", "orig_destination_distance");
fsearches = c("year","month","weekofyear","time","site_id","visitor_location_country_id","srch_destination_id","srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count","srch_room_count","srch_saturday_night_bool","random_bool");
allfeatsjcnosearches = setdiff(allfeatsjc, fsearches)
hfeats = c("prop_country_id","prop_starrating","prop_review_score","prop_brand_bool","count_clicks","avg_bookings_usd","stdev_bookings_usd","count_bookings");
allfeatsjcnohotels = setdiff(allfeatsjc, hfeats);

options(width=190)
print("CA features")
print(allfeatsca)
print("JCnosearches features")
print(allfeatsjcnosearches)
print("JCnohotels features")
print(allfeatsjcnohotels)
print("JC features")
print(allfeatsjc)

print("Running FFS for CA with 01")
pt = proc.time();
outset = myffs(allfeatsca, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished FFS for CA with 01")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for CA")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running FFS for JCnosearches with 01")
pt = proc.time();
outset = myffs(allfeatsjcnosearches, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnosearches with 01")
print("Hold out validation for JCnosearches")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnosearches")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running FFS for JCnohotels with 01")
pt = proc.time();
outset = myffs(allfeatsjcnohotels, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished FFS for JCnohotels with 01")
print("Hold out validation for JCnohotels")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JCnohotels")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running FFS for JC with 01")
pt = proc.time();
outset = myffs(allfeatsjc, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished FFS for JC with 01")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of FFS for JC")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)



print("Running BFS for CA with 01")
pt = proc.time();
outset = mybfs(allfeatsca, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished BFS for CA with 01")
print("Hold out validation for CA")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for CA")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running BFS for JCnosearches with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnosearches, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnosearches with 01")
print("Hold out validation for JCnosearches")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnosearches")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running BFS for JCnohotels with 01")
pt = proc.time();
outset = mybfs(allfeatsjcnohotels, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished BFS for JCnohotels with 01")
print("Hold out validation for JCnohotels")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JCnohotels")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)

print("Running BFS for JC with 01")
pt = proc.time();
outset = mybfs(allfeatsjc, 2, EHtrain, EHtest, '01');
print(proc.time() - pt)
print("Finished BFS for JC with 01")
print("Hold out validation for JC")
pt = proc.time(); outsettr = myNBlog(EHtrain[,outset], EHtrain[,2], laplace=1); print(proc.time() - pt)
pt = proc.time(); outsettab = table(predict.myNBlog(outsettr, EHhold[,outset]), EHhold[,2], dnn=list('predicted','actual')); print(proc.time() - pt)
print("Holdout validation of outset of BFS for JC")
acc = geterr(outsettab, '01', nrow(EHhold))
print(acc)
