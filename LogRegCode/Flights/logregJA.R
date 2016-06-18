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

geterr <- function(fulltab, errormetric, nexamples, classes=7) {
        fullacc = -1000;
        if(errormetric == 'RMSE') {
                        w = fulltab + t(fulltab);
                        fsum = 0;
                        for(c in 1:(classes - 1)) {
                                er = c*c;
                                for(l in 1:(classes - c)) {
                                        fsum = fsum + w[l, (l + c)] * er
                                }
                        }
                        fullacc = sqrt(fsum/nexamples)
            fullacc = -fullacc; #rmse sign is inverted to ensure the max is selected
        }
        else if(errormetric == '01') {
            fullacc = sum(diag(fulltab))/nexamples;
        }
        else {
            print ("Unrecognized error metric:")
                        print(errormetric)
        }
        return (fullacc);
}

library(Matrix)
library(glmnet)

options(width=190)

OFtrain = read.csv("OFtrain.csv")
OFtest = read.csv("OFtest.csv")
OFtrts = rbind(OFtrain,OFtest)
OFhold = read.csv("OFhold.csv")
otrts = read.csv("SJCOFtraintest.csv")
oth = read.csv("SJCOFhold.csv")
SJCOFRTS = sparseMatrix(otrts$row, otrts$col, x=otrts$val)
SJCOFH = sparseMatrix(oth$row, oth$col, x=oth$val)
ORS = OFtrts[,c("codeshare")]
OH = OFhold[,c("codeshare")]

print("CV for JC for log reg with L1 on SJCOFRTS")
pt = proc.time(); trtsl1cv = cv.glmnet(SJCOFRTS, ORS, family="multinomial", type.measure="class", standardize=FALSE, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl1cv)
print(trtsl1cv$cvm)
print(trtsl1cv$lambda)
print(trtsl1cv$lambda.min)
warnings()

print("Error of best CV for JC L1 model on SJCOFRTS")
 PORS = predict(trtsl1cv, SJCOFRTS, s="lambda.min")
PORSmax = rep('0', length(ORS))
for (i in 1:length(ORS)) {
	PORSmax[i] = names(which.max(PORS[i,,]))
}
PORSmax2 = factor(PORSmax,levels=c("f", "t"))
ftabRS = table(ORS,PORSmax2)
print(ftabRS)
acc=geterr(ftabRS,'01',nrow(OFtrts))
print(acc)

print("Error of best CV for JC L1 model on SJCOFH")
POH = predict(trtsl1cv, SJCOFH, s="lambda.min")
POHmax = rep('0', length(OH))
for (i in 1:length(OH)) {
	POHmax[i] = names(which.max(POH[i,,]))
}
POHmax2 = factor(POHmax,levels=c("f", "t"))
ftabH = table(OH,POHmax2)
print(ftabH)
acc=geterr(ftabH,'01',nrow(OFhold))
print(acc)

print("CV for JC for log reg with L2 on SJCOFRTS")
pt = proc.time(); trtsl2cv = cv.glmnet(SJCOFRTS, ORS, family="multinomial", type.measure="class", standardize=FALSE, alpha=0, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl2cv)
print(trtsl2cv$cvm)
print(trtsl2cv$lambda)
print(trtsl2cv$lambda.min)
warnings()

print("Error of best CV for JC L2 model on SJCOFRTS")
PORS = predict(trtsl2cv, SJCOFRTS, s="lambda.min")
PORSmax = rep('0', length(ORS))
for (i in 1:length(ORS)) {
	PORSmax[i] = names(which.max(PORS[i,,]))
}
PORSmax2 = factor(PORSmax,levels=c("f", "t"))
ftabRS = table(ORS,PORSmax2)
print(ftabRS)
acc=geterr(ftabRS,'01',nrow(OFtrts))
print(acc)

print("Error of best CV for JC L2 model on SJCOFH")
POH = predict(trtsl2cv, SJCOFH, s="lambda.min")
POHmax = rep('0', length(OH))
for (i in 1:length(OH)) {
	POHmax[i] = names(which.max(POH[i,,]))
}
POHmax2 = factor(POHmax,levels=c("f", "t"))
ftabH = table(OH,POHmax2)
print(ftabH)
acc=geterr(ftabH,'01',nrow(OFhold))
print(acc)
