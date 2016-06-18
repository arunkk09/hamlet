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

geterr <- function(fulltab, errormetric, nexamples, classes=5) {
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

MLtrain = read.csv("MLtrain.csv")
MLtest = read.csv("MLtest.csv")
MLtrts = rbind(MLtrain,MLtest)
MLhold = read.csv("MLhold.csv")
wtrts = read.csv("SCAMLtraintest.csv")
wth = read.csv("SCAMLhold.csv")
SCAMLRTS = sparseMatrix(wtrts$row, wtrts$col, x=wtrts$val)
SCAMLH = sparseMatrix(wth$row, wth$col, x=wth$val)
MLS = MLtrts[,c("rating")]
YH = MLhold[,c("rating")]

print("CV for log reg with L1 on SCAMLRTS")
pt = proc.time(); trtsl1cv = cv.glmnet(SCAMLRTS, MLS, family="multinomial", type.measure="class", standardize=FALSE, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl1cv)
print(trtsl1cv$cvm)
print(trtsl1cv$lambda)
print(trtsl1cv$lambda.min)
warnings()

print("Error of best CV L1 model on SCAMLRTS")
 PMLS = predict(trtsl1cv, SCAMLRTS, s="lambda.min")
PMLSmax = rep('0', length(MLS))
for (i in 1:length(MLS)) {
	PMLSmax[i] = names(which.max(PMLS[i,,]))
}
PMLSmax2 = factor(PMLSmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabRS = table(MLS,PMLSmax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(MLtrts),5)
print(acc)

print("Error of best CV L1 model on SCAMLH")
PYH = predict(trtsl1cv, SCAMLH, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(MLhold),5)
print(acc)

print("CV for log reg with L2 on SCAMLRTS")
pt = proc.time(); trtsl2cv = cv.glmnet(SCAMLRTS, MLS, family="multinomial", type.measure="class", standardize=FALSE, alpha=0, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl2cv)
print(trtsl2cv$cvm)
print(trtsl2cv$lambda)
print(trtsl2cv$lambda.min)
warnings()

print("Error of best CV L2 model on SCAMLRTS")
PMLS = predict(trtsl2cv, SCAMLRTS, s="lambda.min")
PMLSmax = rep('0', length(MLS))
for (i in 1:length(MLS)) {
	PMLSmax[i] = names(which.max(PMLS[i,,]))
}
PMLSmax2 = factor(PMLSmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabRS = table(MLS,PMLSmax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(MLtrts),5)
print(acc)

print("Error of best CV L2 model on SCAMLH")
PYH = predict(trtsl2cv, SCAMLH, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(MLhold),5)
print(acc)
