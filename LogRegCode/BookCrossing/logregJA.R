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

BCtrain = read.csv("BCtrain.csv")
BCtest = read.csv("BCtest.csv")
BCtrts = rbind(BCtrain,BCtest)
BChold = read.csv("BChold.csv")
wtrts = read.csv("SJCBCtraintest.csv")
wth = read.csv("SJCBChold.csv")
SJCBCRTS = sparseMatrix(wtrts$row, wtrts$col, x=wtrts$val)
SJCBCH = sparseMatrix(wth$row, wth$col, x=wth$val)
BCS = BCtrts[,c("rating")]
YH = BChold[,c("rating")]

print("CV for log reg with L1 on SJCBCRTS")
pt = proc.time(); trtsl1cv = cv.glmnet(SJCBCRTS, BCS, family="multinomial", type.measure="class", standardize=FALSE, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl1cv)
print(trtsl1cv$cvm)
print(trtsl1cv$lambda)
print(trtsl1cv$lambda.min)
warnings()

print("Error of best CV L1 model on SJCBCRTS")
 PBCS = predict(trtsl1cv, SJCBCRTS, s="lambda.min")
PBCSmax = rep('0', length(BCS))
for (i in 1:length(BCS)) {
	PBCSmax[i] = names(which.max(PBCS[i,,]))
}
PBCSmax2 = factor(PBCSmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabRS = table(BCS,PBCSmax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(BCtrts),5)
print(acc)

print("Error of best CV L1 model on SJCBCH")
PYH = predict(trtsl1cv, SJCBCH, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(BChold),5)
print(acc)

print("CV for log reg with L2 on SJCBCRTS")
pt = proc.time(); trtsl2cv = cv.glmnet(SJCBCRTS, BCS, family="multinomial", type.measure="class", standardize=FALSE, alpha=0, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl2cv)
print(trtsl2cv$cvm)
print(trtsl2cv$lambda)
print(trtsl2cv$lambda.min)
warnings()

print("Error of best CV L2 model on SJCBCRTS")
PBCS = predict(trtsl2cv, SJCBCRTS, s="lambda.min")
PBCSmax = rep('0', length(BCS))
for (i in 1:length(BCS)) {
	PBCSmax[i] = names(which.max(PBCS[i,,]))
}
PBCSmax2 = factor(PBCSmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabRS = table(BCS,PBCSmax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(BCtrts),5)
print(acc)

print("Error of best CV L2 model on SJCBCH")
PYH = predict(trtsl2cv, SJCBCH, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(BChold),5)
print(acc)
