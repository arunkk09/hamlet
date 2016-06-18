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

YRtrain = read.csv("YRtrain.csv")
YRtest = read.csv("YRtest.csv")
YRtrts = rbind(YRtrain,YRtest)
YRhold = read.csv("YRhold.csv")
wtrts = read.csv("SJCYRtraintest.csv")
wth = read.csv("SJCYRhold.csv")
SJCYRRTS = sparseMatrix(wtrts$row, wtrts$col, x=wtrts$val)
SJCYRH = sparseMatrix(wth$row, wth$col, x=wth$val)
YRS = YRtrts[,c("stars")]
YH = YRhold[,c("stars")]

print("CV for log reg with L1 on SJCYRRTS")
pt = proc.time(); trtsl1cv = cv.glmnet(SJCYRRTS, YRS, family="multinomial", type.measure="class", standardize=FALSE, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl1cv)
print(trtsl1cv$cvm)
print(trtsl1cv$lambda)
print(trtsl1cv$lambda.min)
warnings()

print("Error of best CV L1 model on SJCYRRTS")
 PYRS = predict(trtsl1cv, SJCYRRTS, s="lambda.min")
PYRSmax = rep('0', length(YRS))
for (i in 1:length(YRS)) {
	PYRSmax[i] = names(which.max(PYRS[i,,]))
}
PYRSmax2 = factor(PYRSmax,levels=c("1","2","3","4","5"))
ftabRS = table(YRS,PYRSmax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(YRtrts),5)
print(acc)

print("Error of best CV L1 model on SJCYRH")
PYH = predict(trtsl1cv, SJCYRH, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("1","2","3","4","5"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(YRhold),5)
print(acc)

print("CV for log reg with L2 on SJCYRRTS")
pt = proc.time(); trtsl2cv = cv.glmnet(SJCYRRTS, YRS, family="multinomial", type.measure="class", standardize=FALSE, alpha=0, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl2cv)
print(trtsl2cv$cvm)
print(trtsl2cv$lambda)
print(trtsl2cv$lambda.min)
warnings()

print("Error of best CV L2 model on SJCYRRTS")
PYRS = predict(trtsl2cv, SJCYRRTS, s="lambda.min")
PYRSmax = rep('0', length(YRS))
for (i in 1:length(YRS)) {
	PYRSmax[i] = names(which.max(PYRS[i,,]))
}
PYRSmax2 = factor(PYRSmax,levels=c("1","2","3","4","5"))
ftabRS = table(YRS,PYRSmax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(YRtrts),5)
print(acc)

print("Error of best CV L2 model on SJCYRH")
PYH = predict(trtsl2cv, SJCYRH, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("1","2","3","4","5"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(YRhold),5)
print(acc)
