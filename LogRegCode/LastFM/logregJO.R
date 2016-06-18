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

LFsub2train = read.csv("LFsub2train.csv")
LFsub2test = read.csv("LFsub2test.csv")
LFsub2trts = rbind(LFsub2train,LFsub2test)
LFsub2hold = read.csv("LFsub2hold.csv")
wtrts = read.csv("SJCnoartistLFsub2traintest.csv")
wth = read.csv("SJCnoartistLFsub2hold.csv")
SJCnoartistLFsub2RTS = sparseMatrix(wtrts$row, wtrts$col, x=wtrts$val)
SJCnoartistLFsub2H = sparseMatrix(wth$row, wth$col, x=wth$val)
LFsub2S = LFsub2trts[,c("plays")]
YH = LFsub2hold[,c("plays")]

print("CV for log reg with L1 on SJCnoartistLFsub2RTS")
pt = proc.time(); trtsl1cv = cv.glmnet(SJCnoartistLFsub2RTS, LFsub2S, family="multinomial", type.measure="class", standardize=FALSE, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl1cv)
print(trtsl1cv$cvm)
print(trtsl1cv$lambda)
print(trtsl1cv$lambda.min)
warnings()

print("Error of best CV L1 model on SJCnoartistLFsub2RTS")
 PLFsub2S = predict(trtsl1cv, SJCnoartistLFsub2RTS, s="lambda.min")
PLFsub2Smax = rep('0', length(LFsub2S))
for (i in 1:length(LFsub2S)) {
	PLFsub2Smax[i] = names(which.max(PLFsub2S[i,,]))
}
PLFsub2Smax2 = factor(PLFsub2Smax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabRS = table(LFsub2S,PLFsub2Smax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(LFsub2trts),5)
print(acc)

print("Error of best CV L1 model on SJCnoartistLFsub2H")
PYH = predict(trtsl1cv, SJCnoartistLFsub2H, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(LFsub2hold),5)
print(acc)

print("CV for log reg with L2 on SJCnoartistLFsub2RTS")
pt = proc.time(); trtsl2cv = cv.glmnet(SJCnoartistLFsub2RTS, LFsub2S, family="multinomial", type.measure="class", standardize=FALSE, alpha=0, nlambda=100, maxit=10000, thresh=0.001); print(proc.time() - pt)
print(trtsl2cv)
print(trtsl2cv$cvm)
print(trtsl2cv$lambda)
print(trtsl2cv$lambda.min)
warnings()

print("Error of best CV L2 model on SJCnoartistLFsub2RTS")
PLFsub2S = predict(trtsl2cv, SJCnoartistLFsub2RTS, s="lambda.min")
PLFsub2Smax = rep('0', length(LFsub2S))
for (i in 1:length(LFsub2S)) {
	PLFsub2Smax[i] = names(which.max(PLFsub2S[i,,]))
}
PLFsub2Smax2 = factor(PLFsub2Smax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabRS = table(LFsub2S,PLFsub2Smax2)
print(ftabRS)
acc=geterr(ftabRS,'RMSE',nrow(LFsub2trts),5)
print(acc)

print("Error of best CV L2 model on SJCnoartistLFsub2H")
PYH = predict(trtsl2cv, SJCnoartistLFsub2H, s="lambda.min")
PYHmax = rep('0', length(YH))
for (i in 1:length(YH)) {
	PYHmax[i] = names(which.max(PYH[i,,]))
}
PYHmax2 = factor(PYHmax,levels=c("'1'","'2'","'3'","'4'","'5'"))
ftabH = table(YH,PYHmax2)
print(ftabH)
acc=geterr(ftabH,'RMSE',nrow(LFsub2hold),5)
print(acc)
