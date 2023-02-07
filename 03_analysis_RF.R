# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#Move RF analysis to function
#call function with different data sets - all, burned, unburned, logged, unlogged

#RF function
#Strata selection
# rf_hmlu: all burn severity classes: BurnSev %in% c('H','M','L','U')
# rf_hml: subset of classes: BurnSev %in% c('H','M','L')
# rf_binary: binary severity- (unburned + low) - (moderate or severe)
# rf_log: evaluate only logged='no' & binary
# rf_unlog: logged='ye' & binary 


data<-B_RFdata %>%
  mutate(binary=ifelse(BurnSev %in% c('H','M','L'),1,0)) %>%
  dplyr::filter(BurnSev %in% c('H','M','L','U'))
  #dplyr::filter(BurnSev %in% c('H','M','L'))
  #dplyr::filter(logged=='no')
  #dplyr::filter(logged=='ye')
  
str(data)

data$BurnSev <- as.factor(data$BurnSev)
data$binary <- as.factor(data$binary)
#table(data$BurnSev)
#H     L     M     U 
#27371 33893 36185 16870 
#table(data$binary)
#0     1 
#50763 63556 

set.seed(222)
#ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
#reduce size of trraining data set so doesnt error out - works with smaller data set
TrainProportion<-71435/114319 # using same number of cases for training as used in SAS run
ind <- sample(2, nrow(data), replace = TRUE, prob = c(TrainProportion, 1-TrainProportion))
train <- data[ind==1,]
test <- data[ind==2,]

BurnSevTable<-table(train$BurnSev)
BurnSevTable
sum(BurnSevTable)
#H     L     M     U 
#27371 33893 36185 16870 

#Variables used by Phil 
#BurnSev, logged, burned, STAND_AGE, WTED_PROJ_HT, CR_CLOSURE, TREE_PATTERN,
#VERT_COMPLEXITY, LIVE_STEMS, TOT_STEMS, PC_DEAD, 
#PC_broadleaf, SHRB_CC, NVEG_PCT, SITE_INDEX
#rf <- randomForest(BurnSev~., data=train, proximity=FALSE) #use all of the data
#rf_hmlu <- randomForest(BurnSev~ logged + STAND_AGE + WTED_PROJ_HT + CR_CLOSURE+ 
rf_3_1 <- randomForest(binary~ logged + STAND_AGE + WTED_PROJ_HT + CR_CLOSURE+ 
                     SHRB_CC+ SITE_INDEX+
                     PC_broadleaf + PC_DEAD + NVEG_PCT+
                     LIVE_STEMS+TOT_STEMS,
                   data=train, 
                   proximity=FALSE, #if TRUE and a large number of cases then fails due to an internal memory allocation issue with the underlying C code
                   importance=TRUE,
                   #na.action=na.omit,#missing values - omited, individually adjusted above
                   #replace=TRUE,#71.28%OOB if FALSE, %71.06 if TRUE
                   #maxnodes=30,
                   #RF works best with balance strata
                   #strata=train$logged,
                   #sampsize=c(10000,10000,10000,10000),#increases OOB% to 73.86
                   mtry=2, ntree=5000) #if ntree is to high then rf fails due to memory issue

saveRDS(rf,file='tmp/rf_3_1')
rf_3_1<-readRDS(file='tmp/rf_3_1')

print(rf_3_1)
varImpPlot(rf_3_1)
#https://stats.stackexchange.com/questions/463152/random-forest-variable-importance-plot-interpretation
#Ok so the first plot does not reflect % drop in accuracy but rather, the mean change in accuracy 
# scaled by its standard deviation. This is where the change in accuracy is stored, unscaled, note the MeanDecreaseAccuracy is the average of columns 1 and 2
rf_3_1$importance
#When you scale it by SD, you get the numbers you see in the plot:
rf_3_1$importance[,1:3]/rf_3_1$importanceSD[,1:3]

par(mfrow=c(1,2))
boxplot(SITE_INDEX~binary,data=train)
boxplot(WTED_PROJ_HT~binary,data=train)
boxplot(TOT_STEMS~binary,data=train)
boxplot(LIVE_STEMS~binary,data=train)
boxplot(PC_DEAD~binary,data=train)

boxplot(PC_broadleaf~binary,data=train)
boxplot(STAND_AGE~binary,data=train)


###############

#Tune RF run find best mtry value
rf <-randomForest(BurnSev~., data=train, 
                  #sampsize=c(10000,10000,10000,10000),
                  mtry=2,
                  ntree=500) 
varImpPlot(rf)

#rf <-randomForest(Creditability~.,data=mydata, ntree=500) 
#mtry <- tuneRF(mydata[-1],mydata$Creditability, ntreeTry=500,
#               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
mtry <- tuneRF(train[-1],train$BurnSev, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m) #mtry = 3  OOB error = 54.95%


#Need to figure out standard error so can compare to SAS run...
p1 <- predict(rf, train)
confusionMatrix(p1, train$ BurnSev)

p2 <- predict(rf, test)
confusionMatrix(p2, test$ BurnSev)

### classification
#rf <- randomForest(BurnSev~., data=train, proximity=FALSE) #use all of the data
lapply(train, class)

train2<-train %>%
  mutate(LOGGED=if_else(logged=='ye',1,0)) %>%
  mutate(LOGGED=if_else(logged=='ye',1,0)) %>%
  dplyr::select(BurnSev, STAND_AGE, WTED_PROJ_HT, CR_CLOSURE,
                PC_DEAD,
                PC_broadleaf, NVEG_PCT)

irisct <- ctree(BurnSev ~ .,data = train2)
irisct
plot(irisct)
table(predict(irisct), train2$BurnSev)


### estimated class probabilities, a list
tr <- predict(irisct, newdata = train2[1:10,], type = "prob")

lm_habitat_raster <- predict(lm_highrez_brick,
                             lm_randomForest)





#Phil SAS code
if FireYear > 0;  * exclude 1 km buffer;
;;;;
ods graphics on;
;;;;
proc hpsplit maxdepth=5 assignmissing=similar nodes splitonce cvmodelfit minleafsize=30 seed=71435
plots=zoomedtree(nodes=('1' '2') depth=3)
plots=zoomedtree(nodes=('7' '8' '9' 'A' 'B' 'C' 'D' 'E') depth=3);
*     plots=zoomedtree(nodes=('F' 'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N' 'O' 'P' 'Q' 'R' 'S' 'T' 'U') depth=3);

class BurnSev logged;
model BurnSev (event='H' event='L' event='M' event='U')= LOGGED STAND_AGE WTED_PROJ_HT CR_CLOSURE
TREE_PATTERN VERT_COMPLEXITY LIVE_STEMS TOT_STEMS PC_DEAD PC_BROADLEAF SHRB_CC NVEG_PCT SITE_INDEX;
grow entropy;
prune reducederror;
title1 'CART analysis of classifying burned shapes into 4 BARC classes using VRI predictors’;
   title2 ‘depth=5, grow entropy, prune reducederror';
run;
quit;
 