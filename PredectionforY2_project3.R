getwd()
setwd("C:/Users/minga/Documents/CSE7331_Project3")
if(!file.exists("members_clean.rda")) {
  
  claims <- read.csv(file="Claims.csv",,na.strings=c(""))
  members <- read.csv(file="Members.csv",na.strings=c(""))
  dihY2 <- read.csv(file="DaysInHospital_Y2.csv",na.strings=c(""))
  drugs<- read.csv(file="DrugCount.csv",na.strings=c(""))
  labs<-read.csv(file="LabCount.csv",na.strings=c(""))
  
  
  # filter Y1 data
  claimsY1 <- claims[claims$Year == "Y1",]
  drugsY1 <- drugs[drugs$Year == "Y1",]
  labsY1 <- labs[labs$Year == "Y1",]
  n_claims <- table(claimsY1$MemberID)
  membersY1 <- members
  membersY1 <- merge(membersY1, data.frame(MemberID=names(n_claims), 
                                           claims=as.numeric(n_claims)))
  
  # add median paydelay
  levels(claimsY1$PayDelay)[levels(claimsY1$PayDelay)=="162+"] <- 162
  claimsY1$PayDelay <- as.numeric(as.character(claimsY1$PayDelay))
  membersY1 <- merge(membersY1, aggregate(PayDelay~MemberID, 
                                          data=claimsY1, FUN=median))
  
  # add highest Index
  membersY1 <- merge(membersY1, aggregate(CharlsonIndex~MemberID, data=claimsY1, 
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$CharlsonIndex <- as.factor(membersY1$CharlsonIndex)
  
  # add DaysInHospital
  membersY1 <- merge(membersY1, dihY2)
  
  # make MemberID nominal
  membersY1$MemberID <- as.factor(membersY1$MemberID)
  
  # add a discretized version of days in hospital (TRUE/FALSE) 
  # to indicate if the member will stay in the hospital at all
  # next year.
  membersY1$InHospital <- as.factor(membersY1$DaysInHospital > 0)
  
  # fix age
  levels(membersY1$AgeAtFirstClaim) 
  age <- gsub("(\\d+).*", "\\1", levels(membersY1$AgeAtFirstClaim))
  age
  
  levels(membersY1$AgeAtFirstClaim) <- age
  membersY1$AgeAtFirstClaim <- as.numeric(as.character(membersY1$AgeAtFirstClaim))
  
  # fix Charlson Index
  ci <- gsub("(\\d+).*", "\\1", levels(membersY1$CharlsonIndex))
  ci
  
  levels(membersY1$CharlsonIndex) <- ci
  membersY1$CharlsonIndex <- as.numeric(as.character(membersY1$CharlsonIndex))
  
  # fix Sex
  membersY1$Sex[membersY1$Sex == ""] <- NA
  membersY1$Sex <- factor(membersY1$Sex)
  
  head(membersY1)
  summary(membersY1)
  # Fix the DSFS of labY1
  membersY1 <- merge(membersY1, aggregate(DSFS~MemberID, data=drugsY1,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$DSFS <- as.factor(membersY1$DSFS)
  
  membersY1$DSFS <- gsub("0- 1 month","1",membersY1$DSFS)
  membersY1$DSFS <- gsub("1- 2 months","2",membersY1$DSFS) # converted to months
  membersY1$DSFS <- gsub("2- 3 months","3",membersY1$DSFS) 
  membersY1$DSFS <- gsub("3- 4 months","4",membersY1$DSFS) 
  membersY1$DSFS <- gsub("4- 5 months","5",membersY1$DSFS) 
  membersY1$DSFS <- gsub("5- 6 months","6",membersY1$DSFS)
  membersY1$DSFS <- gsub("6- 7 months","7",membersY1$DSFS)
  membersY1$DSFS <- gsub("7- 8 months","8",membersY1$DSFS)
  membersY1$DSFS <- gsub("8- 9 months","9",membersY1$DSFS)
  membersY1$DSFS <- gsub("9- 10 months","10",membersY1$DSFS)
  membersY1$DSFS <- gsub("10- 11 months","11",membersY1$DSFS)
  membersY1$DSFS <- gsub("11- 12 months","12",membersY1$DSFS )
  
  membersY1$DSFS <-as.numeric(as.character(membersY1$DSFS ))
  
  # Translate drug counts
  membersY1 <- merge(membersY1, aggregate(DrugCount~MemberID, data=drugsY1,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$DrugCount <- gsub("7+","7",membersY1$DrugCount)
  membersY1$DrugCount <- as.numeric(membersY1$DrugCount)
  
  # Translate Lab Counts
  membersY1 <- merge(membersY1, aggregate(LabCount~MemberID, data=labsY1,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  
  membersY1$LabCount <- gsub("10+","10",membersY1$LabCount)
  membersY1$LabCount <- as.numeric(membersY1$LabCount)
  # Fix the Specialty from claimsY1
  membersY1 <- merge(membersY1, aggregate(Specialty~MemberID, data=claimsY1 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$Specialty <- as.factor(membersY1$Specialty)
  # Fix the PlaceSvc from claimsY1
  membersY1 <- merge(membersY1, aggregate(PlaceSvc~MemberID, data=claimsY1 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$PlaceSvc <- as.factor(membersY1$PlaceSvc)
  # Fix the PrimaryConditionGroup from claimsY1
  membersY1 <- merge(membersY1, aggregate(PrimaryConditionGroup~MemberID, data=claimsY1 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$PrimaryConditionGroup <- as.factor(membersY1$PrimaryConditionGroup)
  # Fix the ProcedureGroupfrom claimsY1
  membersY1 <- merge(membersY1, aggregate(ProcedureGroup~MemberID, data=claimsY1 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$ProcedureGroup <- as.factor(membersY1$ProcedureGroup)
  # Save the clean data set so we do not have to do the preprocessing
  #  over and over again...
  save(membersY1, file="members_clean.rds")
  # Note: you can use write.csv to save it as a csv file
}
#-------------
# Read Dataset
#-------------
load('members_clean.rds')
str(membersY1)

summary(membersY1)
# Get rid of MemberID, as they are nominal and clean data

membersY1$MemberID<- NULL
#--------------------------
########### Missing Data
#-------------------------
summary(membersY1)
check_NA <- sapply(membersY1, function(x) any(is.na(x)))  # check any Nan value
check_NA
check_NA[check_NA]
membersY1[membersY1=="?"] <- NA   # if presence nan it
length(which(is.na(membersY1)))  # how many NAs in the data
# how many samples would we loose ,if we removed them?
nrow(membersY1)
nrow(membersY1[is.na(membersY1),])
# Missing value are imputed with the mice package:
library(mice)
# impute missing data
membersY1[,c(1,3:7,9:11)] <- apply(membersY1[, c(1,3:7,9:11)], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(membersY1[, c(1,3:7,9:11)],  print = FALSE)
membersY1 <- cbind(membersY1[, c(2,8,12:15), drop = FALSE], mice::complete(dataset_impute, 1))
membersY1<- na.omit(membersY1)
length(which(is.na(membersY1))) 
str(membersY1)
summary(membersY1)
# Importing necessary libraries:
library(tidyverse)
library(caret)
library(doParallel)
library(sampling)
library(ROSE)
library(rpart)
library(rpart.plot)
# let see some plot to make clear concepts
library(ggplot2)
ggplot(membersY1,aes(LabCount,DrugCount)) + geom_point(aes(color=InHospital))
 # above shows inhospital with labcount and drugcounts,we can see three dot true value in chart
#----------
#barplot
#----------
ggplot(membersY1,aes(LabCount)) + geom_histogram(aes(fill=InHospital),color='black',bins=50,alpha=0.6)+theme_bw()
ggplot(membersY1,aes(DrugCount)) + geom_histogram(aes(fill=InHospital),color='black',bins=50,alpha=0.6)+theme_bw()
subset(membersY1,DaysInHospital>=15)
sum(membersY1$DaysInHospital>=15)

##Check for class imbalance:
      # for DaysInHospital distributions
class_of<- group_by(membersY1,DaysInHospital)
imbalance_class<- summarize(class_of,count=n())
ggplot(imbalance_class,aes(x=DaysInHospital,y=count)) + geom_col()

# for better precise,categorization variable for length of rehospitalization

members_Y1<-membersY1
members_Y1<- mutate(members_Y1,hospitalization=ifelse(DaysInHospital %in% 1:5,"short stay",
                                                    ifelse( DaysInHospital %in% 6:15,"long stay","no stay")))
                                                    

members_Y1<- mutate(members_Y1,DrugDone=ifelse(DrugCount %in% 3:4,"Medium DrugCount",
                                                    ifelse(DrugCount %in% 5:6,"High DrugCount","Low DrugCount")))
                                                           
members_Y1<- mutate(members_Y1,LabDone=ifelse(LabCount %in% 4:6,"Medium LabCount",
                                             ifelse(LabCount %in% 7:9,"High LabCount","Low LabCount")))
members_Y1$DrugDone<-as.factor(members_Y1$DrugDone)
members_Y1$LabDone<-as.factor(members_Y1$LabDone)
members_Y1$hospitalization<-as.factor(members_Y1$hospitalization)
# Imbalance of hospitalization:
classs_of<- group_by(members_Y1,hospitalization)
imbalance_classs<- summarize(classs_of,count=n())
ggplot(imbalance_classs,aes(x=hospitalization,y=count)) + geom_col()
# there  i am removing the correlated value:
#member_Y1$LabCount<-NULL
#member_Y1$DrugCount<-NULL
#members_Y1$InHospital<-NULL
#members_Y1<- members_Y1[-7]   # removing DaysInHospital
str(members_Y1)
summary(members_Y1)
levels(members_Y1$hospitalization)
## Features
library(tidyr)

gather(members_Y1, x, y, c(7:15)) %>%
  ggplot(aes(x = y, color = hospitalization, fill = hospitalization)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

library(FSelector)

weights<- chi.squared(hospitalization ~ .,data = members_Y1)
str(weights)
ord<- order(weights$attr_importance)
dotchart(weights$attr_importance[ord],labels = rownames(weights)[ord],
         xlab = "Importance")

# above gives DaysInHospital ,InHospital are correlated so we can delete it in future
#other way to calculate univariate importance scores
oneR(hospitalization ~ .,data = members_Y1)
gain.ratio(hospitalization ~ .,data = members_Y1)
information.gain(hospitalization ~ .,data = members_Y1)

cfs(hospitalization ~ .,data = members_Y1)# it just shows one i.e calimsTruncated
consistency(hospitalization ~ .,data = members_Y1) #it gives
#[1] "AgeAtFirstClaim"       "Sex"                   "claims"               
#[4] "CharlsonIndex"         "ClaimsTruncated"       "DrugCount"            
#[7] "Specialty"             "PlaceSvc"              "PrimaryConditionGroup"
#[10] "ProcedureGroup"        "LabDone"   
#----------------------------------------------------
# Training Validation and test data
## Balance date : Class Imbalance
#Modeling the original unbalanced data
# split dataset training and testing:# creating datapartitin 70% and 30%
#----------------------------------------------------
members_Y1$InHospital<-NULL
members_Y1$DaysInHospital<-NULL    # removing DaysInHospital

index<- createDataPartition(members_Y1$hospitalization,p=0.7,list=FALSE)
train_set<- members_Y1[index,]
test_set<-members_Y1[-index,]

library(dplyr)


rbind(data.frame(group = "train", train_set),
      data.frame(group = "test", test_set)) %>%
  gather(x, y, 8:13) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

#---------------------------------
# Classification
#---------------------------------
# there  i am removing the correlated value:
# there  i am removing the correlated value and unwanted attributes:
#members_Y1$LabCount<-NULL
#members_Y1$LabDone<- NULL
#members_Y1$ProcedureGroup<- NULL
#members_Y1$PayDelay<- NULL


# I was trying to check robustness but all gave same valuess.
#data_ds<-downSample(members_Y1,members_Y1$hospitalization)















# For Decision tree
library(caret)
registerDoParallel()
members_Y1_20000 <- members_Y1[sample(which(complete.cases(members_Y1)),10000),]
#member_Y1_20000$LabCount<-NULL
model <- train(hospitalization ~ AgeAtFirstClaim+claims+Sex+CharlsonIndex, 
               data = members_Y1_20000, method = "rpart",  
               tuneLength = 10,tuneGrid=data.frame(cp=0.0000001))

model  
model$finalModel
library(rpart.plot)
rpart.plot(model$finalModel, extra = 2, under = TRUE,  varlen=0, faclen=0)


#------------------------------------------
## Balance 
#-------------------------------------------
library(sampling)
# downsampling and upsampling

table(members_Y1$hospitalization)   # 
data_ds<-downSample(train_set,train_set$hospitalization) # downsampling
table(data_ds$hospitalization)   # all becomes 770
data_ds$Class <- NULL
data_us<- upSample(train_set,train_set$hospitalization)   # upsampling
table(data_us$hospitalization)    # all becomes 21000
data_us$Class<-NULL

 

folds <- createFolds(data_ds$hospitalization, k=10)
# for knn
knn <- train(hospitalization ~ .,
               data=data_ds, method='knn', tuneGrid=data.frame(k=1:10),
               trControl=trainControl(method='cv', index = folds))
knn
plot(knn)
plot(varImp(knn))
knn$finalModel

# Testing
hospital_knn<-predict(knn,newdata = test_set,na.action = na.pass)
table(hospital_knn)
confusionMatrix(data = hospital_knn,test_set$hospitalization)

# for decison tree
#rpartfit <- train(hospitalization ~ AgeAtFirstClaim+claims+CharlsonIndex+ClaimsTruncated+PrimaryConditionGroup+DrugDone+Specialty,
      #            data = data_ds , method = "rpart",
       #           control=rpart.control(minsplit=2),
        #          trControl = trainControl(method = "cv", number = 10),
         #         tuneLength=10)

rpartfit <- train(hospitalization ~ AgeAtFirstClaim+claims+CharlsonIndex+ClaimsTruncated+sex,
                  data = data_ds , method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=10)
 rpartfit
 plot(rpartfit)

 plot(varImp((rpartfit)))  # it gives messy plot if we consider more attributes
 rpart.plot(rpartfit$finalModel, extra = 2,under=TRUE,varlen=0,faclen=0)
 rpartfit$results
 
 rpartfit$resample
 varImp(rpartfit)
 rpartfit_train <- data_ds[-folds$Fold02,]
 rpartfit_test <- data_ds[folds$Fold02,]
 model_rpartfit <- train(hospitalization ~ ., data=rpartfit_train)
 p_rpartfit <- predict(model_rpartfit, rpartfit_test, type='raw')
 plot(p_rpartfit)
 confusionMatrix(p_rpartfit, rpartfit_test$hospitalization)
 # Testing of decison tree.
 hospital_rpart<-predict(rpartfit,newdata = test_set,na.action = na.pass)
 table(hospital_rpart)
 confusionMatrix(data = hospital_rpart,test_set$hospitalization)
 # compare of cart and knn
resamps<-resamples(list(CART=rpartfit,kNeareestNeighbors=knn))
summary(resamps)
xyplot(resamps)
difs<- diff(resamps)   # to find one model better than other
difs
summary(difs)

knn$resample 
 fold_train <- data_ds[-folds$Fold02,]
 fold_test <- data_ds[folds$Fold02,]
   #knn <- knn3(hospitalization ~ ., data=fold_train, k=1:10)
 p <- predict(knn, fold_test, type='raw')
confusionMatrix(p, fold_test$hospitalization)

# For Random Forest:

randomforestfit<- train(hospitalization ~ .,data=data_ds,method="rf",
                        tuneLength=10,
                        trControl=trainControl(method="cv"))
                                               
randomforestfit
randomforestfit$finalModel
randomforestfit$finalModel$confusion
#feature importance
imp<- randomforestfit$finalModel$importance
imp[order(imp,decreasing = TRUE),]
#estimate variable importance
importance<- varImp(randomforestfit,scale = TRUE)
plot(importance)
# plot is messsy but the importance is given as 
#PayDelay                                claims 
#269.58368773                          187.79161723 
#AgeAtFirstClaim                              LabCount 
#116.47855864                           95.68154748 
#DrugCount                                  DSFS 
#69.51950234                           42.63543175 
#ClaimsTruncated                         CharlsonIndex 
#36.79137778                           34.53031163 
#Sex.F          PrimaryConditionGroup.MSC2a3 
#32.07330061                           31.95816807 
#PrimaryConditionGroup.METAB3                                 Sex.M 
#30.42339266                           29.95797452 
#Specialty.Internal        PrimaryConditionGroup.ARTHSPIN 
#29.46531868                           28.80407478 
#`Specialty.General Practice`                     ProcedureGroup.EM 
#26.26522834                           22.81904919 
#PlaceSvc.Office             `LabDone.Medium LabCount` 
#21.30189816                           18.96047907 
#ProcedureGroup.PL         PrimaryConditionGroup.GIBLEED 
#18.03440082                           17.00153958 
#`DrugDone.Medium DrugCount`                  Specialty.Laboratory 
#16.89516997                           16.88032116 
#

randomforestfit$resample 
random_train <- data_ds[-folds$Fold02,]
random_test <- data_ds[folds$Fold02,]
model_rf <- train(hospitalization ~ ., data=random_train)
p <- predict(model_rf, random_test, type='raw')
confusionMatrix(p, random_test$hospitalization)

# Testing
hospital_randomforest<-predict(randomforestfit,newdata = test_set,na.action = na.pass)
table(hospital_randomforest)
confusionMatrix(data = hospital_randomforest,test_set$hospitalization)

#one hot encoding features:
dmy<- dummyVars("~ .", data = data_ds,fullRank = T) # using fullRank=T to avoid dummy trap

trsf<-data.frame(predict(dmy,newdata = data_ds))   # for female=1,male=0 for column Sex.M        
hospitalizationn<-data_ds$hospitalization
data_dss<-cbind(hospitalizationn,trsf)
data_dss$hospitalization.no.stay<-NULL
data_dss$hospitalization.short.stay<- NULL

## XGBOOST

xgboostFit <- train(hospitalizationn ~ ., method = "xgbTree",data = data_dss ,
                    tuneLength = 10,
                    trControl = trainControl(
                      method = "cv", index = folds),
                    tuneGrid = expand.grid(
                      nrounds = 20,
                      max_depth = 3,
                      colsample_bytree = .6,
                      eta = 0.1,
                      gamma=0,
                      min_child_weight = 1,
                      subsample = .5
                    ))

xgboostFit

xgboostFit$finalModel
#feature importance
impp<- xgboostFit$finalModel$importance

#estimate variable importance
importances<- varImp(xgboostFit,scale = TRUE)
plot(importances)
xgboostFit$resample 
xgboost_train <- data_ds[-folds$Fold02,]
xgboost_test <- data_ds[folds$Fold02,]
model_xgboost <- train(hospitalization ~ ., data=xgboost_train)
p <- predict(model_xgboost, xgboost_test, type='raw')
confusionMatrix(p, xgboost_test$hospitalization)

# Testing
#  for test set

dummy_test<- dummyVars("~ .", data = test_set,fullRank = T) # using fullRank=T to avoid dummy trap

trsff<-data.frame(predict(dummy_test,newdata = test_set))   # for female=1,male=0 for column Sex.M        
hospitalizationn<-test_set$hospitalization
data_dsss<-cbind(hospitalizationn,trsff)
data_dsss$hospitalization.no.stay<-NULL
data_dsss$hospitalization.short.stay<- NULL

hospital_xgboost<-predict(xgboostFit,newdata = data_dsss,na.action = na.pass)
table(hospital_xgboost)
confusionMatrix(data = hospital_xgboost,data_dsss$hospitalizationn)

# Now compare models
models<- list(
  ctree = rpartfit,
  KNN=knn,
  randomForest=randomforestfit,
  xgboost=xgboostFit)

resampss<-resamples(models)
resampss
summary(resampss)
difs<-diff(resampss)
summary(difs)
plot(resampss)
bwplot(resampss)
library(dplyr)
comparison <- data.frame(model = names(models),
                         Sensitivity = rep(NA, length(models)),
                         Specificity = rep(NA, length(models)),
                         Precision = rep(NA, length(models)),
                         Recall = rep(NA, length(models)),
                         F1 = rep(NA, length(models)))


#for (name in names(models)) {
#cm_model <- get(paste0("cm_", name))

#comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
#  mutate(
    
#    Precision = cm_model$byClass["Precision"],
#    Recall = cm_model$byClass["Recall"],
 #   F1 = cm_model$byClass["F1"])
#}

library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)


# we can do ROC also.
### The END