getwd()
setwd("C:/Users/minga/Documents/CSE7331_Project3")
if(!file.exists("members_cleandata.rda")) {
  
  claims <- read.csv(file="Claims.csv",na.strings=c(""))
  members <- read.csv(file="Members.csv",na.strings=c(""))
  dihY3 <- read.csv(file="DaysInHospital_Y3.csv",na.strings=c(""))
  drugs<- read.csv(file="DrugCount.csv",na.strings=c(""))
  labs<-read.csv(file="LabCount.csv",na.strings=c(""))
  
  
  # filter Y1 data
  claimsY2 <- claims[claims$Year == "Y2",]
  drugsY2 <- drugs[drugs$Year == "Y2",]
  labsY2 <- labs[labs$Year == "Y2",]
  n_claims <- table(claimsY2$MemberID)
  membersY2 <- members
  membersY2 <- merge(membersY2, data.frame(MemberID=names(n_claims), 
                                           claims=as.numeric(n_claims)))
  
  # add median paydelay
  levels(claimsY2$PayDelay)[levels(claimsY2$PayDelay)=="162+"] <- 162
  claimsY2$PayDelay <- as.numeric(as.character(claimsY2$PayDelay))
  membersY2 <- merge(membersY2, aggregate(PayDelay~MemberID, 
                                          data=claimsY2, FUN=median))
  
  # add highest Index
  membersY2 <- merge(membersY2, aggregate(CharlsonIndex~MemberID, data=claimsY2, 
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$CharlsonIndex <- as.factor(membersY2$CharlsonIndex)
  
  # add DaysInHospital
  membersY2 <- merge(membersY2, dihY3)
  
  # make MemberID nominal
  membersY2$MemberID <- as.factor(membersY2$MemberID)
  
  # add a discretized version of days in hospital (TRUE/FALSE) 
  # to indicate if the member will stay in the hospital at all
  # next year.
  membersY2$InHospital <- as.factor(membersY2$DaysInHospital > 0)
  
  # fix age
  levels(membersY2$AgeAtFirstClaim) 
  age <- gsub("(\\d+).*", "\\1", levels(membersY2$AgeAtFirstClaim))
  age
  
  levels(membersY2$AgeAtFirstClaim) <- age
  membersY2$AgeAtFirstClaim <- as.numeric(as.character(membersY2$AgeAtFirstClaim))
  
  # fix Charlson Index
  ci <- gsub("(\\d+).*", "\\1", levels(membersY2$CharlsonIndex))
  ci
  
  levels(membersY2$CharlsonIndex) <- ci
  membersY2$CharlsonIndex <- as.numeric(as.character(membersY2$CharlsonIndex))
  
  # fix Sex
  membersY2$Sex[membersY2$Sex == ""] <- NA
  membersY2$Sex <- factor(membersY2$Sex)
  
  
  # Fix the DSFS of labY1
  membersY2 <- merge(membersY2, aggregate(DSFS~MemberID, data=drugsY2,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$DSFS <- as.factor(membersY2$CharlsonIndex)
  
  membersY2$DSFS <- gsub("0- 1 month","1",membersY2$DSFS)
  membersY2$DSFS <- gsub("1- 2 months","2",membersY2$DSFS) # converted to months
  membersY2$DSFS <- gsub("2- 3 months","3",membersY2$DSFS) 
  membersY2$DSFS <- gsub("3- 4 months","4",membersY2$DSFS) 
  membersY2$DSFS <- gsub("4- 5 months","5",membersY2$DSFS) 
  membersY2$DSFS <- gsub("5- 6 months","6",membersY2$DSFS)
  membersY2$DSFS <- gsub("6- 7 months","7",membersY2$DSFS)
  membersY2$DSFS <- gsub("7- 8 months","8",membersY2$DSFS)
  membersY2$DSFS <- gsub("8- 9 months","9",membersY2$DSFS)
  membersY2$DSFS <- gsub("9- 10 months","10",membersY2$DSFS)
  membersY2$DSFS <- gsub("10- 11 months","11",membersY2$DSFS)
  membersY2$DSFS <- gsub("11- 12 months","12",membersY2$DSFS )
  
  membersY2$DSFS <-as.numeric(as.character(membersY2$DSFS ))
  
  # Translate drug counts
  membersY2 <- merge(membersY2, aggregate(DrugCount~MemberID, data=drugsY2,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$DrugCount <- gsub("7+","7",membersY2$DrugCount)
  membersY2$DrugCount <- as.numeric(membersY2$DrugCount)
  
  # Translate Lab Counts
  membersY2 <- merge(membersY2, aggregate(LabCount~MemberID, data=labsY2,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  
  membersY2$LabCount <- gsub("10+","10",membersY2$LabCount)
  membersY2$LabCount <- as.numeric(membersY2$LabCount)
  # Fix the Specialty from claimsY1
  membersY2 <- merge(membersY2, aggregate(Specialty~MemberID, data=claimsY2 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$Specialty <- as.factor(membersY2$Specialty)
  # Fix the PlaceSvc from claimsY1
  membersY2 <- merge(membersY2, aggregate(PlaceSvc~MemberID, data=claimsY2 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$PlaceSvc <- as.factor(membersY2$PlaceSvc)
  # Fix the PrimaryConditionGroup from claimsY1
  membersY2 <- merge(membersY2, aggregate(PrimaryConditionGroup~MemberID, data=claimsY2 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$PrimaryConditionGroup <- as.factor(membersY2$PrimaryConditionGroup)
  # Fix the ProcedureGroupfrom claimsY1
  membersY2 <- merge(membersY2, aggregate(ProcedureGroup~MemberID, data=claimsY2 ,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$ProcedureGroup <- as.factor(membersY2$ProcedureGroup)
  # Save the clean data set so we do not have to do the preprocessing
  #  over and over again...
  save(membersY2, file="members_cleandata.rds")
  # Note: you can use write.csv to save it as a csv file
}
#-------------
# Read Dataset
#-------------
load('members_cleandata.rds')
str(membersY2)

summary(membersY2)
# Get rid of MemberID, as they are nominal and clean data

membersY2$MemberID<- NULL
#--------------------------
########### Missing Data
#-------------------------
summary(membersY2)

membersY2[membersY2=="?"] <- NA   # if presence nan it
length(which(is.na(membersY2)))  # how many NAs in the data
# how many samples would we loose ,if we removed them?
nrow(membersY2)
nrow(membersY2[is.na(membersY2),])
# Missing value are imputed with the mice package:
check_NA <- sapply(membersY2, function(x) any(is.na(x)))  # check any Nan value
check_NA
check_NA[check_NA]
library(mice)
# impute missing data
membersY2[,c(1,3:7,9:11)] <- apply(membersY2[, c(1,3:7,9:11)], 2, function(x) as.numeric(as.character(x)))
dataset_impute <- mice(membersY2[, c(1,3:7,9:11)],  print = FALSE)
membersY2 <- cbind(membersY2[, c(2,8,12:15), drop = FALSE], mice::complete(dataset_impute, 1))
membersY2<- na.omit(membersY2)
length(which(is.na(membersY2))) 
str(membersY2)
summary(membersY2)
# Importing necessary libraries:
library(tidyverse)
library(caret)
library(doParallel)
library(sampling)
library(rpart)
library(rpart.plot)
# let see some plot to make clear concepts
library(ggplot2)
ggplot(membersY2,aes(LabCount,DrugCount)) + geom_point(aes(color=InHospital))
# above shows inhospital with labcount and drugcounts,we can see three dot true value in chart
#----------
#barplot
#----------
ggplot(membersY2,aes(LabCount)) + geom_histogram(aes(fill=InHospital),color='black',bins=50,alpha=0.6)+theme_bw()
ggplot(membersY2,aes(DrugCount)) + geom_histogram(aes(fill=InHospital),color='black',bins=50,alpha=0.6)+theme_bw()
subset(membersY2,DaysInHospital>=15)
sum(membersY2$DaysInHospital>=15)

##Check for class imbalance:
# for DaysInHospital distributions
class_of<- group_by(membersY2,DaysInHospital)
imbalance_class<- summarize(class_of,count=n())
ggplot(imbalance_class,aes(x=DaysInHospital,y=count)) + geom_col()

# for better precise,categorization variable for length of rehospitalization

members_Y2<-membersY2
members_Y2<- mutate(members_Y2,hospitalization=ifelse(DaysInHospital %in% 1:5,"short stay",
                                                      ifelse( DaysInHospital %in% 6:15,"long stay","no stay")))


members_Y2<- mutate(members_Y2,DrugDone=ifelse(DrugCount %in% 3:4,"Medium DrugCount",
                                               ifelse(DrugCount %in% 5:6,"High DrugCount","Low DrugCount")))

members_Y2<- mutate(members_Y2,LabDone=ifelse(LabCount %in% 4:6,"Medium LabCount",
                                              ifelse(LabCount %in% 7:9,"High LabCount","Low LabCount")))
members_Y2$DrugDone<-as.factor(members_Y2$DrugDone)
members_Y2$LabDone<-as.factor(members_Y2$LabDone)
members_Y2$hospitalization<-as.factor(members_Y2$hospitalization)
# Imbalance of hospitalization:
classs_of<- group_by(members_Y2,hospitalization)
imbalance_classs<- summarize(classs_of,count=n())
ggplot(imbalance_classs,aes(x=hospitalization,y=count)) + geom_col()

levels(members_Y2$hospitalization)
## Features
library(tidyr)

gather(members_Y2, x, y, c(7:15)) %>%
  ggplot(aes(x = y, color = hospitalization, fill = hospitalization)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

library(FSelector)

weights<- chi.squared(hospitalization ~ .,data = members_Y2)
weights
str(weights)
ord<- order(weights$attr_importance)
dotchart(weights$attr_importance[ord],labels = rownames(weights)[ord],
         xlab = "Importance")

# above gives DaysInHospital ,InHospital are correlated so we can delete it in future
#other way to calculate univariate importance scores
oneR(hospitalization ~ .,data = members_Y2)
gain.ratio(hospitalization ~ .,data = members_Y2)
information.gain(hospitalization ~ .,data = members_Y2)

cfs(hospitalization ~ .,data = members_Y2)# it just shows one i.e calimsTruncated
consistency(hospitalization ~ .,data = members_Y2) #it gives
#[1] "AgeAtFirstClaim"       "Sex"                   "claims"               
#[4] "CharlsonIndex"         "ClaimsTruncated"       "DrugCount"            
#[7] "Specialty"             "PlaceSvc"              "PrimaryConditionGroup"
#[10] "ProcedureGroup"        "LabDone"   


# Before going let remove correlated files and unwanted feature
#members_Y22<- members_Y2  # copy
# there  i am removing the correlated value and unwanted attributes:
    
#members_Y2$LabCount<-NULL
#members_Y2$LabDone<- NULL
#members_Y2$ProcedureGroup<- NULL
#members_Y2$PayDelay<- NULL
members_Y2$InHospital<-NULL
members_Y2$DaysInHospital<-NULL        # removing DaysInHospital
#----------------------------------------------------
# Training Validation and test data
## Balance date : Class Imbalance
#Modeling the original unbalanced data
# split dataset training and testing:# creating datapartitin 70% and 30%
#----------------------------------------------------


index<- createDataPartition(members_Y2$hospitalization,p=0.7,list=FALSE)
train_set<- members_Y2[index,]
test_set<-members_Y2[-index,]
library(dplyr)


rbind(data.frame(group = "train", train_set),
      data.frame(group = "test", test_set)) %>%
  gather(x, y, 8:13) %>%
  ggplot(aes(x = y, color = group, fill = group)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)


library(corrplot)
#corMatMy<-cor(data_ds[,-7])
#corrplot(corMatMy,order = "hclust")

#---------------------------------
# Classification
#---------------------------------


# For Decision tree
library(caret)
registerDoParallel()
members_Y2_20000 <- members_Y2[sample(which(complete.cases(members_Y2)),20000),]
#member_Y1_20000$LabCount<-NULL
model <- train(hospitalization ~ AgeAtFirstClaim+claims+Sex+CharlsonIndex, 
               data = members_Y2_20000, method = "rpart",  
               tuneLength = 10,tuneGrid=data.frame(cp=0.0000001))

model 
model$finalModel
library(rpart.plot)
rpart.plot(model$finalModel, extra = 2, under = TRUE,  varlen=0, faclen=0)

#------------------------------------------
## Balance 
#-------------------------------------------
library(sampling)
# downsampling and upsampling:
table(members_Y2$hospitalization)   # 
data_ds<-downSample(train_set,train_set$hospitalization)
table(data_ds$hospitalization)   # all becomes 376
data_ds$Class <- NULL
data_us<- upSample(train_set,train_set$hospitalization)
table(data_us$hospitalization)    # all becomes 15804
data_us$Class<-NULL




folds <- createFolds(data_ds$hospitalization, k=10)
knn <- train(hospitalization ~ .,
             data=data_ds, method='knn', tuneGrid=data.frame(k=1:10),
             trControl=trainControl(method='cv', index = folds))
knn
plot(knn)
knn$finalModel
# Testing
hospital_knn<-predict(knn,newdata = test_set,na.action = na.pass)
table(hospital_knn)
confusionMatrix(data = hospital_knn,test_set$hospitalization)



rpartfit <- train(hospitalization ~ AgeAtFirstClaim+claims+CharlsonIndex+ClaimsTruncated+Sex,
                  data = data_ds , method = "rpart",
                  control=rpart.control(minsplit=2),
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength=10)
rpartfit  

plot(varImp(rpartfit))
rpartfit$finalModel
rpart.plot(rpartfit$finalModel, extra = 2,under=TRUE,varlen=0,faclen=0)
rpartfit$resample
rpartfit_train <- data_ds[-folds$Fold02,]
rpartfit_test <- data_ds[folds$Fold02,]
model_rpartfit <- train(hospitalization ~ ., data=rpartfit_train)
p_rpartfit <- predict(model_rpartfit, rpartfit_test, type='raw')
confusionMatrix(p_rpartfit, rpartfit_test$hospitalization)
# # Testing
hospital_rpart<-predict(rpartfit,newdata = test_set,na.action = na.pass)
table(hospital_rpart)
confusionMatrix(data = hospital_rpart,test_set$hospitalization)


resamps<-resamples(list(CART=rpartfit,kNeareestNeighbors=knn))
summary(resamps)
xyplot(resamps)
difs<- diff(resamps)   # to find one model better than other
difs
summary(difs)

knn$resample 
fold_train <- data_ds[-folds$Fold02,]
fold_test <- data_ds[folds$Fold02,]
#knn <- knn3(hospitalization ~ ., data=fold_train, k=1)
p <- predict(knn, fold_test, type='raw')
confusionMatrix(p, fold_test$hospitalization)

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
plot(randomforestfit)

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
plot(hospital_randomforest)


#one hot encoding features:
dmy<- dummyVars("~ .", data = data_ds,fullRank = T) # using fullRank=T to avoid dummy trap

 trsf<-data.frame(predict(dmy,newdata = data_ds))   # for female=1,male=0 for column Sex.M        
 hospitalizationn<-data_ds$hospitalization
 data_dss<-cbind(hospitalizationn,trsf)
 data_dss$hospitalization.no.stay<-NULL
 data_dss$hospitalization.short.stay<- NULL

##  xgboost
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
xgboostFit$finalModel$confusion
#feature importance
impp<- xgboostFit$finalModel$importance
impp[order(impp,decreasing = TRUE),]
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
    cm_model <- get(paste0("cm_", name))
  
  comparison[comparison$model == name, ] <- filter(comparison, model == name) %>%
    mutate(
          
           Precision = cm_model$byClass["Precision"],
           Recall = cm_model$byClass["Recall"],
           F1 = cm_model$byClass["F1"])
#}

library(tidyr)
comparison %>%
  gather(x, y, Sensitivity:F1) %>%
  ggplot(aes(x = x, y = y, color = model)) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 3)

### The END
# we can do ROC also.