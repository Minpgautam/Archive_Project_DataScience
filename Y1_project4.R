getwd()
setwd("C:/Users/minga/Documents/CSE7331_Project4")

# Import the necessary library
library(tidyverse)
library(arules)
library(arulesViz)
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

#-----------------------------------------
# Read Dataset that is done in project 3
#-----------------------------------------
load('members_clean.rds')
str(membersY1)
head(membersY1)
summary(membersY1)
# Get rid of MemberID, as they are nominal and don't affect the observation

membersY1$MemberID<- NULL


#--------------------------
########### Missing Data
#-------------------------
summary(membersY1)
check_NA <- sapply(membersY1, function(x) any(is.na(x)))  # check any Nan value
check_NA
check_NA[check_NA]
membersY1[membersY1=="?"] <- NA   # if presence of any '?' make it nan
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
length(which(is.na(membersY1))) # this unknown sex column whichi is NA's.
#save the data for further use
save(list=ls(),file="../membersY1.rda")
# load the file
load(file="../membersY1.rda")
str(membersY1)
summary(membersY1)
head(membersY1)

#check the presence of numeric value
sapply(membersY1,is.numeric) # or use str syntax
sum(membersY1$DaysInHospital>=15) # more than 15 days hospitalization
# delete some attributes
#membersY1$LabCount<-NULL   # don't make null to read the data
# Let get rid of frequent unimportant items or attributes
membersY1$InHospital<-NULL  # removing as its is correlated with daysinhospital
membersY1$DSFS<- NULL
# Discretize the numeric attributes
membersY1$CharlsonIndex <- as.factor(membersY1$CharlsonIndex) 

membersY1$DaysInHospital <- discretize(membersY1$DaysInHospital, 
                                       method = "fixed", breaks = c(0,1,6,Inf),labels = c("nostay","shortstay","longstay"))
membersY1$DrugCount <- discretize(membersY1$DrugCount, 
                                     method = "fixed", breaks = c(1,3,5,6),labels = c("Low DrugCount","Medium DrugCount","High DrugCount"))
membersY1$LabCount <- discretize(membersY1$LabCount, 
                            method = "fixed", breaks = c(1,4,7,9),labels = c("Low LabCount","Medium LabCount","High LabCount"))
membersY1$AgeAtFirstClaim <- discretize(membersY1$AgeAtFirstClaim, 
                                 method = "fixed", breaks = c(0,30,60,80),labels = c("Young","Middle_aged","old"))
membersY1$ClaimsTruncated <- as.logical(membersY1$ClaimsTruncated)

# Discretize the rest of attribute
membersY1<-discretizeDF(membersY1)
summary(membersY1)
str(membersY1)

#---------------
# creating subsets including the cancer of male and female
#-------------------
male_Y1<-filter(membersY1,Sex=='M'|PrimaryConditionGroup=='CANCRA'|
                PrimaryConditionGroup=='CANCRB'|
                PrimaryConditionGroup=='GYNECA'|
                PrimaryConditionGroup=='CANCRM')

female_Y1<-filter(membersY1,Sex=='F'|PrimaryConditionGroup=='CANCRA'|
                  PrimaryConditionGroup=='CANCRB'|
                  PrimaryConditionGroup=='GYNECA'|
                  PrimaryConditionGroup=='CANCRM')
#delete the columns of sex of particular data that are not considered in same name
male_Y1$Sex<-NULL
female_Y1$Sex<- NULL
#----------------------------------------------------
#create the tranctions
#----------------------------------------------------
trans<-as(membersY1,"transactions")
trans_male_Y1<- as(male_Y1,"transactions")
trans_female_Y1<- as(female_Y1,"transactions")
summary(trans)
summary(trans_male_Y1)
summary(trans_female_Y1)
image(trans[1:300])
transactionInfo(trans[size(trans)>12])# checking long transaction
# checking the plot with support
plot(sort(itemFrequency(trans, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")
plot(sort(itemFrequency(trans_male_Y1, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")
plot(sort(itemFrequency(trans_female_Y1, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")
# getting rid of frequently unimportant items from ours dataset like inhospital and charlsonindex=0
# as we are interested in people that get hospitalization  based on other attributes
trans1 <- trans[,-grep("^InHospital=|CharlsonIndex=0|DaysInHospital=nostay", 
                       itemLabels(trans))]
trans_male_Y1<- trans_male_Y1[,-grep("^InHospital=|CharlsonIndex=0|DaysInHospital=nostay", 
                                     itemLabels(trans_male_Y1))]
trans_female_Y1<- trans_female_Y1[,-grep("^InHospital=|CharlsonIndex=0|DaysInHospital=nostay", 
                                         itemLabels(trans_female_Y1))]
itemFrequencyPlot(trans1, topN = 20)
itemFrequencyPlot(trans_male_Y1, topN = 20,support=0.1)
itemFrequencyPlot(trans_female_Y1, topN = 20)
itemLabels(trans1)
summary(trans1)
inspect(trans1[1:5]) # inspecting 5 transaction in the subset of trans
# plot the item frequency for items with a support greater than 10%
# also, for better readability of the labels,we reduce the label size with parameter cex.name
itemFrequencyPlot(trans1,support=0.1,cex.name=0.8)



#---------------------------
# Getting frequent itemsets
#---------------------------
freq_trans1<- apriori(trans1,parameter = list(target="frequent",
                                              support=0.01,
                                              minlen=4))
freq_male_cancer<- apriori(trans_male_Y1,parameter = list(target="frequent",
                                              support=0.01,
                                              minlen=4))
freq_female_cancer<- apriori(trans_female_Y1,parameter = list(target="frequent",
                                                          support=0.01,
                                                          minlen=4))
sort_df_trans1<-sort(freq_trans1,by="support")
inspect(head(sort_df_trans1))

sort_df_malecancer<-sort(freq_male_cancer,by="support")
inspect(head(sort_df_malecancer))

sort_df_femalecancer<-sort(freq_female_cancer,by="support")
inspect(head(sort_df_femalecancer))

#-------------------------
# Getting closed itemsets
#-------------------------
closed_male_cancer<- apriori(trans_male_Y1,parameter = list(target="closed",
                                                            support=0.01,
                                                            minlen=2))
closed_female_cancer<- apriori(trans_female_Y1,parameter = list(target="closed",
                                                            support=0.01,
                                                            minlen=2))
sort_closed_malecancer<-sort(closed_male_cancer,by="support")
inspect(head(sort_closed_malecancer))

sort_closed_femalecancer<-sort(closed_female_cancer,by="support")
inspect(head(sort_closed_femalecancer))
#-------------------------
# Getting maximal itemsets
#-------------------------
maximal_male_cancer<- apriori(trans_male_Y1,parameter = list(target="maximal",
                                                 support=0.01,
                                                 minlen=2))
maximal_female_cancer<- apriori(trans_female_Y1,parameter = list(target="maximal",
                                                             support=0.01,
                                                             minlen=2))
sort_maximal_malecancer<-sort(maximal_male_cancer,by="support")
inspect(head(sort_maximal_malecancer))

sort_maximal_femalecancer<-sort(maximal_female_cancer,by="support")
inspect(head(sort_maximal_femalecancer))

#---------------------------
# Association rules 
#---------------------------


100/nrow(trans1)   # 0.002710174

rules_Y1<- apriori(trans1,parameter = list(support = 0.001, confidence = 0.9,target="rules"))  # find all rules
rules_Y1<-sort(rules_Y1,by="lift")
#plot
plot(rules_Y1,control=list(jitter=.5))
summary(rules_Y1)
head(quality(rules_Y1))
# convet to rules to dataframe to see with rules
df_rules_Y1<- as(rules_Y1,"data.frame")
head(df_rules_Y1)


# For Male cancer
rules_male_cancer<- apriori(trans_male_Y1,parameter = list(support = 0.001, confidence = 0.9))  # find all rules
rules_male_cancer<-sort(rules_male_cancer,by="lift")
head(quality(rules_male_cancer))
summary(rules_male_cancer)
#plot
plot(rules_male_cancer,control=list(jitter=.5))
# convet to rules to dataframe
df_rules_male_cancer<- as(rules_male_cancer,"data.frame")
head(df_rules_male_cancer)

#For female cancer
rules_female_cancer<- apriori(trans_female_Y1,parameter = list(support = 0.001, confidence = 0.6))  # find all rules
rules_female_cancer<-sort(rules_female_cancer,by="lift")
head(quality(rules_female_cancer))
summary(rules_female_cancer)
#plot
plot(rules_female_cancer,control=list(jitter=.5))
# convet to rules to dataframe
df_rules_female_cancer<- as(rules_female_cancer,"data.frame")
head(df_rules_female_cancer)
## More plots:
plot(rules_Y1,measure=c("support","lift"),shading = "confidence",control=list(jitter=.5))
plot(rules_Y1,shading="order",control = list(main="Two-key plot"))
#plot(rules_Y1,method = "graph",control = list(type="items",cex=0.7,jitter=.5))


subrules = rules_Y1[quality(rules_Y1)$confidence > 0.8];

plot(subrules,method = "matrix",measure = "lift")
subrules2=head(sort(rules_Y1,by="lift"),20);
plot(subrules2,method="graph")
plot(subrules2,method="graph",control = list(type="items",cex=.6))
plot(subrules2,method="paracoord")
plot(subrules2,method="paracoord",control = list(reorder=TRUE))
oneRule=sample(rules_Y1,1)
inspect(oneRule)

# applying the k-mean clustering on Apriori Algorithm
library(ggplot2)
ggplot(df_rules_Y1,aes(x=df_rules_Y1$support,y=df_rules_Y1$confidence)) +
  geom_point(shape=1,size=2,color="black",alpha=1/3) +
  geom_point(size=6,color="red4",alpha=1/3) +
  labs(x="SUPPORT",y="CONFIDENCE")
