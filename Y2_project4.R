getwd()
setwd("C:/Users/minga/Documents/CSE7331_Project4")
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
# Import the necessary library
library(tidyverse)
library(arules)
library(arulesViz)
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
#save the data for further use
save(list=ls(),file="../membersY2.rda")
# load the file
load(file="../membersY2.rda")


# Let get rid of frequent unimportant items or attributes
membersY2$InHospital<-NULL  # removing as its is correlated with daysinhospital
membersY2$DSFS<- NULL
# Discretize the numeric attributes
membersY2$CharlsonIndex <- as.factor(membersY2$CharlsonIndex) 

membersY2$DaysInHospital <- discretize(membersY2$DaysInHospital, 
                                       method = "fixed", breaks = c(0,1,6,Inf),labels = c("nostay","shortstay","longstay"))
membersY2$DrugCount <- discretize(membersY2$DrugCount, 
                                  method = "fixed", breaks = c(1,3,5,6),labels = c("Low DrugCount","Medium DrugCount","High DrugCount"))
membersY2$LabCount <- discretize(membersY2$LabCount, 
                                 method = "fixed", breaks = c(1,4,7,9),labels = c("Low LabCount","Medium LabCount","High LabCount"))
membersY2$AgeAtFirstClaim <- discretize(membersY2$AgeAtFirstClaim, 
                                        method = "fixed", breaks = c(0,30,60,80),labels = c("Young","Middle_aged","old"))
membersY2$ClaimsTruncated <- as.logical(membersY2$ClaimsTruncated)

# Discretize the rest of attribute
membersY2<-discretizeDF(membersY2)
summary(membersY2)
str(membersY2)

#---------------
# creating subsets including the cancer of male and female
#-------------------
male_Y2<-filter(membersY2,Sex=='M'|PrimaryConditionGroup=='CANCRA'|
                  PrimaryConditionGroup=='CANCRB'|
                  PrimaryConditionGroup=='GYNECA'|
                  PrimaryConditionGroup=='CANCRM')

female_Y2<-filter(membersY2,Sex=='F'|PrimaryConditionGroup=='CANCRA'|
                    PrimaryConditionGroup=='CANCRB'|
                    PrimaryConditionGroup=='GYNECA'|
                    PrimaryConditionGroup=='CANCRM')
#delete the columns of sex of particular data that are not considered in same name
male_Y2$Sex<-NULL
female_Y2$Sex<- NULL
#----------------------------------------------------
#create the tranctions
#----------------------------------------------------
trans<-as(membersY2,"transactions")
trans_male_Y2<- as(male_Y2,"transactions")
trans_female_Y2<- as(female_Y2,"transactions")
summary(trans)
summary(trans_male_Y2)
summary(trans_female_Y2)
image(trans[1:300])
transactionInfo(trans[size(trans)>12])# checking long transaction
# checking the plot with support
plot(sort(itemFrequency(trans, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")
plot(sort(itemFrequency(trans_male_Y2, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")
plot(sort(itemFrequency(trans_female_Y2, type="absolute"), decreasing=TRUE),
     xlab = "Items", ylab="Support Count", type="l")
# getting rid of frequently unimportant items from ours dataset like inhospital and charlsonindex=0
# as we are interested in people that get hospitalization  based on other attributes
trans1 <- trans[,-grep("^InHospital=|CharlsonIndex=0|DaysInHospital=nostay", 
                       itemLabels(trans))]
trans_male_Y2<- trans_male_Y2[,-grep("^InHospital=|CharlsonIndex=0|DaysInHospital=nostay", 
                       itemLabels(trans_male_Y2))]
trans_female_Y2<- trans_female_Y2[,-grep("^InHospital=|CharlsonIndex=0|DaysInHospital=nostay", 
                                      itemLabels(trans_female_Y2))]
itemFrequencyPlot(trans1, topN = 20)
itemFrequencyPlot(trans_male_Y2, topN = 20)
itemFrequencyPlot(trans_female_Y2, topN = 20)
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
freq_male_cancer<- apriori(trans_male_Y2,parameter = list(target="frequent",
                                                          support=0.01,
                                                          minlen=4))
freq_female_cancer<- apriori(trans_female_Y2,parameter = list(target="frequent",
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
closed_male_cancer<- apriori(trans_male_Y2,parameter = list(target="closed",
                                                            support=0.01,
                                                            minlen=2))
closed_female_cancer<- apriori(trans_female_Y2,parameter = list(target="closed",
                                                                support=0.01,
                                                                minlen=2))
sort_closed_malecancer<-sort(closed_male_cancer,by="support")
inspect(head(sort_closed_malecancer))

sort_closed_femalecancer<-sort(closed_female_cancer,by="support")
inspect(head(sort_closed_femalecancer))
#-------------------------
# Getting maximal itemsets
#-------------------------
maximal_male_cancer<- apriori(trans_male_Y2,parameter = list(target="maximal",
                                                             support=0.01,
                                                             minlen=2))
maximal_female_cancer<- apriori(trans_female_Y2,parameter = list(target="maximal",
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

rules_Y2<- apriori(trans1,parameter = list(support = 0.001, confidence = 0.9,target="rules"))  # find all rules
rules_Y2<-sort(rules_Y2,by="lift")
#plot
plot(rules_Y2,control=list(jitter=.5))
summary(rules_Y2)
head(quality(rules_Y2))
# convet to rules to dataframe to see with rules
df_rules_Y2<- as(rules_Y2,"data.frame")
head(df_rules_Y2)


# For Male cancer
rules_male_cancer<- apriori(trans_male_Y2,parameter = list(support = 0.001, confidence = 0.9))  # find all rules
rules_male_cancer<-sort(rules_male_cancer,by="lift")
head(quality(rules_male_cancer))
summary(rules_male_cancer)
#plot
plot(rules_male_cancer,control=list(jitter=.5))
# convet to rules to dataframe
df_rules_male_cancer<- as(rules_male_cancer,"data.frame")
head(df_rules_male_cancer)

#For female cancer
rules_female_cancer<- apriori(trans_female_Y2,parameter = list(support = 0.001, confidence = 0.9))  # find all rules
rules_female_cancer<-sort(rules_female_cancer,by="lift")
head(quality(rules_female_cancer))
summary(rules_female_cancer)
#plot
plot(rules_female_cancer,control=list(jitter=.5))
# convet to rules to dataframe
df_rules_female_cancer<- as(rules_female_cancer,"data.frame")
head(df_rules_female_cancer)
## More plots:
plot(rules_Y2,measure=c("support","lift"),shading = "confidence",control=list(jitter=.5))
plot(rules_Y2,shading="order",control = list(main="Two-key plot"))
#plot(rules_Y1,method = "graph",control = list(type="items",cex=0.7,jitter=.5))
#plot(rules_Y2,interactive=TRUE)
#plot(rules_Y2,method="grouped")
plot(rules_Y2,method="grouped",control = list(k=13))
#sel=plot(rules_Y2,method="grouped",interactive = TRUE)
subrules = rules_Y2[quality(rules_Y2)$confidence > 0.8];

plot(subrules,method = "matrix",measure = "lift")
subrules2=head(sort(rules_Y2,by="lift"),20);
#plot(subrules2,method="grouped",control = list(k=5))
plot(subrules2,method="graph")
plot(subrules2,method="graph",control = list(type="items",cex=.7))
plot(subrules2,method="paracoord")
plot(subrules2,method="paracoord",control = list(reorder=TRUE))
oneRule=sample(rules_Y2,1)
inspect(oneRule)

# plot for male and female cancer members
plot(rules_male_cancer,shading="order",control = list(main="Two-key plot")) #male
plot(rules_female_cancer,shading="order",control = list(main="Two-key plot"))


# applying the k-mean clustering on Apriori Algorithm
library(ggplot2)
ggplot(df_rules_Y2,aes(x=df_rules_Y2$support,y=df_rules_Y2$confidence)) +
  geom_point(shape=1,size=2,color="black",alpha=1/3) +
  geom_point(size=6,color="red4",alpha=1/3) +
  labs(x="SUPPORT",y="CONFIDENCE")

cluster_numbers=kmeans(df_rules_Y2,centers = 6,iter.max = 10)
df_rules_Y2$Cluster<- as.factor(cluster_numbers$cluster)




