
getwd()
setwd("C:/Users/minga/Documents/EE7331_Project2")

#-----------------------------------------
  # clean an prepare data for clustering:
#-----------------------------------------
if(!file.exists("membersY1.rda")) {
  
  # Read the file:
  claims<- read.csv(file="Claims.csv",na.strings=c(""))
  members<- read.csv(file="Members.csv",na.strings=c(""))
  drugs<- read.csv(file="DrugCount.csv",na.strings=c(""))
  labs<-read.csv(file="LabCount.csv",na.strings=c(""))
  
  # Filter the Y1 data:
  claimsY1 <- claims[claims$Year == "Y1",]
  drugsY1 <- drugs[drugs$Year == "Y1",]
  labsY1 <- labs[labs$Year == "Y1",]
  membersY1 <- members
  
  
  # add number of claims
  n_claims <- table(claimsY1$MemberID)
  
  membersY1 <- merge(membersY1, data.frame(MemberID=names(n_claims),
                                           claims=as.numeric(n_claims)))
  
  # add median paydelay
  levels(claimsY1$PayDelay)[levels(claimsY1$PayDelay)=="162+"] <- 162
  claimsY1$PayDelay <- as.numeric(as.character(claimsY1$PayDelay))
  membersY1 <- merge(membersY1, aggregate(PayDelay~MemberID,
                                          data=claimsY1, FUN=median))
  # add highest Charlson index
  membersY1 <- merge(membersY1, aggregate(CharlsonIndex~MemberID, data=claimsY1,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY1$CharlsonIndex <- as.factor(membersY1$CharlsonIndex)
  
  membersY1$MemberID <- factor(membersY1$MemberID)
  
  summary(membersY1)
  
  # translate age
  levels(membersY1$AgeAtFirstClaim) 
  age <- gsub("(\\d+).*", "\\1", levels(membersY1$AgeAtFirstClaim))
  age
  
  levels(membersY1$AgeAtFirstClaim) <- age
  membersY1$AgeAtFirstClaim <- as.numeric(as.character(membersY1$AgeAtFirstClaim))
  
  # translate Charlson Index
  levels(membersY1$CharlsonIndex)
  levels(membersY1$CharlsonIndex) <- c(0, 1.5, 3.5, 5)
  membersY1$CharlsonIndex <- as.numeric(as.character(membersY1$CharlsonIndex))
  # Translate DSFS of labsY1
  
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
  
  summary(membersY1)
  # taking the mean of data of certain columns
  for(i in 4:ncol(membersY1)){
    membersY1[is.na(membersY1[,i]), i] <- mean(membersY1[,i], na.rm = TRUE)
  }
  
  # save data for later
 save(membersY1, file = "membersY1.rda")
}

# load the save data:
load("membersY1.rda")
str(membersY1)
summary(membersY1)

#-----------------------------------
    # For Clustering interms of only Female
#-----------------------------------
library(dplyr)
membersY11<-filter(membersY1,Sex=="F")
# female member
# membersY11<-filter(membersY1,Sex=="M")    for male portion (do after female part)
# Let's do some clustering
membersY1_1<- membersY11[c(2,4,5,6,7,8,9)]
# Since if 35% of data is missing then it is better to drop it. Here ther are so many
# values 2716 are missing in ageatfirstclaims. It is better to drop it.
membersY1_2 <- membersY1_1[complete.cases(membersY1_1),]
# membersY1_2<- na.omit(membersY1_1)

# Let check with the boxplot 
boxplot(membersY1_2,main=" Year Y1")
boxplot(membersY1_2[c(3)])
quantile(membersY1_2$PayDelay,c(0,0.005,0.01,0.25,0.5,0.9,0.995,1)) # it seems it doesn't
# effect so we don't  make any change.
# we probably should scale the data and make dataframe to it.
membersY11_3<- data.frame(scale(membersY1_2))
# confirm it:
colMeans(membersY11_3)
apply(membersY11_3,2,sd)  # 2=columns wise sd.

# Let the cluster with k=4
#plot(membersY1_3)
km <- kmeans(membersY11_3,centers = 5)

# look at the centroid
def.par <- par(no.readonly = TRUE,mar=c(10,4,1,1)) # save default
layout(t(1:4))   # 4 plots in one
for(i in 1:4) plot(1:20)
for (i in 1:4) barplot(km$centers[i,],main=paste("Cluster",i), las=2)

par(def.par)

# use k=10 assume Euclidean distance
km <- kmeans(membersY11_3,centers = 5,nstart = 10)
km_min <- kmeans(membersY11_3[1:5000,],centers = 5)
km_min
# plot
plot(membersY11_3[1:5000,],col=km$cluster)
points(km_min$centers,pch=3,cex=2)  # this adds the centroids
text(km_min$centers,labels=1:4,pos = 2)  # this adds the cluster ID






# Let plot first 1000 points only
pairs(membersY11_3[1:1000,], col=km$cluster[1:1000])

# Run variable clustering procedure
#install.packages("Clustofvar")
#  to check which variabe are important
library(ClustOfVar)
var_tree<- hclustvar(membersY11_3)
plot(var_tree)
#--------------------------
  # Deciding about variables:
#--------------------------

# decide number of clusters
stability(var_tree)
str(membersY1_3)








#-----------------------------
   # K-Mean Visualization:
#-----------------------------
library(GGally)
ggparcoord(cbind(data.frame(km$centers),data.frame(id= as.character(1:5))),columns=1:ncol(km$centers),groupColumn='id',title = "Year Y1") +
  theme(axis.text.x = element_text(angle = 90))
library(cluster)
clusplot(membersY11_3[1:5000,], km_min$cluster)



#scale the variables

# our data is ready for clustering
# For hierarchical clustering,we'll first calculate a distance matrix 
# based on Eucledean measure.
#Hierarchical Clustering
scaled_wd<- data.frame(scale( membersY11_3))
d <- dist(scaled_wd[1:5000,], method = "euclidean")
#distance matrix
h_clust <- hclust(d, method = "ward.D") #clustering
#dendrogram
library(ClustOfVar)
plot(h_clust)
rect.hclust(h_clust,k=5)
var_tree<- hclustvar(scaled_wd)
stability(var_tree)   # gives plot stability is maximum

# 
#extract clusters
groups <- cutree(h_clust,k=5) 
groups
# Principal compoenenent help taken


library(fpc)
pcmp <- princomp(scaled_wd)
pred_pc <- predict(pcmp, newdata=scaled_wd)[,1:2]

#create a data frame having pc values and their corresponding clusters
library(data.table)
comp_dt <- cbind(as.data.table(pred_pc),cluster = as.factor(groups))
library(ggplot2)
ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = cluster),size=5)

membersY111_3 <- data.frame(membersY11_3,comp_dt)
   # visualize the cluster n data
table(membersY111_3$cluster)
plot(membersY111_3$AgeAtFirstClaim,membersY111_3$claims,pch=21,bg=c("red","green3","blue","yellow","brown")
     [unclass(membersY111_3$cluster)],main="cluster of Age & claims")
plot(membersY111_3$AgeAtFirstClaim,membersY111_3$PayDelay,pch=21,bg=c("red","green3","blue","yellow","brown")
     [unclass(membersY111_3$cluster)],main="cluster of Age & PayDelay")
# above plot didn't gave the meaning so don't consider it.
library(scatterplot3d)
sc <- membersY111_3
sc$pcolor[sc$cluster==1]<-"red"
sc$pcolor[sc$cluster==2]<-"green3"
sc$pcolor[sc$cluster==3]<-"blue"
sc$pcolor[sc$cluster==4]<-"yellow"
sc$pcolor[sc$cluster==5]<-"brown"

scatterplot3d(membersY111_3$AgeAtFirstClaim,membersY111_3$claims,membersY111_3$PayDelay,
              pch=16,highlight.3d = FALSE,
              color = sc$pcolor,
              main = "3D scatterplot of female group Y1")
pc<- prcomp(as.matrix(scaled_wd))
plot(pc)
#plot of the projected data and add the origina dimensional as arrows:
biplot(pc,col=c('grey','red'))
#kmeans
kclust <- kmeans(scaled_wd,centers = 5)
kclust$cluster
ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = as.factor(kclust$cluster)),size=5)
#---------------------------------
# Internal Clsuter Validation
#---------------------------------

#For both methods: k value against two distance scores
library(fpc)
# fpc::cluster.stats(membersY11_3, km$cluster)
tunek <- kmeansruns(scaled_wd[1:5000,],krange = 1:10,criterion = "ch")
tunek$bestk
tunekw <- kmeansruns(scaled_wd[1:5000,],krange = 1:10,criterion = "asw")
tunekw$bestk 
tunekw <- kmeansruns(scaled_wd[1:5000,],krange = 1:10,criterion = "asw")

#plot
library(factoextra)
fviz_nbclust(scaled_wd[1:5000,-c(1,3)], FUN = hcut, method = "silhouette")

fviz_nbclust(scaled_wd[1:5000,-c(1,3)], FUN = hcut, method = "wss")



