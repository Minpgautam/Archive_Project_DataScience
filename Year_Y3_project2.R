
getwd()
setwd("C:/Users/minga/Documents/EE7331_Project2")

#-----------------------------------------
# clean and prepare data for clustering for year Y3:
#-----------------------------------------
if(!file.exists("membersY3.rda")) {
  
  # Read the file:
  claims<- read.csv(file="Claims.csv",na.strings=c(""))
  members<- read.csv(file="Members.csv",na.strings=c(""))
  drugs<- read.csv(file="DrugCount.csv",na.strings=c(""))
  labs<-read.csv(file="LabCount.csv",na.strings=c(""))
  
  # Filter the Y2 data:
  claimsY3 <- claims[claims$Year == "Y3",]
  drugsY3 <- drugs[drugs$Year == "Y3",]
  labsY3 <- labs[labs$Year == "Y3",]
  membersY3 <- members
  
  
  # add number of claims
  n_claims <- table(claimsY3$MemberID)
  
  membersY3 <- merge(membersY3, data.frame(MemberID=names(n_claims),
                                           claims=as.numeric(n_claims)))
  
  # add median paydelay
  levels(claimsY3$PayDelay)[levels(claimsY3$PayDelay)=="162+"] <- 162
  claimsY3$PayDelay <- as.numeric(as.character(claimsY3$PayDelay))
  membersY3 <- merge(membersY3, aggregate(PayDelay~MemberID,
                                          data=claimsY3, FUN=median))
  # add highest Charlson index
  membersY3 <- merge(membersY3, aggregate(CharlsonIndex~MemberID, data=claimsY3,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY3$CharlsonIndex <- as.factor(membersY3$CharlsonIndex)
  
  membersY3$MemberID <- factor(membersY3$MemberID)
  
  summary(membersY3)
  
  # translate age
  levels(membersY3$AgeAtFirstClaim) 
  age <- gsub("(\\d+).*", "\\1", levels(membersY3$AgeAtFirstClaim))
  age
  
  levels(membersY3$AgeAtFirstClaim) <- age
  membersY3$AgeAtFirstClaim <- as.numeric(as.character(membersY3$AgeAtFirstClaim))
  
  # translate Charlson Index
  levels(membersY3$CharlsonIndex)
  levels(membersY3$CharlsonIndex) <- c(0, 1.5, 3.5, 5)
  membersY3$CharlsonIndex <- as.numeric(as.character(membersY3$CharlsonIndex))
  # Translate DSFS of labsY3
  
  membersY3 <- merge(membersY3, aggregate(DSFS~MemberID, data=drugsY3,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY3$DSFS <- as.factor(membersY3$DSFS)
  
  membersY3$DSFS <- gsub("0- 1 month","1",membersY3$DSFS)
  membersY3$DSFS <- gsub("1- 2 months","2",membersY3$DSFS) # converted to months
  membersY3$DSFS <- gsub("2- 3 months","3",membersY3$DSFS) 
  membersY3$DSFS <- gsub("3- 4 months","4",membersY3$DSFS) 
  membersY3$DSFS <- gsub("4- 5 months","5",membersY3$DSFS) 
  membersY3$DSFS <- gsub("5- 6 months","6",membersY3$DSFS)
  membersY3$DSFS <- gsub("6- 7 months","7",membersY3$DSFS)
  membersY3$DSFS <- gsub("7- 8 months","8",membersY3$DSFS)
  membersY3$DSFS <- gsub("8- 9 months","9",membersY3$DSFS)
  membersY3$DSFS <- gsub("9- 10 months","10",membersY3$DSFS)
  membersY3$DSFS <- gsub("10- 11 months","11",membersY3$DSFS)
  membersY3$DSFS <- gsub("11- 12 months","12",membersY3$DSFS )
  
  membersY3$DSFS <-as.numeric(as.character(membersY3$DSFS ))
  
  # Translate drug counts
  membersY3 <- merge(membersY3, aggregate(DrugCount~MemberID, data=drugsY3,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY3$DrugCount <- gsub("7+","7",membersY3$DrugCount)
  membersY3$DrugCount <- as.numeric(membersY3$DrugCount)
  
  # Translate Lab Counts
  membersY3<- merge(membersY3, aggregate(LabCount~MemberID, data=labsY3,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  
  membersY3$LabCount <- gsub("10+","10",membersY3$LabCount)
  membersY3$LabCount <- as.numeric(membersY3$LabCount)
  
  summary(membersY3)
  # taking the mean of data of certain columns
  for(i in 4:ncol(membersY3)){
    membersY3[is.na(membersY3[,i]), i] <- mean(membersY3[,i], na.rm = TRUE)
  }
  
  # save data for later
  save(membersY3, file = "membersY3.rda")
}

# load the save data:
load("membersY3.rda")
str(membersY3)
summary(membersY3)
library(dplyr)
membersY3<-filter(membersY3,Sex=="F")
#-----------------------------------
# For Clustering
#-----------------------------------

# Let's do some clustering
membersY3_1<- membersY3[c(2,4,5,6,7,8,9)]
# Since if 35% of data is missing then it is better to drop it. Here ther are so many
# values 2716 are missing in ageatfirstclaims. It is better to drop it.
membersY3_2 <- membersY3_1[complete.cases(membersY3_1),]
# membersY2_2<- na.omit(membersY2_1)
colMeans(membersY3_2)
apply(membersY3,2,sd)
# Let check with the boxplot 
boxplot(membersY3_2,main=" Year Y3")
boxplot(membersY3_2[c(3)])
quantile(membersY3_2$PayDelay,c(0,0.005,0.01,0.25,0.5,0.9,0.995,1)) # it seems it doesn't
# effect so we don't  make any change.
# we probably should scale the data and make dataframe to it.
membersY3_3<- data.frame(scale(membersY3_2))
# confirm it:
colMeans(membersY3_3)
apply(membersY3_3,2,sd)  # 2=columns wise sd.

# Let the cluster with k=4
#plot(membersY1_3)
km <- kmeans(membersY3_3,centers = 4)

# look at the centroid
def.par <- par(no.readonly = TRUE,mar=c(10,4,1,1)) # save default
layout(t(1:4))   # 4 plots in one
for(i in 1:4) plot(1:20)
for (i in 1:4) barplot(km$centers[i,],main=paste("Cluster Year_Y3",i), las=2)

par(def.par)

# use k=10 assume Euclidean distance
km <- kmeans(membersY3_3,centers = 5,nstart = 10)
km_min <- kmeans(membersY3_3[1:5000,],centers = 5)
km_min

# plot

plot(membersY3_3[1:5000,],col=km$cluster)
points(km_min$centers,pch=3,cex=2)  # this adds the centroids
text(km_min$centers,labels=1:4,pos = 2)  # this adds the cluster ID






# Let plot first 1000 points only
pairs(membersY3_3[1:1000,], col=km$cluster[1:1000])

# Run variable clustering procedure
#install.packages("Clustofvar")
#  to check which variabe are important
library(ClustOfVar)
var_tree<- hclustvar(membersY1_3)
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
ggparcoord(cbind(data.frame(km$centers),data.frame(id= as.character(1:5))),columns=1:ncol(km$centers),groupColumn='id',title = "Year Y3") +
  theme(axis.text.x = element_text(angle = 90))
library(cluster)
clusplot(membersY3_3[1:5000,], km_min$cluster)



#---------------------------------------------------------------------------
# let do for only four variables AgeAtFirstClaim, Claims, PayDelay and DSFS
#----------------------------------------------------------------------------



#scale the variables

     # our data is ready for clustering
# For hierarchical clustering,we'll first calculate a distance matrix 
# based on Eucledean measure.
#Hierarchical Clustering
scaled_wd<- data.frame(scale( membersY3_3))
d <- dist(scaled_wd, method = "euclidean")
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
# visualize the cluster n data
table(membersY3_3$cluster)
plot(membersY3_3$AgeAtFirstClaim,membersY3_3$claims,pch=21,bg=c("red","green3","blue","yellow","brown")
     [unclass(membersY3_3$cluster)],main="cluster of Age & claims")
plot(membersY3_3$AgeAtFirstClaim,membersY3_3$PayDelay,pch=21,bg=c("red","green3","blue","yellow","brown")
     [unclass(membersY22_3$cluster)],main="cluster of Age & PayDelay")
# above plot didn't gave the meaning so don't consider it.
library(scatterplot3d)
sc <- membersY3_3
sc$pcolor[sc$cluster==1]<-"red"
sc$pcolor[sc$cluster==2]<-"green3"
sc$pcolor[sc$cluster==3]<-"blue"
sc$pcolor[sc$cluster==4]<-"yellow"
sc$pcolor[sc$cluster==5]<-"brown"

scatterplot3d(membersY3_3$AgeAtFirstClaim,membersY3_3$claims,membersY3_3$PayDelay,
              pch=16,highlight.3d = FALSE,
              color = sc$pcolor,
              main = "3D scatterplot of female group Y3")
pc<- prcomp(as.matrix(scaled_wd))
plot(pc)
#plot of the projected data and add the origina dimensional as arrows:
biplot(pc,col=c('grey','red'))
#kmeans
kclust <- kmeans(scaled_wd,centers = 5)
kclust$cluster
ggplot(comp_dt,aes(Comp.1,Comp.2))+ geom_point(aes(color = as.factor(kclust$cluster)),size=3)

#For both methods: k value against two distance scores
library(fpc)
tunek <- kmeansruns(scaled_wd,krange = 1:10,criterion = "ch")
 tunek$bestk
tunekw <- kmeansruns(scaled_wd,krange = 1:5,criterion = "asw")
 tunekw$bestk 
#plot
 library(factoextra)
 fviz_nbclust(scaled_wd, FUN = hcut, method = "silhouette")

 fviz_nbclust(scaled_wd, FUN = hcut, method = "wss")
 
 
 ## 
 # Calculation of measure of two clusters
 
 library(cluster)
 shape<- scale(scaled_wd[1:5000,])
 ks <-2:15
 km1<-kmeans(shape,centers = 5)
 d<- dist(shape)
 hc<- hclust(d,method = "complete")
 hc_3<-cutree(hc,3)
 library("kernlab")
 spec<-specc(shape[1:5000,],centers=5)
 random3 <-sample(1:3,nrow(scaled_wd),replace = TRUE)
 random5 <-sample(1:5,nrow(scaled_wd),replace = TRUE)
 truth <-as.integer(scaled_wd$class)
 r<- rbind(
   kmeans=c(
     unlist(fpc::cluster.stats(d,km1$cluster,truth,compareonly = TRUE))),
   hc=c(
     unlist(fpc::cluster.stats(d,hc_3,truth,compareonly = TRUE))),
   spec=c(
     unlist(fpc::cluster.stats(d,spec,truth,compareonly = TRUE))),
   random3=c(
     unlist(fpc::cluster.stats(d,random3,truth,compareonly = TRUE))),
   random5=c(
     unlist(fpc::cluster.stats(d,random5,truth,compareonly = TRUE))))
 
 

