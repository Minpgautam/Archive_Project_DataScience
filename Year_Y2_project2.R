
getwd()
setwd("C:/Users/minga/Documents/EE7331_Project2")

#-----------------------------------------
# clean and prepare data for clustering for year Y2:
#-----------------------------------------
if(!file.exists("membersY2.rda")) {
  
  # Read the file:
  claims<- read.csv(file="Claims.csv",na.strings=c(""))
  members<- read.csv(file="Members.csv",na.strings=c(""))
  drugs<- read.csv(file="DrugCount.csv",na.strings=c(""))
  labs<-read.csv(file="LabCount.csv",na.strings=c(""))
  
  # Filter the Y2 data:
  claimsY2 <- claims[claims$Year == "Y2",]
  drugsY2 <- drugs[drugs$Year == "Y2",]
  labsY2 <- labs[labs$Year == "Y2",]
  membersY2 <- members
  
  
  # add number of claims
  n_claims <- table(claimsY2$MemberID)
  
  membersY2 <- merge(membersY2, data.frame(MemberID=names(n_claims),
                                           claims=as.numeric(n_claims)))
  
  # add median paydelay
  levels(claimsY2$PayDelay)[levels(claimsY2$PayDelay)=="162+"] <- 162
  claimsY2$PayDelay <- as.numeric(as.character(claimsY2$PayDelay))
  membersY2 <- merge(membersY2, aggregate(PayDelay~MemberID,
                                          data=claimsY2, FUN=median))
  # add highest Charlson index
  membersY2 <- merge(membersY2, aggregate(CharlsonIndex~MemberID, data=claimsY2,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$CharlsonIndex <- as.factor(membersY2$CharlsonIndex)
  
  membersY2$MemberID <- factor(membersY2$MemberID)
  
  summary(membersY2)
  
  # translate age
  levels(membersY2$AgeAtFirstClaim) 
  age <- gsub("(\\d+).*", "\\1", levels(membersY2$AgeAtFirstClaim))
  age
  
  levels(membersY2$AgeAtFirstClaim) <- age
  membersY2$AgeAtFirstClaim <- as.numeric(as.character(membersY2$AgeAtFirstClaim))
  
  # translate Charlson Index
  levels(membersY2$CharlsonIndex)
  levels(membersY2$CharlsonIndex) <- c(0, 1.5, 3.5, 5)
  membersY2$CharlsonIndex <- as.numeric(as.character(membersY2$CharlsonIndex))
  # Translate DSFS of labsY2
  
  membersY2 <- merge(membersY2, aggregate(DSFS~MemberID, data=drugsY2,
                                          FUN=function(x) levels(x)[which.max(table(x))]))
  membersY2$DSFS <- as.factor(membersY2$DSFS)
  
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
  

  # taking the mean of data of certain columns
  for(i in 4:ncol(membersY2)){
    membersY2[is.na(membersY2[,i]), i] <- mean(membersY2[,i], na.rm = TRUE)
  }
  
  # save data for later
  save(membersY2, file = "membersY2.rda")
}

# load the save data:
load("membersY2.rda")
library(dplyr)
membersY2<-filter(membersY2,Sex=="F")
str(membersY2)
summary(membersY2)

#-----------------------------------
# For Clustering
#-----------------------------------


# Let's do some clustering
membersY2_1<- membersY2[c(2,4,5,6,7,8,9)]
# Since if 35% of data is missing then it is better to drop it. Here ther are so many
# values 2716 are missing in ageatfirstclaims. It is better to drop it.
membersY2_2 <- membersY2_1[complete.cases(membersY2_1),]
# membersY2_2<- na.omit(membersY2_1)

# Let check with the boxplot 
boxplot(membersY2_2,main=" Year Y2")
boxplot(membersY2_2[c(3)])
quantile(membersY2_2$PayDelay,c(0,0.005,0.01,0.25,0.5,0.9,0.995,1)) # it seems it doesn't
# effect so we don't  make any change.
# we probably should scale the data and make dataframe to it.
membersY2_3<- data.frame(scale(membersY2_2))
# confirm it:
colMeans(membersY2_3)
apply(membersY2_3,2,sd)  # 2=columns wise sd.

# Let the cluster with k=4
#plot(membersY1_3)
km <- kmeans(membersY2_3,centers = 5)

# look at the centroid
def.par <- par(no.readonly = TRUE,mar=c(10,4,1,1)) # save default
layout(t(1:4))   # 4 plots in one
for(i in 1:4) plot(1:20)
for (i in 1:4) barplot(km$centers[i,],main=paste("Cluster Year_Y2",i), las=2)

par(def.par)

# use k=10 assume Euclidean distance
km <- kmeans(membersY2_3,centers = 5,nstart = 10)
km_min <- kmeans(membersY2_3[1:5000,],centers = 5)
km_min
# plot
plot(membersY2_3[1:5000,],col=km$cluster)
points(km_min$centers,pch=3,cex=2)  # this adds the centroids
text(km_min$centers,labels=1:4,pos = 2)  # this adds the cluster ID






# Let plot first 1000 points only
pairs(membersY2_3[1:1000,], col=km$cluster[1:1000])

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
str(membersY2_3)








#-----------------------------
# K-Mean Visualization:
#-----------------------------
library(GGally)
ggparcoord(cbind(data.frame(km$centers),data.frame(id= as.character(1:5))),columns=1:ncol(km$centers),groupColumn='id',title = "Year Y2") +
  theme(axis.text.x = element_text(angle = 90))
library(cluster)
clusplot(membersY2_3[1:5000,], km_min$cluster)

#scale the variables

# our data is ready for clustering
# For hierarchical clustering,we'll first calculate a distance matrix 
# based on Eucledean measure.
#Hierarchical Clustering
scaled_wd<- data.frame(scale( membersY2_3))
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

membersY22_3 <- data.frame(membersY2_3,comp_dt)
# visualize the cluster n data
table(membersY22_3$cluster)
plot(membersY22_3$AgeAtFirstClaim,membersY22_3$claims,pch=21,bg=c("red","green3","blue","yellow","brown")
     [unclass(membersY22_3$cluster)],main="cluster of Age & claims")
plot(membersY22_3$AgeAtFirstClaim,membersY22_3$PayDelay,pch=21,bg=c("red","green3","blue","yellow","brown")
     [unclass(membersY22_3$cluster)],main="cluster of Age & PayDelay")
# above plot didn't gave the meaning so don't consider it.
library(scatterplot3d)
sc <- membersY22_3
sc$pcolor[sc$cluster==1]<-"red"
sc$pcolor[sc$cluster==2]<-"green3"
sc$pcolor[sc$cluster==3]<-"blue"
sc$pcolor[sc$cluster==4]<-"yellow"
sc$pcolor[sc$cluster==5]<-"brown"

scatterplot3d(membersY22_3$AgeAtFirstClaim,membersY22_3$claims,membersY22_3$PayDelay,
              pch=16,highlight.3d = FALSE,
              color = sc$pcolor,
              main = "3D scatterplot of female group Y2")
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


# For the External cluster validataion:

library(cluster)
shape<- scale(scaled_wd)
ks <-2:15
km1<-kmeans(shape,centers = 5)
d<- dist(shape)
hc<- hclust(d,method = "complete")
hc_3<-cutree(hc,3)

ASW<- sapply(ks,FUN = function(k){
  fpc::cluster.stats(d,cutree(hc,k))$avg.silwidth
})
plot(ks,ASW,type = 'l')
ks[which.max(ASW)]
abline(v=ks[which.max(ASW)],col='red',lty=2)

# Calculation of measure of two clusters
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



