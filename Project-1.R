setwd("C:/Users/Project 1/hospital")
claims <- read.csv(file="Claims_Y1.csv")
members <- read.csv(file="Members_Y1.csv")
claims2 <- read.csv(file="Claimsnew.csv")
finalclaims2 <- read.csv(file="finalclaims2.csv")
mem <- read.csv(file="MemberDetails.csv")
mer <- merge(members,claims)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

l <- function(v)
{
  length(unique(v[!is.na(v)]))
}

s <- function(v)
{
  sum(v,na.rm = TRUE)
}

d <- data.frame(table(claims$MemberID))
summary(d)
x <- finalclaims2$CharlsonIndex[which(finalclaims2$MemberID==643099505)]
y <- finalclaims2$paydelay[which(finalclaims2$MemberID==643099505)]
mean(y,na.rm = TRUE)
z <- finalclaims2$ProviderID[which(finalclaims2$MemberID==643099505)]
length(unique(z))
w <- finalclaims2$vendor[which(finalclaims2$MemberID==643099505)]
length(unique(w))
q <- finalclaims2$pcp[which(finalclaims2$MemberID==643099505)]
length(unique(q[!is.na(q)]))
r <- finalclaims2$specialty[which(finalclaims2$MemberID==643099505)]
length(unique(r))
k <- finalclaims2$placesvc[which(finalclaims2$MemberID==643099505)]
length(unique(k))

p <- data.frame(table(claims$ProviderID))
summary(p)
pcp <- data.frame(table(claims$pcp))
summary(pcp)
v <- data.frame(table(claims$vendor))
summary(v)

v$Var1[which(v$Freq==52992)]
maxVendor <- claims$MemberID[which(claims$vendor==2999365)]
p$Var1[which(p$Freq==52992)]
maxProvider <- claims$MemberID[which(claims$ProviderID==776433376)]
s1 <- data.frame(vend=maxVendor,pro=maxProvider)
all(s1$pro == s1$memb)

claims$claimsmade<- ave(as.numeric(claims$MemberID),claims$MemberID,FUN = length)
claims$Provider<- ave(as.numeric(claims$ProviderID),claims$MemberID, FUN = l)
claims$Nvendor<- ave(as.numeric(claims$vendor),claims$MemberID, FUN = l)
claims$Npcp<- ave(as.numeric(claims$pcp),claims$MemberID, FUN = l)
claims$Nspecialty<- ave(as.numeric(claims$specialty),claims$MemberID, FUN = l)
claims$Nplacesvc<- ave(as.numeric(claims$placesvc),claims$MemberID, FUN = l)
claims$Avgpaydelay<- ave(as.numeric(claims$paydelay),claims$MemberID)
claims$ModeCharlsonIndex<- ave((claims$CharlsonIndex),claims$MemberID, FUN = getmode)
claims$TotalLOS<- ave((finalclaims2$LOS),finalclaims2$MemberID, FUN = s)

memberD <- data.frame(cbind(MembersID=claims$MemberID,claims=claims$claimsmade,NumProviders=claims$Provider,NumVendors=claims$Nvendor,Numpcp=claims$Npcp,NumSpecialty=claims$Nspecialty,NumPlace=claims$Nplacesvc,AvgPaydelay=claims$Avgpaydelay,CharlsonIndex=claims$ModeCharlsonIndex,TotalLength=claims$TotalLOS))
memberDetails <- unique(memberD)

write.csv(memberDetails,"MemberDetails.csv")
mem <- read.csv(file="MemberDetails.csv")

summary(mem$AvgPaydelay)
sqrt(var(mem$AvgPaydelay,na.rm = TRUE))
ggplot(mem, aes(x=mem$AvgPaydelay))+
  geom_histogram(binwidth=1,fill="steelblue",col="black")+
  theme_minimal()+
  labs(x="Average Number of payment delay", y="Frequency")

summary(mem$NumProviders)
sqrt(var(mem$NumProviders,na.rm = TRUE))


ggplot(mem, aes(x=mem$claims))+
  geom_histogram(binwidth=1,fill="steelblue",col="black")+
  theme_minimal()+
  labs(x="Number of claims", y="Frequency")

summary(mem$claims)
sqrt(var(mem$claims,na.rm = TRUE))

ggplot(mem, aes(x=mem$claims))+
  geom_histogram(binwidth=1,fill="steelblue",col="black")+
  geom_vline(aes(xintercept=mean(mem$claims)),
             color="red", linetype="dashed", size=1)+
  theme_minimal()+
  labs(x="Number of claims", y="Frequency")

ggplot(mem, aes(x=mem$NumProviders, col=mem$Sex))+
  geom_histogram(binwidth=1)+#,fill="steelblue",col="black")+
  geom_vline(aes(xintercept=mean(mem$NumSpecialty)),
             color="red", linetype="dashed", size=1)+
  theme_minimal()+
  labs(x="Number of claims", y="Frequency")


summary(mem$NumProviders)
sqrt(var(mem$NumProviders,na.rm = TRUE))

plot(mem$claims,mem$NumProviders)

ggplot(mem, aes(x=mem$claims, y=mem$NumProviders)) + 
  geom_point(color="steelblue")+
  stat_smooth(method="lm", se=FALSE)+
  theme_minimal()+
  labs(x="Number of claims", y="Number of providers")


ggplot(mem, aes(x=mem$NumProviders, y=mem$NumVendors)) + 
  geom_point(color="steelblue")+
  stat_smooth(method="lm", se=FALSE)+
  theme_minimal()+
  labs(x="Number of providers", y="Number of vendors")


k <- function(v)
{
  length(!is.na(v))
}
################################################################################################

claims$claimsmade<- ave(as.numeric(claims$MemberID),claims$ProviderID,FUN = length())
claims$Nvendor<- ave(as.numeric(claims$vendor),claims$ProviderID, FUN = l)
claims$Nspecialty<- ave(as.numeric(claims$specialty),claims$ProviderID, FUN = l)
claims$Nplacesvc<- ave(as.numeric(claims$placesvc),claims$ProviderID, FUN = l)
claims$Avgpaydelay<- ave(as.numeric(claims$paydelay),claims$ProviderID)
claims$ModeCharlsonIndex<- ave((claims$CharlsonIndex),claims$ProviderID, FUN = getmode)
claims$TotalLOS<- ave((finalclaims2$LOS),finalclaims2$ProviderID, FUN = s)


memberD <- data.frame(cbind(mer$sex,ProviderID=claims$ProviderID,claims=claims$claimsmade,NumVendors=claims$Nvendor,NumSpecialty=claims$Nspecialty,NumPlace=claims$Nplacesvc,AvgPaydelay=claims$Avgpaydelay,CharlsonIndex=claims$ModeCharlsonIndex,TotalLength=claims$TotalLOS))
memberDetails <- unique(memberD)

write.csv(memberDetails,"ProviderDetails.csv")
mem <- read.csv(file="ProviderDetails.csv")


summary(mem$AvgPaydelay)
sqrt(var(mem$AvgPaydelay,na.rm = TRUE))
ggplot(mem, aes(x=mem$AvgPaydelay))+
  geom_histogram(binwidth=1,fill="steelblue",col="black")+
  theme_minimal()+
  labs(x="Average Number of payment delay", y="Frequency")


ggplot(mem, aes(x=mem$NumSpecialty, y=mem$NumVendors)) + 
  geom_point(color="steelblue")+
  stat_smooth(method="lm", se=FALSE)+
  theme_minimal()+
  labs(x="Number of providers", y="Number of vendors")


ggplot(mem, aes(x=mem$NumVendors))+
  geom_histogram(binwidth=1,fill="steelblue",col="black")+
  geom_vline(aes(xintercept=mean(mem$claims)),
             color="red", linetype="dashed", size=1)+
  theme_minimal()+
  labs(x="Number of claims", y="Frequency")
