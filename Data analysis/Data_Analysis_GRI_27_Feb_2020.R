
#Data Analysis: GRI experiment
# Author code: Denise Laroze
# Co-authors paper: David Hugh-Jones and Arndt Leininger
# 27/Feb/2020


#setwd("[address your folder]")

#bd<-"[address your folder]"


#setwd("C:/Users/dalaro/Dropbox/group ID in legislative bargaining/Data Analysis GRI")
setwd("C:/Users/Denise Laroze/Dropbox/group ID in legislative bargaining/Data Analysis GRI")
#Baseline directory for saving plots and tables
bd<-"C:/Users/Denise Laroze/Dropbox/group ID in legislative bargaining/Social Identity 2019-20/Data analysis/"
#bd<-"C:/Users/André Laroze/Dropbox/group ID in legislative bargaining/Presentations/"
 
library("car")
library("ggplot2")
library("mgcv")
library("texreg")
library(plyr)
library(plm)
#library(flexmix)
library(Hmisc)
library(censReg)
library(ineq)
library(lmtest)
library(stringr)
library(nnet) 
library(arm)
library(effects)
library(psych)
library(rms)
library("multcomp")
library(xtable)
library(pglm) 
library(multiwayvcov)
library(Rmisc)


#Merging datasets
#Corrections to data. In the code used in the Control sessions there was an error: the ideo1-10 scores were not registered
#correctly in the dataset. This did not affect any aspect of the experimental trial (as the data is only recorded for control
#purposes). For this reason, the data for the ideo1-10 scores are errased from the data frame. 
#If necesarry to use the data for statistical analysis, the correct data can be surched for by looking into how the original files were
#created)

 #mydf1<-read.csv("GRIexpt-2014-12-02-114543.csv")
 #for (i in 1:10) mydf1[[paste0("ideo", i)]] <- NA
 #mydf2<-read.csv("GRIexpt-2014-12-02-145238.csv")
 #for (i in 1:10) mydf2[[paste0("ideo", i)]] <- NA
 #mydf3<-read.csv("GRIexpt-2014-12-03-105420.csv")
 #mydf4<-read.csv("GRIexpt-2014-12-03-125727.csv")
 #mydf5<-read.csv("GRIexpt-2014-12-03-142507.csv")
 #mydf6<-read.csv("GRIexpt-2014-12-03-155742.csv")
 #mydf7<-read.csv("GRIexpt-2015-02-25-123025.csv")
 #mydf8<-read.csv("GRIexpt-2015-02-25-102407.csv")
 #mydf9<-read.csv("GRIexpt-2015-02-25-153453.csv")
 #mydf10<-read.csv("GRIexpt-2015-02-25-141544.csv")
 #mydf11<-read.csv("GRIexpt-2015-02-24-122234.csv")
 #mydf12<-read.csv("GRIexpt-2015-02-24-102521.csv")
 #mydf13<-read.csv("GRIexpt-2015-02-24-154255.csv")
 #mydf14<-read.csv("GRIexpt-2015-02-24-140532.csv")
 
 
#mydf<-rbind(mydf1, mydf2, mydf3, mydf4, mydf5, mydf6, mydf7, mydf8, mydf9, mydf10, mydf11, mydf12, mydf13, mydf14)
#write.csv(mydf,file="GRI_Dec2014-Feb2015_sessions.csv",row.names=F)
#rm(mydf1, mydf2, mydf3, mydf4, mydf5, mydf6)

 
mydf<-read.csv("GRI_Dec2014-Feb2015_sessions.csv") 
 
 
##########################
###############   Data 
##########################


# Individual id number 
mydf$uid<- paste(mydf$session, mydf$id,  sep=".")
 
#unique merging group, a.k.a unique observation
mydf$umg<-paste(mydf$session, mydf$mg,  sep=".")


 
 ############### PREPARATION OF DATAFRAME FOR DATA ANALYSIS



##### Differences Max and Min offers to other subjects in the group
id<-rep(NA, nrow(mydf))
others<-list(rep(NA, 2))
#Identifying who is closer in gender, race and ideology to the proposer, and who the proposer is offering more to.
for (i in 1:nrow(mydf)){
  id[i]<-mydf$group_id[i]
  others <- setdiff(1:3, id[i])
  var.ideo<-paste0("prop", others)
  
  mydf$max_other_prop[i]<-if(!is.na(mydf$prop1[i])) apply(mydf[i,var.ideo], 1, max, na.rm=TRUE) else NA
  mydf$min_other_prop[i]<-if(!is.na(mydf$prop1[i])) apply(mydf[i,var.ideo], 1, min, na.rm=TRUE) else NA
} 
rm(others, id)


 #Difference between Max and min offers                                              
mydf[, "diff_other_prop"] <- (mydf$max_other_prop-mydf$min_other_prop)


#Offers to other participants
id<-rep(NA, nrow(mydf))
prop.A<-rep(NA, nrow(mydf))
prop.B<-rep(NA, nrow(mydf))
others<-list(rep(NA, 2))
#Identifying who is closer in ideology to the proposer, and who the proposer is offering more to.
for (i in 1:nrow(mydf)){
  id[i]<-mydf$group_id[i]
  others <- setdiff(1:3, id[i])
  mydf$prop.A[i]<-mydf[[paste0("prop", others[1], sep="")]][[i]]
  mydf$prop.B[i]<-mydf[[paste0("prop", others[2], sep="")]][[i]]
  
}
rm(id, others)

# Identifying proposals where one of the other two people is allocated zero
#col.name1<-which( colnames(mydf)=="prop.A" )
#col.name2<-which( colnames(mydf)=="prop.B" )

 for(i in 1:nrow(mydf)){
  prop.v<-mydf[i, c("prop.A", "prop.B")] 
  mydf$zero.prop[i]<-ifelse(is.na(mydf$prop1[i]), NA, ifelse(0 %in% prop.v, 1 , 0))
  }

 #check<-mydf[ c("group_id", "uid", "prop1", "prop2", "prop3", "zero.prop") ]
rm(prop.v)
 
 
######### Offers in Dictator Games
#How much each participant "gave" to other players in total
mydf[, "generosity"] <-  ifelse(mydf$group_id==1, mydf$pd1-3, ifelse (mydf$group_id==2, mydf$pd2-3 , mydf$pd3-3))
mydf$generosity<-abs(mydf$generosity)



# To fix the -Inf values
is.na(mydf) <- do.call(cbind,lapply(mydf, is.infinite))

 
#check<-mydf[ c("group_id", "uid", "prop1", "prop2", "prop3", "partner.id", "other.id", "x") ]
 
### Code for identifying group types 
 mydf$ugroup<- paste(mydf$session, mydf$group,  sep=".") 

 #Simplification of race categories
 mydf$race2<-as.character(mydf$race)
 mydf$race2[mydf$race2 == "brown" ]<-"other"
 mydf$race2[mydf$race2 == "black" ]<-"other"
 
 tmp<-by(mydf, interaction(mydf$ugroup, mydf$period),  function(x) {
  for (i in 1:nrow(x))
  {
    #Characteristics of the groups
    x$g.gender[i]<-sum(str_count(x$gender, "female"))
    x$g.race[i]<-sum(str_count(x$race, "white"))
    sp.v<-x$sp 
    x$g.sp.sd[i] <- sd(sp.v, na.rm = TRUE)
    
    
    #proposer ID
    prop_id <- as.numeric(sub("prop_", "", x$offer[1]))
    x$prop_id<-prop_id
    x$prop.sp<-x$sp[x$group_id == x$prop_id[i]]
    x$prop.ideo.diff[i]<- ifelse(is.na(x$prop_id[i]), NA, abs(x$sp[i] - x$sp[x$group_id == x$prop_id[i]]))
    x$prop.same.gender[i]<-ifelse(is.na(x$prop_id[i]), NA, ifelse(x$gender[i]==x$gender[x$group_id == x$prop_id[i]], 1, 0))
    x$prop.same.race[i]<-ifelse(is.na(x$prop_id[i]), NA, ifelse(x$race2[i]==x$race2[x$group_id == x$prop_id[i]], 1, 0))
    x$prop.same.race.all[i]<-ifelse(is.na(x$prop_id[i]), NA, ifelse(x$race[i]==x$race[x$group_id == x$prop_id[i]], 1, 0))
    
    #Offers made to each participant every round
    prop_p <- numeric(0)
    for (j in 1:3) prop_p[j] <- which(x$group_id==j)
    
    ##Identifying how much was offered to each group member each round
    roni <- as.matrix(t(x[prop_p, paste0("prop", 1:3)]))
    x$p.offered <- as.numeric(roni[, prop_id])
   
    ##Identifies me for exclusion
    for (w in 1:3) x$prop_dummy[w]<- ifelse(is.na(x$prop_id[w]), NA, ifelse(x$group_id[w]==x$prop_id[w], 1, 0))
    x$sd.p.offered<-sd(x$p.offered)
    
  }
  
  return(x) 
})


 
mydf <- do.call("rbind", tmp)
 
 
 
mydf<-mydf[order(mydf$session, mydf$period,mydf$group, mydf$group_id),]
rm(tmp)

 tmp<-by(mydf, interaction(mydf$ugroup, mydf$period),  function(x) {
   for (i in 1:nrow(x))
   { #Characteristics of the "receivers" 
     x$go1[i]<-as.character(x[1,"gender"])
     x$go2[i]<-as.character(x[2,"gender"])
     x$go3[i]<-as.character(x[3,"gender"])
     x$ro1[i]<-as.character(x[1,"race"])
     x$ro2[i]<-as.character(x[2,"race"])
     x$ro3[i]<-as.character(x[3,"race"])
     x$spo1[i]<-as.character(x[1,"sp"])
     x$spo2[i]<-as.character(x[2,"sp"])
     x$spo3[i]<-as.character(x[3,"sp"])
    }
   return(x) 
 })
 
 mydf <- do.call("rbind", tmp)
 mydf<-mydf[order(mydf$session, mydf$period,mydf$group, mydf$group_id),]
 rm(tmp) 
 
#Corrections to make sure no there are no problems in the data frame
 
 mydf$p.gender[is.na(mydf$diff_other_prop)] <-NA
 mydf$p.race[is.na(mydf$diff_other_prop)] <-NA
 
#Checks
#r1<-mydf[mydf$round==1,]
#check<-mydf[ c("group_id", "gender", "prop2", "prop3","offer",  "p.offered" ) ]
#rm(r1)

###########
#Decision in Dictator Game added to all corresponding subj cells (needed for data analysis)
maxp<-max(mydf$userperiod)
mydf<- merge(mydf, mydf[mydf$userperiod == maxp, c("uid","pd1", "pd2", "pd3", "generosity")], by.x = "uid", by.y = "uid", suffixes = c("", "all")) 
mydf<-mydf[order(mydf$session, mydf$period,mydf$mg, mydf$group_id),]

mydf<-mydf[order(mydf$session, mydf$period,mydf$group, mydf$group_id),]


 
 
 write.csv(mydf,file="GRI_mydf.csv",row.names=F)
 

######################## 
####################################################################### 
############# Data Analysis
####################################################################### 
########################

 mydf<-read.csv("GRI_mydf.csv")  

####################################
##########Subsetting data frames
#################################### 
 
ps<-cbind(mydf$prop1, mydf$prop2, mydf$prop3)
mydf<-transform(mydf, SD.props=apply(ps,1, sd, na.rm = TRUE))
rm(ps)
mydf$mg_id <- as.numeric(mydf$umg)
######## Subsetting data


#Subsets of data
mydf$umg<-paste(mydf$session, mydf$mg,  sep=".")


#Reshaping long, first round of the data frame
vars <- c(outer(c("prop", "go", "ro", "spo"), 1:3, FUN=paste0)) 
mydf.l <- reshape(mydf[,c("treatment" , "id", "userperiod", "round", "uid" ,  "umg", "mg_id", "mg", "group", "ugroup",
                          "group_id", "gender","race" , "race2", "g.gender", "g.race", "g.sp.sd" , "sp", 
                          "generosityall", "vote", "p.offered", "zero.prop", vars)], 
                  idvar=c("treatment" , "id", "userperiod", "round", "mg", "uid" , "umg", "mg_id", "group", "ugroup", 
                          "group_id", "gender", "race" , "race2",  "g.gender", "g.race", "g.sp.sd" , "sp",
                          "generosityall", "vote", "p.offered", "zero.prop" ), timevar="recipient",
                  varying=list(prop=paste0("prop", 1:3), go1=paste0("go", 1:3), ro=paste0("ro", 1:3), spo=paste0("spo", 1:3)), 
                  direction="long", new.row.names=1:(nrow(mydf)*3))

for (i in 1:nrow(mydf.l)){
  mydf.l$me[i]<- if (mydf.l$group_id[i]==mydf.l$recipient[i]) 1 else 0
}


### Creating independent variables of interest in long data frame

#Simplification of race categories
mydf.l$ro2<- as.character(mydf.l$ro1)
mydf.l$ro2[mydf.l$ro2 == "brown" ]<-"other"
mydf.l$ro2[mydf.l$ro2 == "black" ]<-"other"

#Ideological Distance of the proposer to the "other" group member
pair<-list(rep(NA, 3))
not_in_pair<-c(NA)
mydf.l$go.o<-NA
mydf.l$ro.o<-NA
mydf.l$sp.o<-NA
#mydf<-mydf[order(mydf$session, mydf$period,mydf$group, mydf$group_id),]
#check<-mydf.l[, c("group_id", "spo1", "sp.o", "go1", "ro2")]
#View(check)
tmp<-by(mydf.l, interaction(mydf.l$ugroup, mydf.l$userperiod, mydf.l$group_id),  function(x) {
  for (i in 1:nrow(x))
  {if(x$recipient[i] != x$group_id[i]){
    pair[i]<-list(c(x$group_id[i], x$recipient[i] ))
    not_in_pair[i] <- setdiff(1:3, unlist(pair[i]))
    n<-not_in_pair[i]
    
    # Characteristics of the 3rd group member
    x$sp.o[i]<-x$spo1[n]
    x$go.o[i]<-as.character(x$go1[n])
    x$ro.o[i]<-x$ro2[n]
    x$ro.o.all[i]<-as.character(x$ro1[n])
    
  } else {
    x$sp.o[i]<-NA
    x$go.o[i]<-NA
    x$ro.o[i]<-NA
    x$ro.o.all[i]<-NA
  }
  }
  return(x) 
})

mydf.l <- do.call("rbind", tmp)
mydf.l <-mydf.l[order(mydf.l$userperiod, mydf.l$umg,  mydf.l$ugroup, mydf.l$group_id ),]
rm(tmp)

#Difference in Difference, distance with the receiver and other. 
mydf.l$did.sp<-NA
mydf.l$did.sp<-with(mydf.l, abs(abs(sp - spo1) - abs(sp - sp.o)) )


#Ideological distance
mydf.l$spo1<-as.numeric(mydf.l$spo1)
mydf.l$ideo.diff<- abs(mydf.l$sp-mydf.l$spo1)
mydf.l$ideo.diff.o<-abs(mydf.l$sp - mydf.l$sp.o)

#same race and gender categories for treatment
mydf.l$same.g <- ifelse(mydf.l$gender == mydf.l$go1, 1, 0)
mydf.l$same.r <- ifelse(mydf.l$race2 == mydf.l$ro2, 1, 0)
mydf.l$same.r.all<- ifelse(mydf.l$race == mydf.l$ro1, 1, 0)
mydf.l$same.g.o<-ifelse(mydf.l$gender==mydf.l$go.o, 1, 0)
mydf.l$same.r.o<-ifelse(mydf.l$race2==mydf.l$ro.o, 1, 0)
mydf.l$same.r.o.all<- ifelse(mydf.l$race==mydf.l$ro.o.all, 1, 0)
#check<-mydf.l[, c("userperiod", "group_id", "recipient", "sp", "spo1", "sp.o", "gender", "go1", "go.o", "same.g.o" ,"race2", "ro2", "ro.o", "same.r.o")]
#View(check)

#Time variable long dataset, for panel robustness tests
mydf.l$plm.time<- paste(mydf.l$userperiod, mydf.l$recipient,  sep=".")


#########Separating treatment and baseline dataframes 
# differences in the amounts given to each of the other part in the first round of negotiation after period 2
#(valid for comparison purposed)
base.dfl<- subset(mydf.l, treatment== "Control")
base.df<-subset(mydf, treatment== "Control")
treat.dfl<- subset(mydf.l, treatment== "GRI")
treat.df<- subset(mydf, treatment== "GRI")

#Exclusion of proposer for analysis of votes
treat.df.o<-subset(treat.df, prop_dummy==0 )

#Data frame for offers to the other 2 participants
treat.dfl.o<-subset(treat.dfl, me== 0)
base.dfl.o<-subset(base.dfl, me== 0)
mydf.l.o<-subset(mydf.l, me== 0)


#identifying which other group member was allocated zero
treat.dfl.o$non.zero<-0
treat.dfl.o$non.zero[treat.dfl.o$prop1 > 0 ] <- 1
treat.dfl.o$non.zero[is.na(treat.dfl.o$prop1)] <- NA


########## Subsets of data for round 1
treat.r1.df<-subset(treat.df, round==1)
treat.r1.dfl<-subset(treat.dfl, round==1)
treat.r1.df.o<-subset(treat.df.o, round==1)
treat.r1.dfl.o<-subset(treat.dfl.o, round==1)

base.r1.df.o<-subset(mydf, treatment=="Control" & round==1 & prop_dummy==0)
base.r1.dfl.o<-subset(base.dfl.o, round==1)

##########################################
###########################  General Graphics
##########################################


library(MASS)

# Joint Kernall Density
h1 <- hist(treat.r1.df$prop.A, freq=F)
h2 <- hist(treat.r1.df$prop.B, freq=F)
#top <- max(h1$counts, h2$counts)
#k <- kde2d(df$x, df$y, n=25)






 
 #### Histograms for variables of interest
  
#######Gender
# png(filename=paste0(bd, "hist_gender.png", sep=""))
 histogram(treat.r1.df$gender, col="gray", xlab ="Gender")
# dev.off()

 race3<-factor(treat.r1.dfl$race2)
 #Race
 png(filename=paste0(bd, "hist_race.png", sep=""))
 histogram(race3, col="gray", xlab="Race") 
 dev.off() 
 
 
# latex(summary(treat.r1.df$race), caption="Number of subjects per race category", file=paste("race", ".tex", sep="") )
 
###### Histogram Offers
 png(filename=paste0(bd, "offers.png", sep=""))
 hist(treat.r1.dfl.o$prop1, freq=F, col="gray",bar.width=0.8, main="", xlim=c(0,17), xlab="Pounds Offered" )                     # centered at 4
 dev.off()
 
 
 
 props<-hist(treat.r1.dfl.o$prop1, freq=F, col="blue", xaxt="n", main="", xlab="pounds") 
 + axis(side=1,at=props$mids,labels=c(0:12))
 #ggsave("C:/Users/Andr? Laroze/Dropbox/group ID in legislative bargaining/Presentations/hist_props_others.png")
 
 
 #### Dataset of offers towards other people
 
 # Plots Gender in different circumstances
 pg <- ggplot(treat.r1.dfl.o, aes(gender, prop1)) 
 pg <- pg + geom_boxplot(aes(fill = factor(same.g)))
 pg <- pg + xlab("Proposer's Gender") + ylab("pounds") + ggtitle("Offers")
 pg <- pg + scale_fill_discrete(name="Same Gender", labels=c("Different", "Same"))
 pg <- pg + facet_wrap( ~ g.gender) + theme_bw()
 pg
 ggsave(paste0(bd, "prop_gender_gt.png", sep=""))
 
 #lmpg<-lm(prop1~go1*factor(g.gender)+gender, data= treat.r1.dfl.o)
 #summary(lmpg)
 #lmpg.cl<-cl(lmpg, treat.r1.dfl.o$umg)
 
 
 #library(effects) # for plotting interaction terms, but it's very basic
 #plot(effect(term="go1*g.gender",mod=lmpg,default.levels=20),multiline=TRUE)
 
  
 # Plots Race in different circumstances
 pr <- ggplot(treat.r1.dfl.o, aes(race2, prop1)) 
 pr <- pr + geom_boxplot(aes(fill = factor(same.r)))
 pr <- pr + xlab("Proposer's Race") + ylab("pounds") + ggtitle("Offers") 
 pr <- pr + scale_fill_discrete(name="Same Race", labels=c("Different", "Same"))
 pr <- pr + facet_wrap( ~ g.race) + theme_bw()
 pr
 ggsave(paste0(bd, "prop_race_gt.png", sep=""))
  
 #lmpr<-lm(prop1~ro2*factor(g.race)+race2, data= treat.r1.dfl.o)
 #summary(lmpr) 
 #lmpr.cl<-cl(lmpr, treat.r1.dfl.o$umg)
 
 #Ideological distance by N? female group members
 qplot(ideo.diff, prop1, data = treat.r1.dfl.o, geom = c("point", "smooth"),
       method="lm", shape=factor(g.gender), colour=factor(g.gender), ylab="pounds",
       xlab="Absolute value ideological distance" ,main="Offers to other participants ",  
       na.rm=TRUE) + facet_grid(g.gender~.)
 
 #Ideological distance by N? white group members
 qplot(ideo.diff, prop1, data = treat.r1.dfl.o, geom = c("point", "smooth"),
       method="lm", shape=factor(g.race), colour=factor(g.race), ylab="pounds",
       xlab="Absolute value ideological distance" ,main="Offers to other participants ",  
       na.rm=TRUE) + facet_grid(g.race~.)
 
 ######## Tables
 
 #### mean offers to participants, if man in 1 female 2 male group

 #Table male subjects
 type1.1<- treat.r1.dfl.o$gender=="male" & treat.r1.dfl.o$go=="female" & treat.r1.dfl.o$g.gender==1 
 type2.1<- treat.r1.dfl.o$gender=="male" & treat.r1.dfl.o$go=="male" & treat.r1.dfl.o$g.gender==1 
 type3.1<- treat.r1.dfl.o$gender=="male" & treat.r1.dfl.o$go=="female" & treat.r1.dfl.o$g.gender==2 
 type4.1<- treat.r1.dfl.o$gender=="male" & treat.r1.dfl.o$go=="male" & treat.r1.dfl.o$g.gender==2 
 
  
 #Table Gender Interactions
 type1.2<- treat.r1.dfl.o$gender=="female" & treat.r1.dfl.o$go=="female" & treat.r1.dfl.o$g.gender==1 
 type2.2<- treat.r1.dfl.o$gender=="female" & treat.r1.dfl.o$go=="male" & treat.r1.dfl.o$g.gender==1 
 type3.2<- treat.r1.dfl.o$gender=="female" & treat.r1.dfl.o$go=="female" & treat.r1.dfl.o$g.gender==2 
 type4.2<- treat.r1.dfl.o$gender=="female" & treat.r1.dfl.o$go=="male" & treat.r1.dfl.o$g.gender==2 
 
 
 t1.1<-summary(treat.r1.dfl.o$prop1[type1.1])
 t2.1<-summary(treat.r1.dfl.o$prop1[type2.1])
 t3.1<-summary(treat.r1.dfl.o$prop1[type3.1])
 t4.1<-summary(treat.r1.dfl.o$prop1[type4.1])
 
 t1.2<-summary(treat.r1.dfl.o$prop1[type1.2])
 t2.2<-summary(treat.r1.dfl.o$prop1[type2.2])
 t3.2<-summary(treat.r1.dfl.o$prop1[type3.2])
 t4.2<-summary(treat.r1.dfl.o$prop1[type4.2])
 
 
 table.lags1 <- as.table(cbind(t1.1[4], t2.1[4], t1.2[4], t2.2[4]))
 table.lags2 <- as.table(cbind(t3.1[4], t4.1[4], t3.2[4], t4.2[4]))
 table.lags <- as.table(rbind(table.lags1, table.lags2))
 
 
 colnames(table.lags) <- c("MF", "MM", "FF", "FM")
 rownames(table.lags) <- c("1F 2M", "2F 1M")
 table.lags

 
####################################### 
### Ideology and distance  -  Figure 2
#######################################
 
 png(filename=paste0(bd, "hist_sp.png"))
 discrete.histogram(treat.r1.dfl$sp, prob.col="gray",bar.width=0.8 , xlab="Ideological Self-Placement", freq=F, main="", cex.lab=1.5)
 dev.off()
 
 png(filename=paste0(bd, "hist_ideo_diff.png"))
 discrete.histogram(treat.r1.dfl.o$ideo.diff, prob.col="gray",bar.width=0.8 , xlab="Absolute value ideological distance", freq=F, main="", cex.lab=1.5)
 dev.off()
 
 
 
#############
#### Figure 3 - left
############## 
 prop.A<-treat.r1.df$prop.A
 prop.B<-treat.r1.df$prop.B  
 
 #Graph of the offers to other participants
 h.offers<-data.matrix(cbind(prop.A, prop.B))
 
 require(ggplot2)
 p <- ggplot(treat.r1.df, aes(max_other_prop, min_other_prop)) + scale_fill_gradient(low="grey90", high="black")
 p <- p + stat_bin2d(bins = 20) + xlab("Larger offer") +  ylab("Smaller offer") + xlim(c(0,15)) + ylim(c(0,15)) + theme_bw()
 p <-p + geom_text(aes(4, 0.2, label="NE"), colour="blue") +  theme(text=element_text(size=24))
 p
 ggsave(filename=paste0(bd, "offers.png", sep=""),  width=9, height=6)

 
###################### 
#### Figure 3 - right
######################## 
 #Descriptive statistics
 
 treat.r1.df$votetable<-treat.r1.df$vote_result
 treat.r1.df$votetable[treat.r1.df$votetable==0]<-"Rejected"
 treat.r1.df$votetable[treat.r1.df$votetable==1]<-"Accepted"
 treat.r1.df$votetable<-as.factor(treat.r1.df$votetable)
 
 
 
 votetable<-with(treat.r1.df,table(votetable, useNA = "no"))
 votetable<-prop.table(votetable)
 tablenames<-c("Accepted", "Rejected")
 print(votetable[c(1,3)])
 
 png(paste0(bd, "votes_model.png"))
 barplot(votetable[c(1,3)], main="",
         xlab="Vote Result", col="grey", 
         ylab="Ratio", ylim=0:1, cex.names=1.5, cex.axis=1.5)
 
 dev.off()
 
 
 ### Treatment sessions vs control
# votetable<-with(mydf.r1,table(vote, Treatment = treatment, useNA = "no"))
# votetable<-prop.table(votetable,2)
# print(votetable)
 #Latex
# votetable<-xtable(votetable)
# print(votetable,floating=FALSE)
 
  
####################################################################
######################### Empirical Analysis
#################################################################### 

###Analysis on round 1 of the 10 periods. 


########################## 
#Descriptive statistics
#########################
 with(treat.r1.dfl, table(go1, g.gender))
 
 cor(treat.r1.df$generosityall, treat.r1.df$sp)
 
 cor(as.numeric(treat.r1.df$gender), treat.r1.df$sp)
 cor(as.numeric(treat.r1.df$race), treat.r1.df$sp)

 
 
############################################ 
### Manuscript Models of interest - Table 1
############################################
 
  #DV: Offer  - Definition 1 same group
 lm.M1<-ols(prop1~ideo.diff + same.g + same.r, data=treat.r1.dfl.o, y=T, x=T)
 lm.M1.cl<-robcov(lm.M1, treat.r1.dfl.o$umg)
 lm.M1.cl
 
 
 #DV: Offer  - Definition 1 with controles
 lm.M2<-ols(prop1~ideo.diff + ideo.diff.o + same.g + + same.g.o +same.r+ same.g.o + same.r.o, data=treat.r1.dfl.o, y=T, x=T)
 lm.M2.cl<-robcov(lm.M2, treat.r1.dfl.o$umg)
 lm.M2.cl
 
 
 lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 lm.M3.cl<-robcov(lm.M3, treat.r1.dfl.o$umg)
 lm.M3.cl
 
 
 screenreg(list(lm.M1.cl, lm.M2.cl, lm.M3.cl))
 
 # Partner and vote models
 
 log.p <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, x=T, y=T ) 
 log.p.cl <- robcov(log.p, treat.r1.dfl.o$umg) 
 log.p.cl
 
 
 
 screenreg(log.p.cl)
 
 #DV: Vote  - Basic model with clustered se for correlation  within groups
 log.v1<- lrm(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race  + sp + gender + race2, data=treat.r1.df.o, x=T, y=T)
 log.v1.cl <- robcov(log.v1, treat.r1.df.o$umg)
 log.v1.cl
 
 
 log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=treat.r1.df.o, x=T, y=T)
 log.v2.cl <- robcov(log.v2, treat.r1.df.o$umg)
 log.v2.cl
 
 screenreg(list(log.p.cl, log.v1, log.v2.cl))
 
 
 ### Printing all models as 1 table
 texreg(file= paste0(bd, "base_all_models.tex", sep=""), 
        list(lm.M1.cl, lm.M2.cl, lm.M3.cl, log.p.cl, log.v1.cl, log.v2.cl), 
        custom.model.names = c("M1 Offer", "M2 Offer", "M3 Offer", "M4 Partner", "M5 Vote", "M6 Vote"), 
        custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                              "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White", 
                              "Diff SP P-R", "Same Gender", "Same Race", "Amount Offered" ),
        caption = "Statistical Models on Proposal and Voting Behaviour",
        label="table:base_model",
        booktabs = F, dcolumn = F)
 
 
 #  ### Print Models in 2 separate tables
 # texreg(file= paste0(bd, "base_models.tex", sep=""), 
 #        list(lm.M1.cl, lm.M2.cl, lm.M3.cl), 
 #        custom.model.names = c("M1 Offer", "M2 Offer", "M3 Offer"), 
 #        custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
 #                              "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
 #                              "Self-Placement", "Proposer-Male", "Proposer-White" ),
 #        booktabs = F, dcolumn = F)
 # 
 # 
 # texreg(#file= paste0(bd, "log_models.tex", sep=""), 
 #   list(log.p.cl, log.v1, log.v2.cl), 
 #   custom.model.names = c("M4 Partner", "M5 Vote", "M6 Vote"), 
 #   custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
 #                         "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
 #                         "Self-Placement", "Proposer-Male", "Proposer-White", 
 #                         "Diff SP P-R", "Same Gender", "Same Race", "Amount Offered" ),
 #   booktabs = F, dcolumn = F,
 #   stars=c(0.001, 0.01, 0.05,  0.1))
 # 
 # screenreg(list(lm.M1.cl, lm.M2.cl, lm.M3.cl))
 # 
 
 screenreg(list(log.v1.cl, log.v2.cl))
 
 
 plotreg(lm.M3.cl, file = paste0(bd, "base_model.png", sep=""),
         custom.model.names = "OLS - Amount Offered to Others",
         custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                               "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                               "Self-Placement", "Proposer-Male", "Proposer-White"),
         omit.coef = "Intercept",  ci.level = 0.95)
 
 plotreg(log.p.cl, file = paste0(bd, "partner_model.png", sep=""),
         custom.model.names = "Logit- Partner Selection",
         custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                               "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                               "Self-Placement", "Proposer-Male", "Proposer-White"),
         omit.coef = "Intercept",  ci.level = 0.95)
 
 plotreg(log.v2.cl, file = paste0(bd, "vote_result.png", sep=""),
         custom.model.names = "Logit - Vote",
         custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                               "Self-Placement", "Proposer-Male", "Proposer-White",
                               "Amount Offered" ),
         omit.coef = "Intercept",  ci.level = 0.95)
 
 
 
###########################################
### Matching group statistics -- Figure 4
############################################
 base.r1.dfl.o$mg_id<-as.numeric(factor(base.r1.dfl.o$umg))
 treat.r1.dfl.o$mg_id <- as.numeric(factor(treat.r1.dfl.o$umg))
 treat.dfl.o$mg_id<- as.numeric(factor(treat.dfl.o$umg))
 
 ##### Matching group level statistics, on amount given to others
 #ideo.diff Treatment GRI
 ideo.mg<-rep(NA, 30)
 gender.mg<-rep(NA, 30)
 race.mg<-rep(NA, 30)
 for(i in 1:30){
   lm<-lm(prop1~ideo.diff + same.g + same.r, data=subset(treat.r1.dfl.o, treat.r1.dfl.o$mg_id==i))
   summary(lm)
   ideo.mg[i]<-lm$coef[2]
   gender.mg[i]<-lm$coef[3]
   race.mg[i]<-lm$coef[4]
 }
 ideo.mg
 
 ### Figure 4
 #### Graphs for matching groups
 png(filename= paste0(bd, "ideo_mg.png", sep=""))
 plot(ideo.mg, ylab="Coef. Diff SP P-R", xlab="Matching Group", cex.lab=1.5) + abline(h=0, col="red")# +  theme(text=element_text(size=24))
 dev.off()
 
 mean(ideo.mg)
 sum(ideo.mg<0) # Number of matching group with coeff below 0
 wilcox.test(ideo.mg, data=ideo.diff) # Non-parametric test if mean is equal to zero
 
 
 png(filename= paste0(bd, "gender_mg.png", sep=""))
 plot(gender.mg, ylab="Coef. Same Gender", xlab="Matching Group") + abline(h=0, col="red")
 dev.off()
 wilcox.test(gender.mg, data=ideo.diff) # Non-parametric test if mean is equal to zero
 
 png(filename= paste0(bd, "race_mg.png", sep=""))
 plot(race.mg, ylab="Coef. Same Race", xlab="Matching Group") + abline(h=0, col="red")
 dev.off()
 wilcox.test(race.mg, data=ideo.diff) # Non-parametric test if mean is equal to zero
 
 
 
 ##### Matching group analysis for ideo.diff.o
 ideo.diff.o<-rep(NA, 30)
 for(i in 1:30){
   lm<-lm(prop1~ideo.diff.o + ideo.diff + same.g + same.r, data=subset(treat.r1.dfl.o, treat.r1.dfl.o$mg_id==i))
   summary(lm)
   names(lm)
   ideo.diff.o[i]<-lm$coef[2]
 }
 plot(ideo.diff.o)+ abline(h=0, col="red")
 wilcox.test(ideo.diff.o, data=ideo.diff.o)
 ideo.diff.o<-table(ideo.diff.o)
 sum(ideo.diff.o[names(ideo.diff.o)<0])
 
 ##### Matching group level statistics, on partner selection
 ideo.partner<-rep(NA, 30)
 for(i in 1:30){
   log.M1 <- glm(non.zero ~ ideo.diff + same.g + same.r , data=subset(treat.dfl.o, treat.dfl.o$mg_id==i), family = "binomial") 
   summary(log.M1)
   ideo.partner[i]<-log.M1$coef[2]
 }
 plot(ideo.partner, ylab="Coef. Diff SP P-R", xlab="Matching Group") + abline(h=0, col="red")
 wilcox.test(ideo.partner, data=ideo.partner) 
 
 data=subset(treat.dfl.o, treat.dfl.o$mg_id==i)
 
 
 
  
 
 

 ###############################################
#### Coaliton formation - Table 2
###############################################
 
 
 #Identification of who is closest
 
 id<-rep(NA, nrow(mydf))
 others<-list(rep(NA, 2))
 #Identifying who is closer in gender, race and ideology to the proposer, and who the proposer is offering more to.
 for (i in 1:nrow(mydf)){
   id[i]<-mydf$group_id[i]
   others <- setdiff(1:3, id[i])
   
   if (mydf$treatment[i]=="GRI"){
     mydf$ideo.dist.A[i]<-abs(mydf$sp[i]-mydf[[paste0("spo", others[1], sep="")]][[i]]) 
     mydf$ideo.dist.B[i]<-abs(mydf$sp[i]-mydf[[paste0("spo", others[2], sep="")]][[i]]) 
     var.ideo<-c("ideo.dist.A", "ideo.dist.B")
     mydf$closer.ideo[i]<-ifelse( mydf$ideo.dist.A[i]==mydf$ideo.dist.B[i] , "Equal" ,names(which.min(mydf[i,var.ideo])) )
     
     props.i.want<-paste0("prop.", c("A","B"), sep="")
     mydf$offer.more[i]<-ifelse(mydf$prop.A[i]==mydf$prop.B[i] , "Equal" ,names(which.max(mydf[i,props.i.want])) )
   }
   else {
     mydf$ideo.dist.A[i]<-NA
     mydf$ideo.dist.B[i]<-NA
     mydf$closer.ideo[i]<-NA
     mydf$offer.more[i]<-NA
   }
 }
 
 rm(id, others)
 mydf$closer.ideo[is.na(mydf$offer)]<-NA
 mydf$closer.ideo[mydf$closer.ideo=="ideo.dist.A"]<-"A" #simplification of values
 mydf$closer.ideo[mydf$closer.ideo=="ideo.dist.B"]<-"B"
 
 mydf$offer.more[mydf$offer.more=="prop.A"]<-"A"
 mydf$offer.more[mydf$offer.more=="prop.B"]<-"B"
 
 tmp<-by(mydf, interaction(mydf$ugroup, mydf$period),  function(x) {
   
   c.v<-if ( 0 %in% x$vote) "Minimal" else "Grand"
   x$c.type<-c.v
   x$c.type[x$vote_result=="Rejected"]<-"Rejected"
   x$c.type[is.na(x$prop1)]<-NA
   
   id<-rep(NA, nrow(x))
   others<-list(rep(NA, 2))
   
   for (i in 1:nrow(x)){
     id<-x$group_id[i]
     others <- setdiff(1:3, id)
     x$v.favour[i]<-ifelse(is.na(x$prop1[i]), NA, ifelse(x$vote[others[1]]=="1", "A" , "B"))  
     
   }
   
   x$v.favour[x$c.type=="Grand"]<-"Grand"
   x$v.favour[x$vote_result=="Rejected"]<-"Rejected"
   return(x) 
 })
 
 coalition.df <- do.call("rbind", tmp)
 rm(tmp) 
 
 
 coalition.df<-coalition.df[order(coalition.df$session, coalition.df$period,coalition.df$group, coalition.df$group_id),]
 coalition.df.me<-subset(coalition.df, prop_dummy==1)
 coalition.df.me<-coalition.df.me[, c("treatment", "userperiod", "group_id", "offer", "prop_dummy", "vote", "vote_result", "p.offered", "closer.ideo", "c.type", "v.favour" )]
 coalition.df.me$closer.ideo[is.na(coalition.df.me$vote)]<-NA
 
 ##### Table with coalition partner
 coalition.df.me$coalition.vote<-NA
 coalition.df.me$coalition.vote[coalition.df.me$c.type=="Minimal" & coalition.df.me$closer.ideo!=coalition.df.me$v.favour]<- "Disconnected"
 coalition.df.me$coalition.vote[coalition.df.me$c.type=="Minimal" & coalition.df.me$closer.ideo==coalition.df.me$v.favour]<-"Connected"
 coalition.df.me$coalition.vote[coalition.df.me$c.type=="Minimal" & coalition.df.me$closer.ideo=="Equal"]<-"Equal"
 coalition.df.me$coalition.vote[coalition.df.me$c.type=="Grand"]<-"Grand"
 coalition.df.me$coalition.vote[coalition.df.me$c.type=="Rejected"]<-"Rejected"
 
 #Coalitions for Treatment Group
 coalition.t<-table(coalition.df.me$coalition.vote[coalition.df.me$treatment=="GRI"])
 coalition.t<-coalition.t[1:4]
 prop.t<-prop.table(t(coalition.t), 1)*100
 coalition.t<-t(rbind(coalition.t, prop.t))
 coalition.t
 
 # % minimal group
 coalition.min<-coalition.t[1:3]
 prop.min<-prop.table(t(coalition.min), 1)*100
 prop.min[4]<-"--"
 
 #table
 coalition.t<-cbind(coalition.t, prop.min)
 coalition.t
 
 #Coalitions Control
 coalition.base<-table(coalition.df.me$c.type[coalition.df.me$treatment=="Control"])
 coalition.base<-coalition.base[1:2]
 coalition.base.p<-prop.table(t(coalition.base), 1)
 coalition.base.p
 
 coalition.b <- matrix(nrow = 4, ncol = 2)
 coalition.b[2, 1]<-coalition.base[1]
 coalition.b[4, 1]<-coalition.base[2]
 coalition.b[2, 2]<-coalition.base.p[1]*100
 coalition.b[4, 2]<-coalition.base.p[2]*100
 coalition.b<-as.table(coalition.b)
 
 main.coalition<-cbind(coalition.t, coalition.b)
 main.coalition
 
 colnames(main.coalition) <- c( "Number", "%", "%", "Number", "%")
 rownames(main.coalition)<-c("Minimal-Connected", "Minimal-Disconnected", "Minimal-Equal", "Grand")
 main.coalition
 xtable(main.coalition, caption="Types of Coalitions", label="table:coalitions")
 
 
 
 
###############################
#### Love - Hate - Figure 5
############################################ 
 #Offers by ideological distance  - Treatment
 
 plot.df<-ddply(treat.r1.dfl.o, .(ideo.diff), summarise,
                prop = mean(prop1)
 )
 
 p <- ggplot(plot.df, aes(ideo.diff, prop)) + theme_bw()
 p<- p + geom_point() + labs(x = "Absolute value ideological distance", y= "Mean offer") + theme(text=element_text(size=24))
 p<- p + geom_hline(yintercept=mean(treat.r1.dfl.o$prop1),linetype="dashed", color = "red") + geom_hline(yintercept=mean(base.dfl.o$prop1, na.rm=T), color = "blue")
 p
 
 
 ggsave(filename=paste0(bd, "mean_offers_ideo_treat.png", sep=""),  width=6, height=6)
 
 
 
 #Offers by ideological distance  - Baseline
 sp<-qplot(ideo.diff, prop1, data = base.dfl.o, geom = c("point", "smooth"),
           method="lm", ylab="pounds",
           xlab="Absolute value ideological distance" ,main="Offers to other participants - Baseline ",  
           na.rm=TRUE)
 sp<-sp + geom_hline(yintercept=mean(treat.r1.dfl.o$prop1),linetype="dashed", color = "red")
 
 sp<-sp + geom_hline(yintercept=mean(base.dfl.o$prop1, na.rm=T), color = "darkgreen") + theme_bw()
 
 sp    
 ggsave(filename=paste0(bd, "offers_ideo_base.png", sep=""),  width=6, height=6) 
 
 
 
 
 
 
 
 
 ####################################
 #####  Appendix  - Robustness tests
 ####################################
# Table A.1 - generated manually bu the authors
 

 
 
 ############################################  
 ##### Characteristics of the sample - Table A.2
 ############################################
 
 ### Mean, Min and Max earnings, commented in text
 f.earnings<-mydf$final_earnings[mydf$userperiod==0] ##0 GRI and Baseline
 summary(f.earnings)
 
 
 ###Gender and Race Composition of treatment group  - Table A.2
 t.gr <- table(mydf$gender[mydf$userperiod==0 & mydf$treatment=="GRI"], mydf$race[mydf$userperiod==0 & mydf$treatment=="GRI"] ) # A will be rows, B will be columns 
 t.gr # print table - Table in Appendix
 #latex(t.gr, caption="Frequences of Gender and Race in treatment sample ", file=paste0(bd, "t_gr", ".tex", sep="") )
 
 
 #Gender
 gender_GRI<-mydf$gender[mydf$userperiod==0 & mydf$treatment=="GRI"]
 #gender_2d<-mydf.2d.$gender[mydf.2d.$period==1 ]
 #tot.gender<-c(as.character(gender_GRI), as.character(gender_2d))
 #g<-table(tot.gender)
 #g
 #prop.table(g) # cell percentages
 
 #Race
 race_GRI<-mydf$race2[mydf$userperiod==0 & mydf$treatment=="GRI"]
 #race_2d<-mydf.2d.$race2[mydf.2d.$period==1 ]
 #tot.race<-c(as.character(race_GRI), as.character(race_2d))
 #r <-table(tot.race) 
 #r
 #prop.table(r)
 
 #Nationalities in Main treatment and 2Dict samples
 nation<-mydf$nationality[mydf$userperiod==0] # Main tratment
 #nation.2d<-mydf.2d.$nationality[mydf.2d.$userperiod==1] # 2dictator
 #nation<-c(as.character(nation), as.character(nation.2d))
 table(nation) # Table with nationalities included in the sample
 
 
 
 
 #############################
 ### Pro-Social types
 #############################
 treat.r1.dfl.o$prosocial<-NA
 treat.r1.dfl.o$prosocial[treat.r1.dfl.o$generosityall>0]<-1
 treat.r1.dfl.o$prosocial[treat.r1.dfl.o$generosityall==0]<-0
 describe(treat.r1.dfl.o$prosocial)
 table(treat.r1.dfl.o$generosityall[unique(treat.r1.dfl.o$uid)])
 table(treat.r1.dfl.o$prosocial[unique(treat.r1.dfl.o$uid)])
 
 
 #DV: Offer  - M2 basic covariates + 3er player controles 
 lm.M1.ps<-ols(prop1~ideo.diff*prosocial + ideo.diff.o*prosocial + same.g*prosocial + same.g.o*prosocial +same.r*prosocial+ same.g.o*prosocial + same.r.o*prosocial, data=treat.r1.dfl.o, y=T, x=T)
 lm.M1.ps.cl<-robcov(lm.M1.ps, treat.r1.dfl.o$umg)
 lm.M1.ps.cl
 
 
 #DV: Offer  - M3 basic covariates + 3er player controles + proposer characteristics
 lm.M2.ps<-ols(prop1~ideo.diff*prosocial + ideo.diff.o*prosocial + same.g*prosocial + same.g.o*prosocial +same.r*prosocial+ same.g.o*prosocial + same.r.o*prosocial + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 lm.M2.ps.cl<-robcov(lm.M2.ps, treat.r1.dfl.o$umg)
 lm.M2.ps.cl 
 
 screenreg(list(lm.M1.ps.cl, lm.M2.ps.cl))
 
 
 #############################################
 ######Comparison to Baseline and placebo tests
 #############################################
 
 var.treat<-treat.r1.df.o$sd.p.offered[treat.r1.df.o$vote_result=="Accepted"]
 var.base<-base.r1.df.o$sd.p.offered[base.r1.df.o$vote_result=="Accepted"]
 
 var.test(var.treat,var.base)
 t.test(var.treat,var.base)
 
 
 #DV: Offer  - A.M1 placebo only basic covariates
 p.lm.M1<-ols(prop1~ideo.diff + same.g + same.r, data=base.r1.dfl.o, y=T, x=T)
 p.lm.M1.cl<-robcov(p.lm.M1, base.r1.dfl.o$umg)
 
 #DV: Offer  - A.M2 placebo basic covariates + 3er player controles 
 p.lm.M2<-ols(prop1~ideo.diff + ideo.diff.o + same.g + + same.g.o +same.r+ same.g.o + same.r.o, data=base.r1.dfl.o, y=T, x=T)
 p.lm.M2.cl<-robcov(p.lm.M2, base.r1.dfl.o$umg)
 
 #DV: Offer  - A.M3 placebo basic covariates + 3er player controles + proposer characteristics
 p.lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=base.r1.dfl.o, y=T, x=T)
 p.lm.M3.cl<-robcov(p.lm.M3, base.r1.dfl.o$umg)
 
 ##### Partner and vote models
 
 #### DV: A.M4 placebo Non-Zero offer ==1 Partner Selection 
 #p.log.p <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=base.r1.dfl.o, x=T, y=T ) 
 #p.log.p.cl <- robcov(p.log.p, base.r1.dfl.o$umg) 
 
 #DV: Vote  - A.M5 placebo Model on acceptance of offers 
 p.log.v1<- lrm(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race  + sp + gender + race2, data=base.r1.df.o, x=T, y=T)
 p.log.v1.cl <- robcov(p.log.v1, base.r1.df.o$umg)
 
 
 #DV: Vote  - A.M6  placebo Model on acceptance of offers accounting for amount offered 
 p.log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=base.r1.df.o, x=T, y=T)
 p.log.v2.cl <- robcov(p.log.v2, base.r1.df.o$umg)
 
 ########################################### 
 ############### Table Placebo #############
 ########################################### 
 
 # screenreg(list(p.lm.M1.cl, p.lm.M2.cl, p.lm.M3.cl, p.log.p.cl, p.log.v1.cl, p.log.v2.cl))
 # 
 # 
 # ### Printing all models as 1 table
 # texreg(file= paste0(bd, "placebo_all_models.tex", sep=""), 
 #        list(lm.M1.cl, lm.M2.cl, lm.M3.cl, log.p.cl, log.v1.cl, log.v2.cl), 
 #        custom.model.names = c("A.M1 Offer placebo", "A.M2 Offer placebo", "A.M3 Offer placebo", "A.M4 Partner placebo", 
 #                               "A.M5 Vote placebo", "A.M6 Vote placebo"), 
 #        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "Same Race", 
 #                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
 #                              "Self-Placement", "Proposer-Male", "Proposer-White", 
 #                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered" ),
 #        caption = "Regression models on amount offered to other participant (Offer), whether a 
 #        participant was chosen as coalition partner by giving more than zero (Partner), and whether 
 #        a participant chose to accept the offer they received (Vote).",
 #        label="table:base_model",
 #        booktabs = F, dcolumn = F)
 
 
 
 
 
 
 ###########################
 ### Panel Specification Table A.3
 ############################
 
 #Fixed Effects Models on proposal
 fe1 <- plm(prop1~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o, 
            data=treat.r1.dfl.o, index=c("uid", "plm.time"), model="within")
 
 summary(fe1)
 fe1.pse<-coeftest(fe1, vcov=vcovHC(fe1, method="arellano"))
 fe1.pse
 
 
 #Fixed Effects Models on proposal
 fe2 <- plm(prop1~ideo.diff +  same.g + same.r, 
            data=treat.r1.dfl.o, index=c("uid", "plm.time"), model="within")
 
 summary(fe2)
 fe2.pse<-coeftest(fe2, vcov=vcovHC(fe1, method="arellano"))
 fe2.pse
 
 
 # Conditional Logit models on partner and vote
 
 #partner
 cl1 <- clogit(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o 
               + strata(uid),  
               data=treat.r1.dfl.o)
 summary(cl1)
 #vote
 cl2 <- clogit(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race 
               + strata(uid),
               data=treat.r1.df.o)
 summary(cl2)
 
 
 cl3 <- clogit(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race + p.offered
               + strata(uid),
               data=treat.r1.df.o)
 summary(cl3)
 
 #### FE Models
 screenreg(list(fe2, fe1, cl1, cl2, cl3))
 
 
 ### Printing all models as 1 table
 texreg(file= paste0(bd, "FE_all_models.tex", sep=""), 
        list(fe2.pse, fe1.pse, cl1, cl2, cl3), 
        custom.model.names = c("A.M1 Offer FE", "A.M2 Offer FE", "A.M3 Partner C.logit", "A.M4 Vote C.logit", "A.M5 Vote C.logit"), 
        custom.coef.names = c("Ideo. Dist. P-R", "Same Gender", "Same Race", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered" ),
        caption = "Fixed effects regression models on amount offered to other participant (Offer) and conditional logit models
        on whether a participant was chosen as coalition partner by giving more than zero (Partner), and whether 
        a participant chose to accept the offer they received (Vote).",
        label="table:base_model",
        booktabs = F, dcolumn = F) 
 
 
 ########################
 ###### Effect of Time Table A.4
 ########################
 
 treat.r1.dfl.o.05<-subset(treat.r1.dfl.o, userperiod<6)
 treat.r1.dfl.o.610<-subset(treat.r1.dfl.o, userperiod>=6)
 treat.r1.df.o.05<-subset(treat.r1.df.o, userperiod<6)
 treat.r1.df.o.610<-subset(treat.r1.df.o, userperiod>=6)
 
 
 #DV: Offer  - M3 basic covariates + 3er player controles + proposer characteristics
 t1.lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.05, y=T, x=T)
 t1.lm.M3.cl<-robcov(t1.lm.M3, treat.r1.dfl.o.05$umg)
 
 t2.lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.610, y=T, x=T)
 t2.lm.M3.cl<-robcov(t2.lm.M3, treat.r1.dfl.o.610$umg)
 
 
 
 ##### Partner and vote models
 
 #### DV: M4 Non-Zero offer ==1 Partner Selection 
 t1.log.p <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.05, x=T, y=T ) 
 t1.log.p.cl <- robcov(t1.log.p, treat.r1.dfl.o.05$umg) 
 
 t2.log.p <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.610, x=T, y=T ) 
 t2.log.p.cl <- robcov(t2.log.p, treat.r1.dfl.o.610$umg) 
 
 
 #DV: Vote  - M6  Model on acceptance of offers accounting for amount offered 
 t1.log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=treat.r1.df.o.05, x=T, y=T)
 t1.log.v2.cl <- robcov(t1.log.v2, treat.r1.df.o.05$umg)
 
 t2.log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=treat.r1.df.o.610, x=T, y=T)
 t2.log.v2.cl <- robcov(t2.log.v2, treat.r1.df.o.610$umg)
 
 rm(treat.r1.dfl.o.05, treat.r1.dfl.o.610, treat.r1.df.o.05, treat.r1.df.o.610 )
 
 
 ########################################### 
 ############### Table Time ################
 ########################################### 
 
 screenreg(list(t1.lm.M3.cl, t2.lm.M3.cl, t1.log.p.cl, t2.log.p.cl, t1.log.v2.cl, t2.log.v2.cl))
 
 
 ### Printing all models as 1 table
 texreg(file= paste0(bd, "time_all_models.tex", sep=""), 
        list(t1.lm.M3.cl, t2.lm.M3.cl, t1.log.p.cl, t2.log.p.cl, t1.log.v2.cl, t2.log.v2.cl), 
        custom.model.names = c("A.M3 Offer p0-5", "A.M3 Offer p6-10", "A.M4 Partner p0-5", 
                               "A.M4 Partner  p6-10", "A.M6 Vote p0-5", "A.M6 Vote  p6-10"), 
        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "Same Race", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White", 
                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered" ),
        caption = "Regression models on amount offered to other participant (Offer), whether a 
        participant was chosen as coalition partner by giving more than zero (Partner), and whether 
        a participant chose to accept the offer they received (Vote). The table presents results for
        periods 0--5 and 6--10, to account for learning effects.",
        label="table:time_model",
        booktabs = F, dcolumn = F)
 
 
 ###########################################
 
 #### Interaction with mixed gender and race
 #### Subsets of data with and without
 #### Mixed gender and race groups Table A.5
 ######################################
 
 treat.r1.dfl.o.g<-subset(treat.r1.dfl.o, g.gender>0 & g.gender<3)
 treat.r1.dfl.o.r<-subset(treat.r1.dfl.o, g.race>0 & g.race<3)
 treat.r1.df.o.g<-subset(treat.r1.df.o, g.gender>0 & g.gender<3)
 treat.r1.df.o.r<-subset(treat.r1.df.o, g.race>0 & g.race<3)
 
 #### Interaction models
 #DV: Offer  - M3 basic covariates + 3er player controles + proposer characteristics
 int.lm.M3<-ols(prop1~ideo.diff + same.g*g.gender + same.r*g.race + ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 int.lm.M3.cl<-robcov(int.lm.M3, treat.r1.dfl.o$umg)
 
 
 #Visualization
 #lp <- ggplot(data=treat.r1.dfl.o, aes(x=g.gender, y=prop1, shape=factor(same.g), 
 #                            colour=factor(same.g))) + stat_smooth(method="lm") + geom_point()
 #lp
 
 
 #### Only mixed gender groups
 ##### Partner and vote models
 # DV: M4 Non-Zero offer ==1 Partner Selection 
 int.log.p <- lrm(non.zero ~ ideo.diff+  same.g*g.gender + same.r*g.race + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, x=T, y=T ) 
 int.log.p.cl <- robcov(int.log.p, treat.r1.dfl.o$umg) 
 
 #DV: Vote  - M6  Model on acceptance of offers accounting for amount offered 
 int.log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender*g.gender + prop.same.race*g.race + sp + gender + race2 + p.offered, data=treat.r1.df.o, x=T, y=T)
 int.log.v2.cl <- robcov(int.log.v2, treat.r1.df.o$umg)
 
 screenreg(list(int.lm.M3.cl, int.log.p.cl, int.log.v2.cl))
 
 ### Printing interaction with gender and race composition of groups
 texreg(file= paste0(bd, "int_gender_race_models.tex", sep=""), 
        list(int.lm.M3.cl, int.log.p.cl, int.log.v2.cl), 
        custom.model.names = c("A.M3 Offer Int group type", "A.M4 Partner Int group type"
                               , "A.M6 Vote Int group type"), 
        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "N.Female",  
                              "Same Race", "N. White", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White", 
                              "S.Gender x N.Female", "S.Race x N. White",
                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered",
                              "S.Gender x N.Female", "S.Race x N. White"),
        reorder.coef=c( 1, 2, 3, 4, 13, 5, 6, 14, 7, 8, 9, 10, 11, 12, 15),
        caption = "Regression models on amount offered to other participant (Offer), whether a 
        participant was chosen as coalition partner by giving more than zero (Partner), and whether 
        a participant chose to accept the offer they received (Vote). The table presents results of interactions
        between same gender and the number of women in a group and same race and a number of white participants
        in the group.",
        label="table:int_gender_race_models",
        booktabs = F, dcolumn = F)
 
 
 #### Only mixed gender groups
 #DV: Offer  - M3 basic covariates + 3er player controles + proposer characteristics
 g.lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.g, y=T, x=T)
 g.lm.M3.cl<-robcov(g.lm.M3, treat.r1.dfl.o.g$umg)
 
 ##### Partner and vote models
 # DV: M4 Non-Zero offer ==1 Partner Selection 
 g.log.p <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.g, x=T, y=T ) 
 g.log.p.cl <- robcov(g.log.p, treat.r1.dfl.o.g$umg) 
 
 #DV: Vote  - M6  Model on acceptance of offers accounting for amount offered 
 g.log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=treat.r1.df.o.g, x=T, y=T)
 g.log.v2.cl <- robcov(g.log.v2, treat.r1.df.o.g$umg)
 
 screenreg(list(g.lm.M3.cl, g.log.p.cl, g.log.v2.cl))
 
 ### Printing mixed gender models 
 texreg(#file= paste0(bd, "m_gender_models.tex", sep=""), 
   list(g.lm.M3.cl, g.log.p.cl, g.log.v2.cl), 
   custom.model.names = c("A.M3 Offer Mix gender", "A.M4 Partner  Mix gender"
                          , "A.M6 Vote Mix gender"), 
   custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "Same Race", 
                         "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                         "Self-Placement", "Proposer-Male", "Proposer-White", 
                         "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered" ),
   caption = "Regression models on amount offered to other participant (Offer), whether a 
   participant was chosen as coalition partner by giving more than zero (Partner), and whether 
   a participant chose to accept the offer they received (Vote). The table presents results a subset of
   the data that only includes mixed gender groups.",
   label="table:m_gender_model",
   booktabs = F, dcolumn = F)
 
 #### Only mixed race groups
 #DV: Offer  - M3 basic covariates + 3er player controles + proposer characteristics
 r.lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.r, y=T, x=T)
 r.lm.M3.cl<-robcov(r.lm.M3, treat.r1.dfl.o.r$umg)
 
 
 ##### Partner and vote models
 # DV: M4 Non-Zero offer ==1 Partner Selection 
 r.log.p <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.r1.dfl.o.r, x=T, y=T ) 
 r.log.p.cl <- robcov(r.log.p, treat.r1.dfl.o.r$umg) 
 
 
 #DV: Vote  - M6  Model on acceptance of offers accounting for amount offered 
 r.log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=treat.r1.df.o.r, x=T, y=T)
 r.log.v2.cl <- robcov(r.log.v2, treat.r1.df.o.r$umg)
 
 rm(treat.r1.dfl.o.g, treat.r1.dfl.o.r, treat.r1.df.o.g, treat.r1.df.o.r)
 screenreg(list(r.lm.M3.cl, r.log.p.cl, r.log.v2.cl))
 
 
 ### Printing mixed race models 
 texreg(#file= paste0(bd, "m_race_models.tex", sep=""), 
   list(r.lm.M3.cl, r.log.p.cl, r.log.v2.cl), 
   custom.model.names = c("A.M3 Offer Mix gender", "A.M4 Partner  Mix gender"
                          , "A.M6 Vote Mix gender"), 
   custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "Same Race", 
                         "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                         "Self-Placement", "Proposer-Male", "Proposer-White", 
                         "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered" ),
   caption = "Regression models on amount offered to other participant (Offer), whether a 
   participant was chosen as coalition partner by giving more than zero (Partner), and whether 
   a participant chose to accept the offer they received (Vote). The table presents results a subset of
   the data that only includes mixed race groups.",
   label="table:m_race_model",
   booktabs = F, dcolumn = F)
 
 
 ########################### 
 ######## Bootstraping Figures A.2-A.6
 ###########################
 
 c.data<-treat.r1.dfl.o[, c( "prop1", "non.zero",  "ideo.diff", "ideo.diff.o", "same.g", "same.r", "same.g.o", "same.r.o", "sp",
                             "gender", "race2", "umg")]
 c.data$mg_id<- as.numeric(factor(c.data$umg))
 
 
 
 #Bootstrapped coefficients for Amount Offered (M3)
 boot_mg_lm<-function(R, eq){
   
   var.1<-rep(NA, R)
   var.2<-rep(NA, R)
   var.3<-rep(NA, R)
   var.4<-rep(NA, R)
   var.5<-rep(NA, R)
   var.6<-rep(NA, R)
   
   for (i in 1:R){
     # get a vector with all clusters
     c <- sort(unique(c.data$mg_id))
     
     # group the data points per cluster
     clust.group <- function(c) {
       c.data[c.data$mg_id==c,]
     }
     
     clust.list <- lapply(c,clust.group)
     
     # resample clusters with replacement
     c.sample <- sample(c, replace=T)
     
     clust.sample <- clust.list[c.sample]
     
     clust.size <- 120
     
     # combine the cluster list back to a single data matrix
     clust.bind <- function(c) {
       matrix(unlist(c),nrow=clust.size)
     }
     
     c.boot <- do.call(rbind,lapply(clust.sample,clust.bind))
     
     # Just to maintain columns name
     colnames(c.boot) <- names(c.data)
     
     c.boot<-as.data.frame(c.boot)
     # the new data set (single bootstrap replicate)
     c.boot$prop1<-as.numeric(c.boot$prop1)
     c.boot$ideo.diff<-as.numeric(c.boot$ideo.diff)
     c.boot$ideo.diff.o<-as.numeric(c.boot$ideo.diff.o)
     c.boot$sp<-as.numeric(c.boot$sp)
     
     lm<-lm(eq,  c.boot)
     var.1[i]<-lm$coef[2]
     var.2[i]<-lm$coef[3]
     var.3[i]<-lm$coef[4]
     var.4[i]<-lm$coef[5]
     var.5[i]<-lm$coef[6]
     var.6[i]<-lm$coef[7]
     results<-cbind(var.1,var.2, var.3, var.4,var.5, var.6 )
   }
   
   return(results)
 }
 
 results<-boot_mg_lm(10000, eq=prop1~ideo.diff + ideo.diff.o + same.g  + same.g.o + same.r + same.r.o + sp + gender + race2)
 
 coef.ideo.diff<-results[,1]
 coef.ideo.diff.o<-results[, 2]
 
 png(filename=paste0(bd, "boot_ideodiff_mg.png", sep=""))
 mean<-mean(coef.ideo.diff)
 sd<-sd(coef.ideo.diff)
 ll<-mean-1.96*sd
 ul<-mean+1.96*sd
 hist(coef.ideo.diff, freq=F, xlab="Bootstrapped Coef. for Ideo. Dist.  P-R", main="")
 abline(v=ll, col = "red", lwd = 1)
 abline(v=ul, col = "red", lwd = 1)
 dev.off() 
 
 
 
 png(filename=paste0(bd, "boot_ideodiff3rd_mg.png", sep=""))
 mean<-mean(coef.ideo.diff.o)
 sd<-sd(coef.ideo.diff.o)
 ll.o<-mean-1.96*sd
 ul.o<-mean+1.96*sd
 hist(coef.ideo.diff.o, freq=F, xlab="Bootstrapped Coef. for Ideo. Dist. P-3rd", main="")
 abline(v=ll.o, col = "red", lwd = 1)
 abline(v=ul.o, col = "red", lwd = 1)
 dev.off()  
 
 
 
 #### Bootstrapped coefficients for Partner Models (M4)
 
 boot_mg_glm<-function(R, eq){
   
   var.1<-rep(NA, R)
   var.2<-rep(NA, R)
   var.3<-rep(NA, R)
   var.4<-rep(NA, R)
   var.5<-rep(NA, R)
   var.6<-rep(NA, R)
   
   for (i in 1:R){
     # get a vector with all clusters
     c <- sort(unique(c.data$mg_id))
     
     # group the data points per cluster
     clust.group <- function(c) {
       c.data[c.data$mg_id==c,]
     }
     
     clust.list <- lapply(c,clust.group)
     
     # resample clusters with replacement
     c.sample <- sample(c, replace=T)
     
     clust.sample <- clust.list[c.sample]
     
     clust.size <- 120
     
     # combine the cluster list back to a single data matrix
     clust.bind <- function(c) {
       matrix(unlist(c),nrow=clust.size)
     }
     
     c.boot <- do.call(rbind,lapply(clust.sample,clust.bind))
     
     # Just to maintain columns name
     colnames(c.boot) <- names(c.data)
     
     c.boot<-as.data.frame(c.boot)
     # the new data set (single bootstrap replicate)
     c.boot$prop1<-as.numeric(c.boot$prop1)
     c.boot$ideo.diff<-as.numeric(c.boot$ideo.diff)
     c.boot$ideo.diff.o<-as.numeric(c.boot$ideo.diff.o)
     c.boot$sp<-as.numeric(c.boot$sp)
     
     glm<-glm(eq,  c.boot, family = "binomial")
     var.1[i]<-glm$coef[2]
     var.2[i]<-glm$coef[3]
     var.3[i]<-glm$coef[4]
     var.4[i]<-glm$coef[5]
     var.5[i]<-glm$coef[6]
     var.6[i]<-glm$coef[7]
     results.glm<-cbind(var.1,var.2, var.3, var.4,var.5, var.6 )
   }
   
   return(results.glm)
 }
 
 results.glm<-boot_mg_glm(10000, eq=non.zero~ideo.diff + ideo.diff.o + same.g  + same.g.o + same.r + same.r.o + sp + gender + race2)
 
 
 coef.glm.ideo.diff<-results.glm[,1]
 coef.glm.ideo.diff.o<-results.glm[, 2]
 coef.glm.gender<-results.glm[, 3]
 
 png(filename=paste0(bd, "boot_p_ideodiff_mg.png", sep=""))
 mean<-mean(coef.glm.ideo.diff)
 sd<-sd(coef.glm.ideo.diff)
 ll<-mean-1.96*sd
 ul<-mean+1.96*sd
 hist(coef.glm.ideo.diff, freq=F, xlab="Bootstrapped Coef. for Ideo. Dist. P-R", main="")
 abline(v=ll, col = "red", lwd = 1)
 abline(v=ul, col = "red", lwd = 1)
 dev.off() 
 
 
 
 png(filename=paste0(bd, "boot_p_ideodiff3rd_mg.png", sep=""))
 mean<-mean(coef.glm.ideo.diff.o)
 sd<-sd(coef.glm.ideo.diff.o)
 ll.o<-mean-1.96*sd
 ul.o<-mean+1.96*sd
 hist(coef.glm.ideo.diff.o, freq=F, xlab="Bootstrapped Coef. for Ideo. Dist. P-3rd", main="")
 abline(v=ll.o, col = "red", lwd = 1)
 abline(v=ul.o, col = "red", lwd = 1)
 dev.off()  
 
 
 
 
 png(filename=paste0(bd, "boot_p_sgender_mg.png", sep=""))
 mean<-mean(coef.glm.gender)
 sd<-sd(coef.glm.gender)
 ll<-mean-1.96*sd
 ul<-mean+1.96*sd
 hist(coef.glm.gender, freq=F, xlab="Bootstrapped Coef. for Same Gender", main="")
 abline(v=ll, col = "red", lwd = 1)
 abline(v=ul, col = "red", lwd = 1)
 dev.off() 
 
 #####
  ##############################################
 #### Model with Disaggregated Race Categories A.6
 ##############################################
 lm.M1.race<-ols(prop1~ideo.diff + same.g + same.r.all, data=treat.r1.dfl.o, y=T, x=T)
 lm.M1.race.cl<-robcov(lm.M1.race, treat.r1.dfl.o$umg)
 lm.M1.race.cl
 
 lm.M2.race<-ols(prop1~ideo.diff + ideo.diff.o + same.g + + same.g.o +same.r.all+ same.g.o + same.r.o.all, data=treat.r1.dfl.o, y=T, x=T)
 lm.M2.race.cl<-robcov(lm.M2.race, treat.r1.dfl.o$umg)
 lm.M2.race.cl
 
 lm.M3.race<-ols(prop1~ideo.diff + same.g+same.r.all+ ideo.diff.o + same.g.o + same.r.o.all + sp + gender + race , data=treat.r1.dfl.o, y=T, x=T)
 lm.M3.race.cl<-robcov(lm.M3.race, treat.r1.dfl.o$umg)
 lm.M3.race.cl
 
 log.p.race <- lrm(non.zero ~ ideo.diff+  same.g + same.r.all + ideo.diff.o+ same.g.o +same.r.o.all + sp + gender + race , data=treat.r1.dfl.o, x=T, y=T ) 
 log.p.race.cl <- robcov(log.p.race, treat.r1.dfl.o$umg) 
 log.p.race.cl
 
 
 log.v1.race<- lrm(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race.all  + sp + gender + race, data=treat.r1.df.o, x=T, y=T)
 log.v1.race.cl <- robcov(log.v1.race, treat.r1.df.o$umg)
 log.v1.race.cl
 
 
 log.v2.race<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race.all + sp + gender + race + p.offered, data=treat.r1.df.o, x=T, y=T)
 log.v2.race.cl <- robcov(log.v2.race, treat.r1.df.o$umg)
 log.v2.race.cl
 
 
 
 texreg(file= paste0(bd, "base_raceall_models.tex", sep=""), 
        list(lm.M1.race.cl, lm.M2.race.cl, lm.M3.race.cl, log.p.race.cl, log.v1.race.cl, log.v2.race.cl), 
        custom.model.names = c("M1 Offer", "M2 Offer", "M3 Offer", "M4 Partner", "M5 Vote", "M6 Vote"), 
        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "Same Race All", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd All", 
                              "Self-Placement", "Proposer-Male", "Proposer-Brown" ,"Proposer-White", 
                              "Ideo. Dist. P-R", "Same Gender", "Same Race All", "Amount Offered" ),
        caption = "Statistical Models on proposal and voting behavior using a disaggregated race category",
        label="table:base_race_model",
        booktabs = F, dcolumn = F)
 
 ###########################
 ######## Count Models
 ###########################
 
 library(MASS)
 
 #describe(treat.r1.dfl.o$prop1.p)
 
 treat.r1.dfl.o$prop1.p<-treat.r1.dfl.o$prop1*100
 poisson1 <- glm(prop1.p~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o + sp + gender + race2, data= treat.r1.dfl.o, family=poisson)
 summary(poisson1)
 
 
 negbin1 <- glm.nb(prop1.p~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o + sp + gender + race2, data= treat.r1.dfl.o)
 negbin1 # if the theta is stat sign then the data is overdispersed
 
 
 
 ########################################## 
 ### Models with interaction g.sp.sd*same.Table g A.8
 ##########################################
 #DV: Offer  - Definition 1 same group
 lm.M1.sp<-ols(prop1~ ideo.diff + same.g*g.sp.sd + same.r, data=treat.r1.dfl.o, y=T, x=T)
 lm.M1.sp.cl<-robcov(lm.M1.sp, treat.r1.dfl.o$umg)
 lm.M1.sp.cl
 
 
 #DV: Offer  - Definition 1 with controles
 lm.M2.sp<-ols(prop1~ ideo.diff + same.g*g.sp.sd + ideo.diff.o + same.g.o +same.r+ same.g.o + same.r.o, data=treat.r1.dfl.o, y=T, x=T)
 lm.M2.sp.cl<-robcov(lm.M2.sp, treat.r1.dfl.o$umg)
 lm.M2.sp.cl
 
 lm.M3.sp<-ols(prop1~ideo.diff + same.g*g.sp.sd + ideo.diff.o + same.g.o +same.r+ same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 lm.M3.sp.cl<-robcov(lm.M3.sp, treat.r1.dfl.o$umg)
 lm.M3.sp.cl
 
 
 # Partner and vote models
 
 log.p.sp <- lrm(non.zero ~ ideo.diff + same.g*g.sp.sd + ideo.diff.o + same.g.o +same.r+ same.g.o + same.r.o + sp + gender + race2, data=treat.r1.dfl.o, x=T, y=T ) 
 log.p.sp.cl <- robcov(log.p.sp, treat.r1.dfl.o$umg) 
 log.p.sp.cl
 
 #DV: Vote  - Basic model with clustered se for correlation  within groups
 log.v1.sp<- lrm(vote ~  prop.ideo.diff +   prop.same.gender*g.sp.sd + prop.same.race  + sp + gender + race2, data=treat.r1.df.o, x=T, y=T)
 log.v1.sp.cl <- robcov(log.v1.sp, treat.r1.df.o$umg)
 log.v1.sp.cl
 
 
 log.v2.sp<- lrm(vote ~  prop.ideo.diff +   prop.same.gender*g.sp.sd + prop.same.race  + sp + gender + race2 + p.offered, data=treat.r1.df.o, x=T, y=T)
 log.v2.sp.cl <- robcov(log.v2.sp, treat.r1.df.o$umg)
 log.v2.sp.cl
 
 screenreg(list(lm.M1.sp.cl, lm.M2.sp.cl, lm.M3.sp.cl, log.p.sp.cl, log.v1.sp.cl, log.v2.sp.cl))
 ### Printing all models as 1 table
 texreg(file= paste0(bd, "Int_gender_models.tex", sep=""), 
        list(lm.M1.sp.cl, lm.M2.sp.cl, lm.M3.sp.cl, log.p.sp.cl, log.v1.sp.cl, log.v2.sp.cl), 
        custom.model.names = c("M1 Group SP*Gender", "M2 Group SP*Gender", "M3 Group SP*Gender", "M4 Group SP*Gender", "M5 Group SP*Gender", "M6 Group SP*Gender"), 
        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "SD SP Group" , "Same Race",
                              "Same Gender * SD SP Group", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White",  
                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Same Gender * SD SP Group", "Amount Offered" ),
        caption = "Statistical models on proposal and voting behaviour with interaction of Gender and Standard Deviation of Self-Placement in Groups ",
        label="table:model_intgender",
        booktabs = F, dcolumn = F)
 
 
 
 ########################################## 
 ### Models with interaction g.sp.sd*same.r Table A.9
 ##########################################
 #DV: Offer  - Definition 1 same group
 lm.M1.spr<-ols(prop1~ ideo.diff + same.g + same.r*g.sp.sd, data=treat.r1.dfl.o, y=T, x=T)
 lm.M1.spr.cl<-robcov(lm.M1.spr, treat.r1.dfl.o$umg)
 lm.M1.spr.cl
 
 
 #DV: Offer  - Definition 1 with controles
 lm.M2.spr<-ols(prop1~ ideo.diff + same.g + ideo.diff.o + same.g.o +same.r*g.sp.sd+ same.g.o + same.r.o, data=treat.r1.dfl.o, y=T, x=T)
 lm.M2.spr.cl<-robcov(lm.M2.spr, treat.r1.dfl.o$umg)
 lm.M2.spr.cl
 
 lm.M3.spr<-ols(prop1~ideo.diff + same.g + ideo.diff.o + same.g.o +same.r*g.sp.sd+ same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 lm.M3.spr.cl<-robcov(lm.M3.spr, treat.r1.dfl.o$umg)
 lm.M3.spr.cl
 
 
 # Partner and vote models
 
 log.p.spr <- lrm(non.zero ~ ideo.diff + same.g + ideo.diff.o + same.g.o +same.r*g.sp.sd+ same.g.o + same.r.o + sp + gender + race2, data=treat.r1.dfl.o, x=T, y=T ) 
 log.p.spr.cl <- robcov(log.p.spr, treat.r1.dfl.o$umg) 
 log.p.spr.cl
 
 #DV: Vote  - Basic model with clustered se for correlation  within groups
 log.v1.spr<- lrm(vote ~  prop.ideo.diff +   prop.same.gender + prop.same.race*g.sp.sd  + sp + gender + race2, data=treat.r1.df.o, x=T, y=T)
 log.v1.spr.cl <- robcov(log.v1.spr, treat.r1.df.o$umg)
 log.v1.spr.cl
 
 
 log.v2.spr<- lrm(vote ~  prop.ideo.diff +   prop.same.gender + prop.same.race*g.sp.sd  + sp + gender + race2 + p.offered, data=treat.r1.df.o, x=T, y=T)
 log.v2.spr.cl <- robcov(log.v2.spr, treat.r1.df.o$umg)
 log.v2.spr.cl
 
 screenreg(list(lm.M1.spr.cl, lm.M2.spr.cl, lm.M3.spr.cl, log.p.spr.cl, log.v1.spr.cl, log.v2.spr.cl))
 ### Printing all models as 1 table
 texreg(file= paste0(bd, "Int_race_models.tex", sep=""), 
        list(lm.M1.spr.cl, lm.M2.spr.cl, lm.M3.spr.cl, log.p.spr.cl, log.v1.spr.cl, log.v2.spr.cl), 
        custom.model.names = c("M1 Group SP*Race", "M2 Group SP*Race", "M3 Group SP*Race", "M4 Group SP*Race", "M5 Group SP*Race", "M6 Group SP*Race"), 
        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender","Same Race", "SD SP Group" , 
                              "Same Race * SD SP Group", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White",  
                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Same Race * SD SP Group", "Amount Offered" ),
        caption = "Statistical models on proposal and voting behaviour with interaction of Race and Standard Deviation of Self-Placement in Groups ",
        label="table:model_intrace",
        booktabs = F, dcolumn = F)
 
 
 ##################################################################################
 #### Data Analysis Full Data Frame. Same models as using the round 1 results only A.7
 #################################################################
 
 lm.FM1<-ols(prop1~ideo.diff + same.g + same.r, data=treat.dfl.o, y=T, x=T)
 lm.FM1.cl<-robcov(lm.FM1, treat.dfl.o$umg)
 lm.FM1.cl
 
 lm.FM1.r<-ols(prop1~ideo.diff + same.g + same.r, data=treat.dfl.o, y=T, x=T)
 lm.FM1.r.cl<-robcov(lm.FM1.r, treat.dfl.o$umg)
 lm.FM1.r.cl
 
 lm.FM2<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o, data=treat.dfl.o, y=T, x=T)
 lm.FM2.cl<-robcov(lm.FM2, treat.dfl.o$umg)
 lm.FM2.cl
 
 lm.FM3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.dfl.o, y=T, x=T)
 lm.FM3.cl<-robcov(lm.FM3, treat.dfl.o$umg)
 lm.FM3.cl
 
 log.FMp <- lrm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 , data=treat.dfl.o, x=T, y=T ) 
 log.FMp.cl <- robcov(log.FMp, treat.dfl.o$umg) 
 log.FMp.cl
 
 log.FMv1<- lrm(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race  + sp + gender + race2, data=treat.df.o, x=T, y=T)
 log.FMv1.cl <- robcov(log.FMv1, treat.df.o$umg)
 log.FMv1.cl
 
 
 log.FMv2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + sp + gender + race2 + p.offered, data=treat.df.o, x=T, y=T)
 log.FMv2.cl <- robcov(log.FMv2, treat.df.o$umg)
 log.FMv2.cl
 
 
 screenreg(list(lm.FM1.cl, lm.FM2.cl, lm.FM3.cl, log.FMp.cl, log.FMv1.cl, log.FMv2.cl))
 texreg(file= paste0(bd, "Full_base_models.tex", sep=""), 
        list(lm.FM1.cl, lm.FM2.cl, lm.FM3.cl, log.FMp.cl, log.FMv1.cl, log.FMv2.cl), 
        custom.model.names = c(" M1 Full Offer", "M2 Full Offer", "M3 Full Offer", "M4 Full Partner", "M5 Full Vote", "M6 Full Vote"), 
        custom.coef.names = c("Intercept", "Ideo. Dist. P-R", "Same Gender", "Same Race", 
                              "Ideo. Dist. P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White", 
                              "Ideo. Dist. P-R", "Same Gender", "Same Race", "Amount Offered" ),
        caption="Models M1-M6 in main text, but including all rounds",
        booktabs = F, dcolumn = F)
 
 
 # Partner and vote models
 
 log.p <- lrm(non.zero ~ ideo.diff+ ideo.diff.o+ same.g+same.g.o + same.r+same.r.o + gender + race2 + sp, data=treat.dfl.o, x=T, y=T ) 
 log.p.cl <- robcov(log.p, treat.dfl.o$umg) 
 log.p.cl
 screenreg(log.p.cl)
 
 #DV: Vote  - Basic model with clustered se for correlation  within groups
 log.v1<- lrm(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp, data=treat.df.o, x=T, y=T)
 log.v1.cl <- robcov(log.v1, treat.df.o$umg)
 log.v1.cl
 
 
 log.v2<- lrm(vote ~  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp + p.offered, data=treat.df.o, x=T, y=T)
 log.v2.cl <- robcov(log.v2, treat.df.o$umg)
 log.v2.cl
 
 log.v3<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp + sd.p.offered + prop.sp, data=treat.df.o, x=T, y=T)
 log.v3.cl <- robcov(log.v3, treat.df.o$umg)
 log.v3.cl
 
 log.v4<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp + sd.p.offered*p.offered + prop.sp, data=treat.df.o, x=T, y=T)
 log.v4.cl <- robcov(log.v4, treat.df.o$umg)
 log.v4.cl
 
 log.v5<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp*sd.p.offered, data=treat.df.o, x=T, y=T)
 log.v5.cl <- robcov(log.v5, treat.df.o$umg)
 log.v5.cl
 
 
 ################################################################################################## 
 ### Correlations and regessions between standard devation of offers and self-placement - Page XXXXX
 ################################################################################
 
 #Full sample
 cor(mydf$sp, mydf$SD.props, use = "complete.obs")
 
 #Treatment sample 
 cor(treat.df$sp, treat.df$SD.props, use = "complete.obs")
 
 #Proposer sample
 treat.df.me<-subset(treat.df, prop_dummy==1)
 cor(treat.df.me$sp, treat.df.me$SD.props,  use = "complete.obs")
 
 ##### regression of SD.props on sp
 summary(lm(SD.props~sp, mydf)) #full sample
 summary (lm(SD.props~sp, treat.df)) #Treatment sample
 summary (lm(SD.props~sp, base.df)) # Baseline sample
 