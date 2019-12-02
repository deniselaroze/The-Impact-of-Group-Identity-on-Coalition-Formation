
#Data Analysis: GRI experiment
# Author: Denise Laroze
# Co-authors: David Hugh-Jones and Arndt Leininger
# 18/Nov/2019


#setwd("C:/Users/dalaro/Dropbox/group ID in legislative bargaining/Data Analysis GRI")
setwd("C:/Users/Denise Laroze/Dropbox/group ID in legislative bargaining/Data Analysis GRI")
#Baseline directory for saving plots and tables
bd<-"C:/Users/Denise Laroze/Dropbox/group ID in legislative bargaining/Social Identity 2019-20/Data analysis/Figures/"
#bd<-"C:/Users/Andr� Laroze/Dropbox/group ID in legislative bargaining/Presentations/"

#bd <- "/Users/david/Dropbox/Group ID in legislative bargaining/Data Analysis GRI"
#setwd(bd)

 
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

 
 
####################################
##########Subsetting data frames
#################################### 
 
 
mydf<-read.csv("GRI_mydf.csv")  
 
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
treat.r1.df.o<-subset(treat.df.o, round==1)
treat.r1.dfl.o<-subset(treat.dfl.o, round==1)
base.r1.df.o<-subset(mydf, treatment=="Control" & round==1 & prop_dummy==0)


##########################################
########################### Graphics
##########################################

prop.A<-treat.r1.df$prop.A
prop.B<-treat.r1.df$prop.B  

#Gragph of the offers to other participants
scatter2D(prop.A, prop.B, ylab="Offers to B", xlab="Offers to A")
h.offers<-data.matrix(cbind(prop.A, prop.B))

require(ggplot2)
p <- ggplot(treat.r1.df, aes(max_other_prop, min_other_prop)) + scale_fill_gradient(low="grey90", high="black")
p <- p + stat_bin2d(bins = 20) + xlab("Larger offer") +  ylab("Smaller offer") + xlim(c(0,15)) + ylim(c(0,15)) + theme_bw()
p <-p + geom_text(aes(4, 0.2, label="NE"), colour="blue")
p
ggsave(filename=paste0(bd, "offers.png", sep=""),  width=6, height=6)



library(MASS)

# Joint Kernall Density
h1 <- hist(treat.r1.df$prop.A, freq=F)
h2 <- hist(treat.r1.df$prop.B, freq=F)
#top <- max(h1$counts, h2$counts)
#k <- kde2d(df$x, df$y, n=25)


k <- kde2d(prop.A, prop.B, n=300)
image(k, col = gray(c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1, 0), alpha = NULL),
      xlab="Pounds offered to A", ylab="Pounds offered to B")




 
 #### Histograms for variables of interest
  
#######Gender
 png(filename=paste0(bd, "hist_gender.png", sep=""))
 histogram(treat.r1.df$gender, col="gray", xlab ="Gender")
 dev.off()

 race3<-factor(treat.r1.dfl$race2)
 #Race
 png(filename=paste0(bd, "hist_race.png", sep=""))
 histogram(race3, col="gray", xlab="Race") 
 dev.off() 
 
 
 latex(summary(treat.r1.df$race), caption="Number of subjects per race category", file=paste("race", ".tex", sep="") )
 
 png(filename="C:/Users/Andr? Laroze/Dropbox/group ID in legislative bargaining/Presentations/hist_sp.png")
 discrete.histogram(treat.r1.dfl$sp, prob.col="gray",bar.width=0.8 , xlab="Ideological Self-Placement", freq=F, main="")
 dev.off()
  
 png(filename="C:/Users/Andr? Laroze/Dropbox/group ID in legislative bargaining/Presentations/hist_ideo_diff.png")
 discrete.histogram(treat.r1.dfl.o$ideo.diff, prob.col="gray",bar.width=0.8 , xlab="Absolut value ideological distance", freq=F, main="")
 dev.off()
  
 
 
 ###### Histogram Offers
 
 png(filename=paste0(bd, "offers.png", sep=""))
 
 p1 <- hist(treat.r1.dfl.o$prop1)                     # centered at 4
 p2 <- hist(base.r1.dfl.o$prop1)                     # centered at 6
 plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,17), freq=F, main="", xlab="pounds")  # first histogram
 plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,17), freq=F, add=T, main="", xlab="pounds") 
 GRIcol<-rgb(0,0,1,1/4)
 Controlcol<-rgb(1,0,0,1/4)
 legend('topright',c('GRI','Control'),
        fill = c(GRIcol, Controlcol), 
        bty = 'n',
        border = NA)
 dev.off()
 
 
 
 png(filename=paste0(bd, "offers.png", sep=""))
 hist(treat.r1.dfl.o$prop1, freq=F, col="gray",bar.width=0.8, main="", xlim=c(0,17), xlab="Pounds Offered" )                     # centered at 4
 dev.off()
 
 
 
 props<-hist(treat.r1.dfl.o$prop1, freq=F, col="blue", xaxt="n", main="", xlab="pounds") 
 + axis(side=1,at=props$mids,labels=c(0:12))
 ggsave("C:/Users/Andr? Laroze/Dropbox/group ID in legislative bargaining/Presentations/hist_props_others.png")
 
 
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
       xlab="Absolut value ideological distance" ,main="Offers to other participants ",  
       na.rm=TRUE) + facet_grid(g.gender~.)
 
 #Ideological distance by N? white group members
 qplot(ideo.diff, prop1, data = treat.r1.dfl.o, geom = c("point", "smooth"),
       method="lm", shape=factor(g.race), colour=factor(g.race), ylab="pounds",
       xlab="Absolut value ideological distance" ,main="Offers to other participants ",  
       na.rm=TRUE) + facet_grid(g.race~.)
 
 
 #### Love - Hate
 #Offers by ideological distance  - Treatment
 
 plot.df<-ddply(treat.r1.dfl.o, .(ideo.diff), summarise,
       prop = mean(prop1)
       )

 p <- ggplot(plot.df, aes(ideo.diff, prop)) + theme_bw()
 p<- p + geom_point() + labs(x = "Absolut value ideological distance", y= "Mean offer")
 p<- p + geom_hline(yintercept=mean(treat.r1.dfl.o$prop1),linetype="dashed", color = "red") + geom_hline(yintercept=mean(base.dfl.o$prop1, na.rm=T), color = "blue")
 p
 
 
 ggsave(filename=paste0(bd, "mean_offers_ideo_treat.png", sep=""),  width=6, height=6)
 
 
 
 #Offers by ideological distance  - Baseline
 sp<-qplot(ideo.diff, prop1, data = base.dfl.o, geom = c("point", "smooth"),
           method="lm", ylab="pounds",
           xlab="Absolut value ideological distance" ,main="Offers to other participants - Baseline ",  
           na.rm=TRUE)
 sp<-sp + geom_hline(yintercept=mean(treat.r1.dfl.o$prop1),linetype="dashed", color = "red")
 
 sp<-sp + geom_hline(yintercept=mean(base.dfl.o$prop1, na.rm=T), color = "darkgreen") + theme_bw()
 
 sp    
 ggsave(filename=paste0(bd, "offers_ideo_base.png", sep=""),  width=6, height=6)
 
 
 
 
  
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

 
  
####################################################################
######################### Empirical Analysis
#################################################################### 

###Analysis on round 1 of the 10 periods. 

########################## 
 #Descriptive statistics
 with(treat.r1.dfl, table(go1, g.gender))
 
 cor(treat.r1.df$generosityall, treat.r1.df$sp)
 
 
#Linear Models on offers to other participants
###############################
 
 #baseline treatments

 
 #DV proposal to others, main independent variables same gender, race, ideo difference
 lm.1<-ols(prop1~ideo.diff + same.g+same.r, data=treat.r1.dfl.o, y=T, x=T)
 summary(lm.1)
 lm.1.cl<-robcov(lm.1,treat.r1.dfl.o$umg)
 lm.1.cl

#DV proposal lm.1 + ideo.diff.o (difference to the third person)
 lm.2<-ols(prop1~ideo.diff + ideo.diff.o  + same.g+same.r, data=treat.r1.dfl.o, x=T)
 lm.2.cl<-robcov(lm.2, treat.r1.dfl.o$umg)
 lm.2.cl
 
 #Includes a variable for the difference-in-difference between the proposer, reciever and prop other 
 lm.2.1<-ols(prop1~ did.sp + same.g+same.r + sp, data=treat.r1.dfl.o, x=T, y=T)
 lm.2.1.cl<-robcov(lm.2.1, treat.r1.dfl.o$umg)
 lm.2.1.cl

lm.2.1<-ols(prop1~did.sp*sp + same.g+same.r + sp, data=treat.r1.dfl.o, x=T, y=T)
lm.2.1.cl<-robcov(lm.2.1, treat.r1.dfl.o$umg)
lm.2.1.cl

 #Model just including the self-placement of the receiver
 lm.2.2<-ols(prop1~ spo1+ same.g+same.r + sp, data=treat.r1.dfl.o, x=T, y=T)
 lm.2.2.cl<-robcov(lm.2.2, treat.r1.dfl.o$umg)
 lm.2.2.cl

#Model just including the self-placement of the receiver
lm.2.3<-ols(prop1~ ideo.diff*spo1+ same.g+same.r + sp, data=treat.r1.dfl.o, x=T, y=T)
lm.2.3.cl<-robcov(lm.2.3, treat.r1.dfl.o$umg)
lm.2.3.cl


 #baseline with some controles
 lm.3<-ols(prop1~ideo.diff + ideo.diff.o + same.g+same.r+ race2 + gender + sp, data=base.r1.dfl.o, x=T, y=T)
 lm.3.cl<-robcov(lm.3, base.r1.dfl.o$umg)
 lm.3.cl
 
 
 #Main interaction model
 lm.4<-ols(prop1~ideo.diff*ideo.diff.o + same.g + same.r, data= treat.r1.dfl.o, x=T, y=T)
 lm.4.cl<-robcov(lm.4, treat.r1.dfl.o$umg)
 lm.4.cl


###baseline with interactions 
#####################################
 
 #All interaction terms
 lm.3<-lm(prop1~ideo.diff*g.sp.sd + ideo.diff*sp + same.g + same.r + gender + race2, data= treat.r1.dfl.o)
 summary(lm.3) 
 lm.3.cl<-cl(lm.3, treat.r1.dfl.o$umg)
 lm.3.cl
  
 plotreg(lm.3.cl, omit.coef="(Intercept)")
 plotreg(lm.3.cl, file = paste0(bd, "LM_inter.png", sep=""),
         custom.model.names = "",
         custom.coef.names = c("Intercept", "Diff SP", "sigma SP",  "SP", "Same Gender", "Same Race", "Proposer-Male", "Proposer-White", 
                               "Diff SP*sigma SP", "Diff SP*SP"),
         omit.coef = "Intercept", reorder.coef = NULL, ci.level = 0.95,
         )
  
 #Interaction with race 
 lm.4<-lm(prop1~ideo.diff*factor(g.race) + same.g + same.r, data= treat.r1.dfl.o)
 summary(lm.4) 
 lm.4.cl<-cl(lm.4, treat.r1.dfl.o$umg)
 lm.4.cl
 
 lm.5<-lm(prop1~ideo.diff*factor(g.race) , data= treat.r1.dfl.o)
 summary(lm.5) 
 lm.5.cl<-cl(lm.5, treat.r1.dfl.o$umg)
 lm.5.cl
 
 #Interaction with gender composition of the group
 lm.8<-lm(prop1~ideo.diff*factor(g.gender) + same.g + same.r, data= treat.r1.dfl.o)
 summary(lm.8) 
 lm.8.cl<-cl(lm.8, treat.r1.dfl.o$umg)
 lm.8.cl

 lm.9<-lm(prop1~ideo.diff*factor(g.gender), data= treat.r1.dfl.o)
 summary(lm.9) 
 lm.9.cl<-cl(lm.9, treat.r1.dfl.o$umg)
 lm.9.cl
 
 # Interaction with standared deviation of the group's self-placement points
 lm.6<-lm(prop1~ideo.diff*g.sp.sd , data= treat.r1.dfl.o)
 summary(lm.6) 
 lm.6.cl<-cl(lm.6, treat.r1.dfl.o$umg)
 lm.6.cl
  # Interaction with standared deviation of the group's self-placement points, no controls
lm.7<-lm(prop1~ideo.diff*g.sp.sd + same.g + same.r, data= treat.r1.dfl.o)
 summary(lm.7) 
 lm.7.cl<-cl(lm.7, treat.r1.dfl.o$umg)
 lm.7.cl
 



 
 texreg(file= paste0(bd, "Inter_models.tex", sep=""), list(lm.4.cl, lm.5.cl), 
        custom.model.names = c("Diff SP*Gender", "DiffcSP*Race"), 
        custom.coef.names = c("Intercept", "Diff SP", "1F21M", "2F1M", "3F", 
                              "Diff SP*1F2M", "Diff SP*2F1M", "Diff SP*3F", 
                              "1W2O", "2W1O", "3W",
                              "Diff SP*1W2O", "Diff SP*2W1O", 
                              "Diff SP*3W"),
        reorder.coef = NULL, ci.level = 0.90,
        booktabs = T, dcolumn = T)
 
 
 texreg(file= paste0(bd, "Inter_SP_models.tex", sep=""), list(lm.6.cl, lm.7.cl), 
        custom.model.names = c("Diff SP*SD SP", "Diff SP*SD SP + treat"), 
        custom.coef.names = c("Intercept", "Diff SP", "Sigma SP" , "Diff SP*Sigma SP",
                              "Receiver-White","Receiver-Male"),        
        reorder.coef = NULL, ci.level = 0.90,
        booktabs = T, dcolumn = T)
 
 
################################
#plotting the interaction term
################################
 library(effects)
 
 #Interaction
lm.2.1<-ols(prop1~did.sp*sp + same.g+same.r + sp, data=treat.r1.dfl.o, x=T, y=T)
lm.2.1.cl<-robcov(lm.2.1, treat.r1.dfl.o$umg)
lm.2.1.cl


lm.int.did<-lm(prop1~did.sp*sp + same.g+same.r + sp, data=treat.r1.dfl.o)
summary(lm.int.did)
plot(effect(term="did.sp*sp",mod=lm.int.did),multiline=TRUE, main="",
     ylab="Pounds")


 
 #Interaction Diff in SP with variation in SP
 lm.7<-ols(prop1~ideo.diff*ideo.diff.o + same.g + same.r, data= treat.r1.dfl.o, y=T, x=T)
 summary(lm.7) 
 lm.7.cl<-robcov(lm.7, treat.r1.dfl.o$umg)
 lm.7.cl
 plot(effect(term="ideo.diff*ideo.diff.o",mod=lm.7),multiline=TRUE, main="",
      ylab="Pounds", xlab="Ideological Distance")

 eff_df<-data.frame(effect(term="ideo.diff*ideo.diff.o",mod=lm.7),multiline=TRUE)
 ggplot(eff_df)+geom_line(aes(ideo.diff,fit,linetype=rev(factor(ideo.diff.o))))+
   xlab("Diff SP P-R ")+ylab("Fitted Offers")+
   theme(legend.position="right") + labs(linetype='Diff SP P-3rd') + theme_bw()
  
 ggsave(filename= paste0(bd, "SP-SP3.png", sep=""))
  

 lm.diffspsp<-lm(prop1~ideo.diff*sp, data= treat.r1.dfl.o)
 #plot(effect(term="ideo.diff*sp",mod=lm.race),multiline=T, main="",
 #      ylab="Pounds", xlab="Diff SP")
 
 eff_df<-data.frame(effect(term="ideo.diff*sp",mod= lm.diffspsp))
 ggplot(eff_df)+geom_line(aes(ideo.diff,fit,linetype=factor(sp))) + labs(linetype='SP') +xlab("Diff SP")+ylab("Fitted Offers") + theme_bw()
 ggsave(filename= paste0(bd, "Inter_diffSP_sp.png", sep=""))
 
 
 lm.race<-lm(prop1~ideo.diff*factor(g.race), data= treat.r1.dfl.o)
 plot(effect(term="ideo.diff*factor(g.race)",mod=lm.race,default.levels=10),  main="",multiline=T)
 
 
#interaction gender over time
lm.gen.time<-lm(prop1~ideo.diff + userperiod*same.g + same.r, data= treat.r1.dfl.o)
plot(effect(term="userperiod*same.g ",mod=lm.gen.time,default.levels=2),  main="",multiline=T)

#interaction race over time
lm.race.time<-lm(prop1~ideo.diff + userperiod*same.r + same.g, data= treat.r1.dfl.o)
plot(effect(term="userperiod*same.r ",mod=lm.race.time,default.levels=2),  main="",multiline=T)


lm.ideo.time<-lm(prop1~userperiod*ideo.diff + same.r + same.g, data= treat.r1.dfl.o)
plot(effect(term="userperiod*ideo.diff ",mod=lm.ideo.time,default.levels=8),  main="",multiline=T)
summary(lm.ideo.time)


 ### Interaction in voting model

log.v4.eff<- glm(vote ~  sd.p.offered*p.offered , data=treat.r1.df.o, family = "binomial")
plot(effect(term="sd.p.offered*p.offered",mod=log.v4.eff),  main="",multiline=T)
summary(log.v4.eff)

 
 
####################################
#####  Appendix  - Robustness tests
####################################

### Matching group statistics
#############################
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
#### Graphs for matching groups
png(filename= paste0(bd, "ideo_mg.png", sep=""))
plot(ideo.mg, ylab="Coef. Diff SP P-R", xlab="Matching Group") + abline(h=0, col="red")
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


#############################################
######Comparison to Control and placebo tests
#############################################

var.treat<-treat.r1.df.o$sd.p.offered[treat.r1.df.o$vote_result=="Accepted"]
var.base<-base.r1.df.o$sd.p.offered[base.r1.df.o$vote_result=="Accepted"]

var.test(var.treat,var.base)
t.test(var.treat,var.base)

######## Statistical analysis on Control group
 lm.treat1<-ols(prop1~ideo.diff + ideo.diff.o + same.g+same.r+ race2 + gender + sp , data=treat.r1.dfl.o, y=T, x=T)
 lm.treat1
 lm.treat1.cl<-robcov(lm.treat1, treat.r1.dfl.o$umg)
 lm.treat1.cl
 
 lm.base1<-ols(prop1~ideo.diff + ideo.diff.o + same.g+same.r+ race2 + gender + sp, data=base.r1.dfl.o, y=T, x=T)
 lm.base1
 lm.base1.cl<-robcov(lm.base1, base.r1.dfl.o$umg)
 lm.base1.cl
 
 texreg(file= paste0(bd, "placebo.tex", sep=""), 
        list(lm.treat1.cl, lm.base1.cl), 
        custom.model.names = c("Treatment", "Control"), 
        custom.coef.names = c("Intercept", "Diff SP P-R", "Diff SP P-3rd", "Same Gender" , "Same Race", 
                              "Proposer-White", "Proposer-Male", "SP"),        
        ci.level = 0.95)
#########
############# Panel Specification 
#########

 #Random Effects models
 re1 <- plm(prop1~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o + sp + gender + race2, 
            data=treat.r1.dfl.o, index=c("uid", "plm.time"), model="random")
 summary(re1)
 re1.pse<-coeftest(re1, vcov=vcovHC(re1, method="arellano"))
 re1.pse
 

  re2 <- pglm(non.zero ~ ideo.diff+  same.g + same.r + ideo.diff.o+ same.g.o +same.r.o + sp + gender + race2 ,  
           data=treat.r1.dfl.o, family= binomial('logit'), index=c("uid", "plm.time"), model="random")
  summary(re2)

  re3 <- pglm(vote ~    prop.ideo.diff + prop.same.gender + prop.same.race  + sp + gender + race2,
            data=treat.r1.df.o, family= binomial('logit'), index=c("uid", "period"), model="random")
  summary(re3)



#Fixed Effects Models
 fe1 <- plm(prop1~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o, 
            data=treat.r1.dfl.o, index=c("uid", "plm.time"), model="within")
 
 summary(fe1)
 fe1.pse<-coeftest(fe1, vcov=vcovHC(fe1, method="arellano"))
 fe1.pse

#### FE Models
 
 screenreg(list(re1, fe1))
  
#        custom.model.names = c("RE", "RE Inter", "FE", "FE Inter"), 
#        custom.coef.names = c("Intercept", "Diff SP", "Same Gender","Same Race",
#                              "Sigma SP", "Diff SP*Sigma SP"),
#        booktabs = F, dcolumn = F) 
 
texreg(file= paste0(bd, "FE_Models.tex", sep=""),
       list(re1.pse, re2, fe1.pse), 
       custom.model.names = c("RE", "RE", "FE"), 
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                             "Self-Placement", "Proposer-Male", "Proposer-White"),
                              booktabs = F, dcolumn = F,
       caption="Random and Fixed Effects Models with Arellano-Bond s.e.")
###########################
######## Count Models
###########################
 
 library(MASS)
 
 #describe(treat.r1.dfl.o$prop1.p)
 
 treat.r1.dfl.o$prop1.p<-treat.r1.dfl.o$prop1*100
 poisson1 <- glm(prop1.p~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o + sp + gender + race2, data= treat.r1.dfl.o, family=poisson)
 summary(poisson1)
 
 
 negbin1 <- glm.nb(prop1.p~ideo.diff +  same.g + same.r + ideo.diff.o + same.g.o + same.r.o + sp + gender + race2, data= treat.r1.dfl.o)
 summary(negbin1) # if the theta is stat sign then the data is overdispersed
 
 texreg(list(poisson1, negbin1), 
        custom.model.names = c("Poisson", "Neg Binomial"), 
        reorder.coef = NULL, ci.level = 0.95)
 
 texreg(file= paste0(bd, "count.tex", sep=""),
        list(poisson1, negbin1), 
        custom.model.names = c("Poisson", "Neg Binomial"), 
        custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                              "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                              "Self-Placement", "Proposer-Male", "Proposer-White"),        
        reorder.coef = NULL, ci.level = 0.95,
        caption="Count MLE with hundreds of pence and the dependent variable",
        label="table:count")
########################### 
######## Bootstraping
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
hist(coef.ideo.diff, freq=F, xlab="Bootstrapped Coef. for Diff Ideo P-R", main="")
abline(v=ll, col = "red", lwd = 1)
abline(v=ul, col = "red", lwd = 1)
dev.off() 



png(filename=paste0(bd, "boot_ideodiff3rd_mg.png", sep=""))
mean<-mean(coef.ideo.diff.o)
sd<-sd(coef.ideo.diff.o)
ll.o<-mean-1.96*sd
ul.o<-mean+1.96*sd
hist(coef.ideo.diff.o, freq=F, xlab="Bootstrapped Coef. for Diff Ideo P-3rd", main="")
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
hist(coef.glm.ideo.diff, freq=F, xlab="Bootstrapped Coef. for Diff Ideo P-R", main="")
abline(v=ll, col = "red", lwd = 1)
abline(v=ul, col = "red", lwd = 1)
dev.off() 



png(filename=paste0(bd, "boot_p_ideodiff3rd_mg.png", sep=""))
mean<-mean(coef.glm.ideo.diff.o)
sd<-sd(coef.glm.ideo.diff.o)
ll.o<-mean-1.96*sd
ul.o<-mean+1.96*sd
hist(coef.glm.ideo.diff.o, freq=F, xlab="Bootstrapped Coef. for Diff Ideo P-3rd", main="")
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



#hierarchical Models
 
 library(lme4)
 
 hm1<-lmer(prop1~ideo.diff+ ideo.diff.o + same.g + same.r+          
               (1 + ideo.diff+ ideo.diff.o|umg), data=treat.r1.dfl.o)
 summary(hm1)
 
 
#####

 
######################################################## 
################# Other Dependent Variables of Interest 
######################################################## 

###################### 
#### Partner Selection 
#####################

######Non.zero
###########
# logit on partner selection 
#########


          
log.p <- lrm(non.zero ~ ideo.diff+ ideo.diff.o+ same.g+same.g.o + same.r+same.r.o + gender + race2 + sp, data=treat.r1.dfl.o, x=T, y=T ) 
log.p.cl <- robcov(log.p, treat.r1.dfl.o$umg) 
log.p.cl
screenreg(log.p.cl)

#### Ploting predicted probabilities of logit Partner model
mylogit <- glm(non.zero ~ ideo.diff+ ideo.diff.o+ same.g+same.g.o + same.r+same.r.o + gender + race2 + sp, data=treat.r1.dfl.o, family = "binomial")

newdata1 <- with(treat.r1.dfl.o, data.frame(same.g = 1, same.g.o=1, same.r = 1, same.r.o=1, ideo.diff= c(0:7), 
                                             ideo.diff.o = mean(ideo.diff.o), race2="white", gender="female",
                                             sp=mean(sp) ))
newdata1$ideo.diff.p <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(treat.r1.dfl.o, data.frame(same.g = 1, same.g.o=1, same.r = 1, same.r.o=1, ideo.diff.o= rep(seq(from = 0, to = 7, length.out = 8), 8), 
                                             ideo.diff = rep(0:7, each = 8), race2="white", gender="female",
                                             sp=mean(sp) ))
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## plot predicted probabilities
pp<-ggplot(newdata3, aes(x = ideo.diff.o, y = PredictedProb)) 
pp<- pp + geom_ribbon(aes(ymin = LL, ymax = UL, fill = factor(ideo.diff)), alpha = 0.2, )+ scale_fill_grey()
pp<- pp + geom_line(aes(colour = factor(ideo.diff)), size = 1) + scale_colour_grey()
pp<- pp + theme_bw() + labs(fill= "Diff SP P-R", colour= "Diff SP P-R", x="Diff SP P-3rd") 
pp
ggsave(filename=paste0(bd, "predict_prob.png", sep=""))


###############################################
#### Identification of Partners in Full Dataset
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



### Identification of coalition type Using vote definition of Coalition
#######################################################################

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


###### Coalition formation, based on definiton of Offers
#############################################################
treat.df$coalition<-NA
treat.df$coalition[treat.df$zero.prop==0]<-"Grand"
treat.df$coalition[treat.df$zero.prop==1 &  treat.df$closer.ideo==treat.df$offer.more]<-"Connected"
treat.df$coalition[treat.df$zero.prop==1 & treat.df$closer.ideo!=treat.df$offer.more]<- "Disconnected" 
treat.df$coalition[treat.df$zero.prop==1 & treat.df$offer.more=="Equal"]<-"Rogue"
treat.df$coalition[treat.df$zero.prop==1 & treat.df$closer.ideo=="Equal"]<-"Same.ideo"
treat.df$coalition[is.na(treat.df$prop.A)]<-NA
c.type.table<-table(treat.df$coalition)
c.type.table


print(xtable(c.type.table, caption="Types of Coalitions ", file=paste0(bd, "c_type", sep=""), append=F ))



### First round subset
treat.r1.df<-subset(treat.df, round==1)
table(treat.r1.df$coalition)


min.win<-subset(treat.df, zero.prop==1)
table.p.min.win<-with(min.win,table(offer.more, Treatment =closer.ideo, useNA = "no"))
table.p.min.win

latex(ideo.partner.table1, caption="Offers to group-members by similarity in ideology ", file=paste0(bd, "closerto", ".tex", sep="") )
 
 png(filename= paste0(bd, "closerto.png", sep=""))
 barplot(ideo.partner.table2, main="",
         xlab="Proposer is closer to:", #col=c("darkblue", "cyan2", "darkolivegreen4"), 
         legend.text = c("More to A", "More to B", "Equal"),
         args.legend = list(x = "bottomright"))
 dev.off()
 
 
 #table for who the proposer is offering more to, given the similarities in gender
 gender.partner.table<-with(treat.r1.df,table(offer.more, Treatment =closer.gender, useNA = "no"))
 #gender.partner.table<-prop.table(gender.partner.table,2)
 print(gender.partner.table)
 
 
 race.partner.table<-with(treat.r1.df,table(offer.more, Treatment =closer.race, useNA = "no"))
 #race.partner.table<-prop.table(race.partner.table,2)
 print(race.partner.table)
 
 
 
 
 

#############
### Vote
#############

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
         xlab="Vote Results", col="grey", 
         ylab="Ratio", ylim=0:1)

 dev.off()
 
 
 ### Control vs. treatment
 votetable<-with(mydf.r1,table(vote, Treatment = treatment, useNA = "no"))
 votetable<-prop.table(votetable,2)
 print(votetable)
 #Latex
 votetable<-xtable(votetable)
 print(votetable,floating=FALSE)
 #Plot for the votetable
 png(filename="C:/Users/Andr? Laroze/Dropbox/group ID in legislative bargaining/Presentations/votes.png")
 barplot(votetable, main="",
         xlab="Treatments", col=c("red","darkblue"), 
         legend.text = c("Reject", "Accept"),
         args.legend = list(x = "bottomright"))
 dev.off()
 
 treat.r1.dfl.o$race2<-as.factor(treat.r1.dfl.o$race2) 

 
 #Models on vote
log1<-glm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp, family=binomial(link="logit"), data=treat.r1.df.o)
summary(log1)

log.lrm<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp, data=treat.r1.df.o, x=T, y=T)
log.lrm.cl <- robcov(log.lrm, treat.r1.df.o$umg)
log.lrm.cl

log2<-glm(vote ~  p.offered + prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp + sd.p.offered , family=binomial(link="logit"), data=treat.r1.df.o)
summary(log2)



log.v3<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp + sd.p.offered + prop.sp, data=treat.r1.df.o, x=T, y=T)
log.v3.cl <- robcov(log.v3, treat.r1.df.o$umg)
log.v3.cl


log.v4<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp + sd.p.offered*p.offered + prop.sp, data=treat.r1.df.o, x=T, y=T)
log.v4.cl <- robcov(log.v4, treat.r1.df.o$umg)
log.v4.cl

log.v5<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp*sd.p.offered, data=treat.r1.df.o, x=T, y=T)
log.v5.cl <- robcov(log.v5, treat.r1.df.o$umg)
log.v5.cl


# Plot interaction terms
log.v4.eff<- glm(vote ~   p.offered + sp*sd.p.offered , data=treat.r1.df.o, family = "binomial")
plot(effect(term="sp*sd.p.offered",mod=log.v4.eff),  main="",multiline=T)






qplot(vote, data=treat.r1.dfl.o, geom = "density", na.rm=TRUE,
                    xlab="Pounds")+ scale_x_discrete(limit = c(0:17))





# Vote on Earnings
qplot(vote, p.offered, data = treat.r1.dfl.o, geom = c("point", "smooth"), method="lm") + theme_bw()
ggsave(paste0(bd, "lm_vote_p.offered.png", sep=""))

#Vote by same gender
votegender<-with(treat.r1.dfl.o,table(vote, Same.Gender = same.g, useNA = "no"))
votegender<-prop.table(votegender,2)
print(votegender)

#Vote by same race
voterace<-with(treat.r1.dfl.o,table(vote, Same.Race = same.r, useNA = "no"))
voterace<-prop.table(voterace,2)
print(voterace)

#Vote by ideo.diff
voteideo<-with(treat.r1.dfl.o,table(vote, Ideo.diff = ideo.diff, useNA = "no"))
voteideo<-prop.table(voteideo,2)
print(voteideo)

plot(voteideo)




 
 

##################################
#### Model with Disaggregated Race Categories
##################################
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
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race All", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd All", 
                             "Self-Placement", "Proposer-Male", "Proposer-Brown" ,"Proposer-White", 
                             "Diff SP P-R", "Same Gender", "Same Race All", "Amount Offered" ),
       caption = "Statistical Models on proposal and voting behavior using a disaggregated race category",
       label="table:base_race_model",
       booktabs = F, dcolumn = F)



######################################## 
### Models with interaction g.sp.sd*same.g
########################################
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
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "SD SP Group" , "Same Race",
                             "Same Gender * SD SP Group", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                             "Self-Placement", "Proposer-Male", "Proposer-White",  
                             "Diff SP P-R", "Same Gender", "Same Race", "Same Gender * SD SP Group", "Amount Offered" ),
       caption = "Statistical Models on Proposal and Voting Behaviour with interaction of Gender and Standard Deviation of Self-Placement in Groups ",
       label="table:model_intgender",
       booktabs = F, dcolumn = F)



######################################## 
### Models with interaction g.sp.sd*same.r
########################################
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
       custom.model.names = c("M1 Group SP*Gender", "M2 Group SP*Gender", "M3 Group SP*Gender", "M4 Group SP*Gender", "M5 Group SP*Gender", "M6 Group SP*Gender"), 
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender","Same Race", "SD SP Group" , 
                             "Same Race * SD SP Group", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                             "Self-Placement", "Proposer-Male", "Proposer-White",  
                             "Diff SP P-R", "Same Gender", "Same Race", "Same Race * SD SP Group", "Amount Offered" ),
       caption = "Statistical Models on Proposal and Voting Behaviour with interaction of Race and Standard Deviation of Self-Placement in Groups ",
       label="table:model_intrace",
       booktabs = F, dcolumn = F)






######################### 
### Printing models of interest
##########################
  
 
 
 #DV: Offer  - Definition 1 same group
 lm.M1<-ols(prop1~ideo.diff + same.g + same.r, data=treat.r1.dfl.o, y=T, x=T)
 lm.M1.cl<-robcov(lm.M1, treat.r1.dfl.o$umg)
 lm.M1.cl
 

 #DV: Offer  - Definition 1 with controles
 lm.M2<-ols(prop1~ideo.diff + ideo.diff.o + same.g + + same.g.o +same.r+ same.g.o + same.r.o, data=treat.r1.dfl.o, y=T, x=T)
 lm.M2.cl<-robcov(lm.M2, treat.r1.dfl.o$umg)
 lm.M2.cl


 lm.M2.lm<-lm(prop1~ideo.diff + ideo.diff.o + same.g + + same.g.o +same.r+ same.g.o + same.r.o, data=treat.r1.dfl.o)
boot_year <- cluster.boot(lm.M2.lm, as.numeric(as.factor(cbind(treat.r1.dfl.o$umg,treat.r1.dfl.o$uid ))))
coeftest(lm.M2.lm, boot_year)



 
 lm.M3<-ols(prop1~ideo.diff + same.g+same.r+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 lm.M3.cl<-robcov(lm.M3, treat.r1.dfl.o$umg, treat.r1.dfl.o$uid)
 lm.M3.cl


# This model doesn't show any sign difference. Send to appendix
 lm.M4<-ols(prop1~ideo.diff*sp + same.g*gender+same.r*race2+ ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o, y=T, x=T)
 lm.M4.cl<-robcov(lm.M4, treat.r1.dfl.o$umg)
 lm.M4.cl 
# This model doesn't show any relevant differences. Send to appendix
library("multcomp")
lm.M4ft<-lm(prop1~ideo.diff:sp + same.g:gender +same.r:race2+  ideo.diff.o + same.g.o + same.r.o + sp + gender + race2 , data=treat.r1.dfl.o)
lm.M4ft
glht(lm.M4, linfct = c("same.g:genderfemale = 0", "same.g:gendermale + gendermale==0", "same.r:race2white + race2white=0", "same.r:race2other=0",
                       "ideo.diff:sp + sp=0"))



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


     




### Print Models in 2 separate tables
texreg(file= paste0(bd, "base_models.tex", sep=""), 
       list(lm.M1.cl, lm.M2.cl, lm.M3.cl), 
       custom.model.names = c("M1 Offer", "M2 Offer", "M3 Offer"), 
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                             "Self-Placement", "Proposer-Male", "Proposer-White" ),
       booktabs = F, dcolumn = F)


texreg(#file= paste0(bd, "log_models.tex", sep=""), 
       list(log.p.cl, log.v1, log.v2.cl), 
       custom.model.names = c("M4 Partner", "M5 Vote", "M6 Vote"), 
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                             "Self-Placement", "Proposer-Male", "Proposer-White", 
                             "Diff SP P-R", "Same Gender", "Same Race", "Amount Offered" ),
       booktabs = F, dcolumn = F,
       stars=c(0.001, 0.01, 0.05,  0.1))

screenreg(list(lm.M1.cl, lm.M2.cl, lm.M3.cl))


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
 


#################################################################
#### Data Analysis Full Data Frame. Same models as using the round 1 results only
#################################################################

lm.FM1<-ols(prop1~ideo.diff + same.g + same.r, data=treat.dfl.o, y=T, x=T)
lm.FM1.cl<-robcov(lm.FM1, treat.dfl.o$umg)
lm.FM1.cl

lm.FM1.r<-ols(prop1~ideo.diff + same.g + same.r + factor(round), data=treat.dfl.o, y=T, x=T)
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
       custom.coef.names = c("Intercept", "Diff SP P-R", "Same Gender", "Same Race", 
                             "Diff SP P-3rd", "Same Gender P-3rd", "Same Race P-3rd", 
                             "Self-Placement", "Proposer-Male", "Proposer-White", 
                             "Diff SP P-R", "Same Gender", "Same Race", "Amount Offered" ),
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


######################################################
#### Identification of Partners  - Draft
######################################################
### Who you give more to 
id<-rep(NA, nrow(treat.df))
others<-list(rep(NA, 2))
#Identifying who is closer in gender, race and ideology to the proposer, and who the proposer is offering more to.
for (i in 1:nrow(treat.df)){
  id[i]<-treat.df$group_id[i]
  others <- setdiff(1:3, id[i])
  treat.df$ideo.dist.A[i]<-abs(treat.df$sp[i]-treat.df[[paste0("spo", others[1], sep="")]][[i]]) 
  treat.df$ideo.dist.B[i]<-abs(treat.df$sp[i]-treat.df[[paste0("spo", others[2], sep="")]][[i]]) 
  var.ideo<-c("ideo.dist.A", "ideo.dist.B")
  treat.df$closer.ideo[i]<-ifelse( treat.df$ideo.dist.A[i]==treat.df$ideo.dist.B[i] , "Equal" ,names(which.min(treat.df[i,var.ideo])) )
  
  treat.df$gender.dist.A[i]<-if(treat.df$gender[i]==treat.df[[paste0("go", others[1], sep="")]][[i]]) 1 else 0  
  treat.df$gender.dist.B[i]<-if(treat.df$gender[i]==treat.df[[paste0("go", others[2], sep="")]][[i]]) 1 else 0 
  
  treat.df$race.dist.A[i]<-if(treat.df$race[i]==treat.df[[paste0("ro", others[1], sep="")]][[i]]) 1 else 0  
  treat.df$race.dist.B[i]<-if(treat.df$race[i]==treat.df[[paste0("ro", others[2], sep="")]][[i]]) 1 else 0 
  
  var.gender<-c("gender.dist.A", "gender.dist.B")
  treat.df$closer.gender[i]<-ifelse(treat.df$gender.dist.A[i]==treat.df$gender.dist.B[i], "Equal" , names(which.max(treat.df[i,var.gender])) )
  
  var.race<-c("race.dist.A", "race.dist.B")
  treat.df$closer.race[i]<-ifelse( treat.df$race.dist.A[i]==treat.df$race.dist.B[i], "Equal" ,names(which.max(treat.df[i,var.race])) )
  
  props.i.want<-paste0("prop.", c("A","B"), sep="")
  treat.df$offer.more[i]<-ifelse(treat.df$prop.A[i]==treat.df$prop.B[i] , "Equal" ,names(which.max(treat.df[i,props.i.want])) )
}

rm(id, others)

treat.df$closer.ideo[treat.df$closer.ideo=="ideo.dist.A"]<-"A"
treat.df$closer.ideo[treat.df$closer.ideo=="ideo.dist.B"]<-"B"

treat.df$offer.more[treat.df$offer.more=="prop.A"]<-"A"
treat.df$offer.more[treat.df$offer.more=="prop.B"]<-"B"

treat.df$coalition<-NA
treat.df$coalition[treat.df$zero.prop==0]<-"Grand"
treat.df$coalition[treat.df$zero.prop==1 &  treat.df$closer.ideo==treat.df$offer.more]<-"Connected"
treat.df$coalition[treat.df$zero.prop==1 & treat.df$closer.ideo!=treat.df$offer.more]<- "Disconnected" 
treat.df$coalition[treat.df$zero.prop==1 & treat.df$offer.more=="Equal"]<-"Rogue"
treat.df$coalition[treat.df$zero.prop==1 & treat.df$closer.ideo=="Equal"]<-"Same.ideo"
treat.df$coalition[is.na(treat.df$prop.A)]<-NA
table(treat.df$coalition)


### First round subset
treat.r1.df<-subset(treat.df, round==1)
table(treat.r1.df$coalition)


#### Table for who is offering more to whom. 
#table for who the proposer is offering more to, given the ideological distance
ideo.partner.table1<-with(treat.df,table(offer.more, Treatment =closer.ideo, useNA = "no"))
ideo.partner.table1

ideo.partner.table<-with(treat.df,table(closer.ideo, Treatment =offer.more, useNA = "no"))
ideo.partner.table2<-prop.table(ideo.partner.table,2)
print(ideo.partner.table)
print(ideo.partner.table2)


min.win<-subset(treat.df, zero.prop==1)
table.p.min.win<-with(min.win,table(offer.more, Treatment =closer.ideo, useNA = "no"))
table.p.min.win


############################################
#### in/our group love-hate
############################################
summarySE(data = mydf.l.o, "prop1", groupvars = "treatment", na.rm = T, conf.interval = 0.95, .drop = TRUE)


t.test( mydf.l.o$prop1~ mydf.l.o$treatment)

summarySE(data = mydf.l.o, "g.sp.sd", groupvars = "treatment", na.rm = T, conf.interval = 0.95, .drop = TRUE)

t.test( mydf.l.o$g.sp.sd~ mydf.l.o$treatment)
############################################  
##### Characteristics of the sample 
############################################
 
mydf.uniq<-subset(mydf, userperiod==0 & treatment=="GRI")
 
 #Gender and Race Composition of treatment group
 t.gr <- table(mydf.uniq$gender, mydf.uniq$race ) # A will be rows, B will be columns 
 t.gr # print table 
 #latex(t.gr, caption="Frequences of Gender and Race in treatment sample ", file=paste0(bd, "t_gr", ".tex", sep="") )
 
 
 #ideological composition fof treatment group, by gender
 t.gi <- table(mydf.uniq$gender, mydf.uniq$sp ) # A will be rows, B will be columns 
 t.gi<-prop.table(t.gi, 1) # row percentages 
 t.gi<-round(t.gi, 2) # print table 
 Ideology<-t.gi
 
 #latex(Ideology, caption="Percentage ideological self-placement in treatment, by gender", file=paste0(bd, "t_gi", ".tex", sep="") )
 
 rm(Ideology)
 
 
 # Payments
 describe(mydf.uniq$final_earnings)
 
 
 
 #Gender
  g <-table(mydf.uniq$gender) 
 prop.table(g) # cell percentages
 
 #Race
 r <-table(mydf.uniq$race2) 
 prop.table(r)

 
 
 #Nationalities
 nation<-table(mydf.uniq$gender, mydf.uniq$nationality )
 
 
#Corralation between standard devation of offers and self-placement

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
summary (lm(SD.props~sp, mydf.2d.)) #2dictator sample

############################## 
####  Graphical Appendix
##############################
 
 
 den_max_prop<-qplot(max_other_prop, data=mydf.r1, shape=treatment, 
                     colour=treatment, geom = "density", na.rm=TRUE,
                     xlab="Pounds")+ scale_x_discrete(limit = c(0:17)) + theme_bw()
 den_max_prop
 ggsave(filename= paste0(bd, "den_max_prop.png", sep=""))
 #Difference between "partner" and the third participant (AKA "Other")
 den_diff_prop<-qplot(diff_other_prop, data=mydf.r1, shape=treatment, 
                      colour=treatment, geom = "density", na.rm=TRUE,
                      xlab="Pounds"
                      ) + scale_x_discrete(limit = c(0:17))
 den_diff_prop
  

 
############################### 
#### DHJ stuff for presentation
########################

library(dplyr)

tmp <- unlist(tapply(mydf$ideology, interaction(mydf$period, mydf$ugroup), 
   function(x) c(diff(sort(x)), max(x) - min(x))))
barplot(prop.table(table(tmp))*100, ylab="%", ylim=c(0,30), xlab="Ideol. distance")

barplot(with(treat.r1.dfl.o, tapply(prop1, race2==ro2, mean)), ylim=c(0,5), 
      names.arg=c("Different race", "Same race")) 
barplot(with(treat.r1.dfl.o, tapply(prop1, gender==go1, mean)), ylim=c(0,5), 
      names.arg=c("Different gender", "Same gender"))
id.means <- with( treat.r1.dfl.o, tapply(prop1, ideo.diff, mean))
barplot(id.means, ylim=c(0,6), xlab="Ideol. distance", ylab="£") -> xvals
text(xvals, id.means + .125, table(treat.r1.dfl.o$ideo.diff), cex=0.8, col="grey")
 
lm1 <- robcov(ols(prop1 ~ ideo.diff, data=treat.r1.dfl.o , x=T, y=T), treat.r1.dfl.o$uid)
lm2 <- robcov(ols(prop1 ~ sp + ideo.diff + gender*go1 + race2*ro2, data=treat.r1.dfl.o , x=T, y=T), 
      treat.r1.dfl.o$uid)
lm3 <- robcov(ols(prop1 ~ sp + ideo.diff + ideo.diff.o, data=treat.r1.dfl.o , x=T, y=T), 
      treat.r1.dfl.o$uid)
 screenreg(list(lm1, lm2, lm3))

tapply(dfr$vote, cut(dfr$p.offered, -1:13), mean) %>% barplot(names.arg=0:13)
log.M1 <- lrm(vote ~prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp, data=treat.r1.df.o, x=T, y=T)
log.M2<- lrm(vote ~  p.offered +  prop.ideo.diff + prop.same.gender + prop.same.race + gender + race2+ sp, data=treat.r1.df.o, x=T, y=T)
robcov(log.M2, treat.r1.df.o$umg)

# without including effect of offer:
barplot(with(treat.r1.df.o, tapply(vote, prop.ideo.diff, mean))*100, ylim=c(0,100), ylab="% prob. accepted", xlab="Ideol. distance", col="green4")

# earnings by ideol dist from proposer (I hope!)
barplot(tapply(treat.r1.df.o$earnings, treat.r1.df.o$prop.ideo.diff, mean, na.rm=T), ylim=c(0,6), xlab="Ideol. distance from proposer", ylab="£", col="red3")
# % earning zero
barplot(tapply(treat.r1.df.o$earnings==0 * 100, treat.r1.df.o$prop.ideo.diff, mean, na.rm=T), ylim=c(0,6), xlab="Ideol. distance from proposer", ylab="% earning 0", col="red3")

