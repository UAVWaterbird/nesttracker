### R script for nearest neighbor analysis using spatstat



library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)

#Read the shapefiles
a <- readOGR(dsn="./TestData", layer="AWPE_F1_400_D")
a <- readOGR(dsn="./TestData", layer="AWPE_F3_400_D")
b <- readOGR(dsn="./TestData", layer="AWPE_F4_300_DSHIFT")



######## Reciprocal Nearest Neighbor ###############

#Number of nesting birds from e.g., Flight 1 to Flight 4

reciprocalnn <- function(a, b){
  # Add testing to make sure a + b are SPDFs and have the right
  # columns
  
  pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[1,2]), 
                                     yrange=c(a@bbox[2,1], a@bbox[2,2])))
  
  pointsb <- as.ppp(b@coords, W=owin(xrange=c(b@bbox[1,1], b@bbox[1,2]), 
                                     yrange=c(b@bbox[2,1], b@bbox[2,2])))
  
  #Nearest neighbors from flight a to flight b
  pointsnn <- nncross(pointsa, pointsb)
  a$dist <- pointsnn$dist
  a$nnid <- b@data[pointsnn$which,]$UFID
  head(a) # "a" is the data for the flight from shapefile
  
  #Now go from flight b to flight a 
  pointsnnba <- nncross(pointsb, pointsa)
  b$dist <- pointsnnba$dist
  b$nnid <- a@data[pointsnnba$which,]$UFID
  head(b) # "b" is the data for the flight to shapefile 
  
  nesting_birds <- 0
  for(i in 1:nrow(a)){
    # take a point from flight a
    cur_bird <- a[i,]
    # identify its nearest neighbor in flight b
    nn_bird <- cur_bird$nnid
    # find nearest neighbor from flight b
    to_bird <- b[b$UFID == nn_bird,]
    # if ITS nearest neighbor is point from flight a, add 1 to our count of nesting birds
    if(to_bird$nnid == cur_bird$UFID){
      #    nesting_birds <- nesting_birds + 1
      a$Nesting[i] <- 1
    } else {
      a$Nesting[i] <- 0
    }
    
  }
  return(a)
}

a <- reciprocalnn(a, b)

nestimate<-sum(a$Nesting)
nestimate
#See list of the birds that were counted as non nesters 
NoNest<-subset(a, Nesting==0)
NoNest
#See list of birds classified as nesters
ActiveNest<-subset(a, Nesting==1)
#ActiveNest # remove the "#" to see the list of birds classified as nesting

png("boxplotf1f4saddle.png")
boxplot(a$dist ~ a$Nesting)
dev.off()

###Accuracy Assessment for Multitemporal Nearest Neighbor ###

# Add observed values
obs<- read.csv("TestData/Observed_Values_D.csv")
obs<-obs[ which(obs$Flight=="F3"), ]        #### CHANGE FLIGHT NUMBER HERE (FROM FLIGHT)
obsvalue<-obs$Observed
a$observed<-obsvalue
head(a)  ## this names the column "observed.observed" , need to figure out why is adding the ".observed"

write.csv(a, "f1f3_recip_d.csv")                   #### CHANGE FILE NAME

### Here is where I want to create a shapefile of only birds that I think are actively nesting 
### e.g., writeOGR(a, "BSouthnest1", driver="ESRI shapefile")

#calculate Kappa, sensitivity, specificty, auc

f<- as.data.frame(a)  #create a dataframe that can be used by package Presence Absence 

f<-data.frame(f$UFID, f$observed, f$Nesting)

cmx<-cmx(f, which.model=1)
PCC<-pcc(cmx)
kappa<-Kappa(cmx)
sensitivity<-sensitivity(cmx)
specificity<-specificity(cmx)
auc<-auc(f)


png("f1f4summary_saddle.png") ##Get ready to export the presence.absence.summary figure
presence.absence.summary(f)
dev.off() #Export the latest figure
png("f1f4ROC_saddle.png")
auc.roc.plot(f)
dev.off()

#accresults<-data.frame("kappa"=character(0), "kappa.sd"=character(0), "sensitivity"=character(0),"sensitivity.sd"=character(0), "specificity"=character(0), "specificity.sd"=character(0), "auc"=character(0),"auc.sd"=character(0), "colony"=character(0), "flight"=character(0), stringsAsFactors = FALSE) #Only use this line for first series
#Rf13arm<-data.frame(PCC, kappa, sensitivity, specificity, auc, nestimate, colony="arocksmush", flight="f1f3", stringsAsFactors =FALSE )
#Rf14arm<-data.frame(PCC, kappa, sensitivity, specificity, auc, nestimate, colony="arocksmush", flight="f1f4", stringsAsFactors =FALSE )
#Rf41arm<-data.frame(PCC, kappa, sensitivity, specificity, auc, nestimate, colony="arocksmush", flight="f4f1", stringsAsFactors =FALSE )
#Rf43arm<-data.frame(PCC, kappa, sensitivity, specificity, auc, nestimate, colony="arocksmush", flight="f4f3", stringsAsFactors =FALSE )
#Rf31arm<-data.frame(PCC, kappa, sensitivity, specificity, auc, nestimate, colony="arocksmush", flight="f3f1", stringsAsFactors =FALSE )
#Rf34arm<-data.frame(PCC, kappa, sensitivity, specificity, auc, nestimate, colony="arocksmush", flight="f3f4", stringsAsFactors =FALSE )

RR2<-rbind(Rf13d, Rf14d, Rf41d, Rf43d, Rf31d, Rf34d, Rf13ss, Rf14ss, Rf41ss, Rf43ss, Rf31ss, Rf34ss, 
           Rf13amf, Rf14amf, Rf41amf, Rf43amf, Rf31amf, Rf34amf, Rf13afe, Rf14afe, Rf41afe, Rf43afe, Rf31afe, Rf34afe, 
           Rf13arm, Rf14arm, Rf41arm, Rf43arm, Rf31arm, Rf34arm)
RR2
write.csv(RR2, "RecipResults2.csv")

ResultsAllFIX<-rbind(Resultsf31c, Resultsf13c, Resultsf43c, Resultsf41c, Resultsf14c, Resultsf34c, 
                     Resultsf13bsouth, Resultsf14bsouth, Resultsf41bsouth, Resultsf43bsouth, Resultsf31bsouth, 
                     Resultsf34bsouth, Resultsf13bnorth, Resultsf14bnorth, Resultsf41bnorth, Resultsf43bnorth, 
                     Resultsf31bnorth, Resultsf34bnorth, Resultsf13saddle, Resultsf14saddle, Resultsf41saddle, 
                     Resultsf43saddle, Resultsf31saddle, Resultsf34saddle, Resultsf13bluffn, Resultsf14bluffn,
                     Resultsf41bluffn, Resultsf43bluffn, Resultsf31bluffn, Resultsf34bluffn, Resultsf13bluffs,
                     Resultsf14bluffs, Resultsf41bluffs, Resultsf43bluffs, Resultsf31bluffs)
ResultsAllFIX

write.csv(ResultsAllFIX,"RecipResultsFIX.csv")






Resultsf1f4saddle<-data.frame(kappa, sensitivity, specificity, auc, nestimate, colony="saddle", flight="f1f4", stringsAsFactors =FALSE )

ResultsAll<-rbind(Resultsf1f3bnorth, Resultsf3f1bnorth, Resultsf4f1bnorth, Resultsf1f4bnorth,
                  Resultsf3f4bnorth, Resultsf4f3bnorth, Resultsf4f3c, Resultsf3f4c, Resultsf1f4c, 
                  Resultsf4f1c, Resultsf1f3c, Resultsf3f1c, Resultsf1f3bluffn, Resultsf3f1bluffn, 
                  Resultsf3f4bluffn, Resultsf4f3bluffn, Resultsf4f1bluffn, Resultsf1f4bluffn, 
                  Resultsf1f4bsouth, Resultsf4f1bsouth, Resultsf4f3bsouth, Resultsf3f4bsouth, 
                  Resultsf3f1bsouth, Resultsf1f3bsouth, Resultsf1f3saddle, Resultsf3f1saddle, 
                  Resultsf3f4saddle, Resultsf4f3saddle, Resultsf4f1saddle, Resultsf1f4saddle) 
ResultsAll
# to delete a row = e.g., ResultsAll<-ResultsAll[-c(2, 4, 6), ]
#to export results
write.csv(ResultsAll,"RecipResults.csv")

#Try a Wilcoxon test for medians; similar results but 2 hour vs 26 hour not 
## Data is not normal so I'm going to try a non parametric test
daycomp<-read.csv("TwoDayResults_TEMP.csv")

tH<-subset(daycomp, daycomp$Method == "2hours")
tfH<-subset(daycomp, daycomp$Method == "24hours")
tsH<-subset(daycomp, daycomp$Method == "26hours")

wilcox.test(tfH$TSS, tH$TSS, mu=0, alt="two.sided", paired=TRUE, conf.int=T, conf.level=0.99, exact=FALSE) # p = 0.1176
wilcox.test(tH$TSS, tsH$TSS, mu=0, alt="two.sided", paired=TRUE, conf.int=T, conf.level=0.99, exact=FALSE) #p = 0.01661
wilcox.test(tsH$TSS, tfH$TSS, mu=0, alt="two.sided", paired=TRUE, conf.int=T, conf.level=0.99,exact=FALSE) #p = 0.05703


par(mfrow=c(1,3))
boxplot(tH$Kappa, tsH$Kappa, ylim=c(0,1), ylab = "Kappa", xlab="Flight Intervals", names=c("2 hours", "26 hours"))

boxplot(tH$Kappa, tfH$Kappa, ylim=c(0,1), ylab = "Kappa", xlab="Flight Intervals", names=c("2 hours", "24 hours"))

boxplot(tfH$Kappa, tsH$Kappa, ylim=c(0,1), ylab = "Kappa", xlab="Flight Intervals", names=c("24 hours", "26 hours"))

# Let's do the same thing to see if 2 images are similar to three images
## too circular - using 3 images as gold standard...
#Need to be careful because spatial accuracy may throw off Kappa and make three day actually look worse. Might want to
#Compare this by colonies with a certian level of positional accuracy

#Test Bsouth first
CompareResults<-read.csv("ALLresults_TEMP.csv", stringsAsFactors = FALSE)
bscompare<-CompareResults[CompareResults$colony%in%c("bsouth"),]
bscompareD<-bscompare[bscompare$Method%in%c("Double"),]
bscompareD$Kappa<-sapply(bscompareD$Kappa, as.numeric)
#bscompareD<-droplevels(bscompareD)
bscompareT<-bscompare[bscompare$Method%in%c("Triple"),]
bscompareT$Kappa<-sapply(bscompareT$Kappa, as.numeric)
#bscompareT<-droplevels(bscompareT)

wilcox.test(bscompareD$Kappa, bscompareT$Kappa, mu=0, alt="two.sided", paired=TRUE, conf.int=T, conf.level=0.99, exact=FALSE)

