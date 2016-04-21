

library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)

#Read the shapefiles
a <- readOGR(dsn="./TestData", layer="AWPE_F4_300_C")

pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[1,2]), 
                                   yrange=c(a@bbox[2,1], a@bbox[2,2])))

#calculate the nearest neighbor within the shapefile 
pointsnn <- nndist(pointsa, k=1, by=marks(a))
a$dist <- pointsnn
pointwhich<-nnwhich(pointsa)
a$nnid <- pointwhich
a$nnid2 <- a$UFID[nnwhich(pointsa)]

#sort the points by nn distance
asort <- a[order(a$dist),] 

#first find if there are reciprocal neighbors 
recip <- 0
for(i in seq_along(asort)){
  # take a point from flight a
  cur_bird <- asort[i,]
  next_bird<-asort[i+1,]
  # identify its nearest neighbor
  nn_bird <- next_bird$nnid2
  
  #find the nearest neighbor of the cur_bird on the next line down
  # if ITS nearest neighbor is point from flight a, add 1 to our count of nesting birds
  if(nn_bird == cur_bird$UFID){
    #    nesting_birds <- nesting_birds + 1
    a$recip[i] <- 0
  } else {
    a$recip[i] <- 1
  }
}
#This finds reciprocal nn's within a single image but I need it to find reciprocals and only
#assign 1 bird out of the pair a "1"
#In Excel, this works by saying "if the current bird is equal to the nnid on the next row, add 0."
#Does this only work if the points are sorted? 
head(a)











#add observed values
obs<- read.csv("TestData/Observed_Values_Bnorth.csv")
obs<-obs[ which(obs$Flight=="F4"), ] 
a$observed <- obs$Observed
head(a)





# Create some empty variables to store data in later:
sens <- NULL
spec <- NULL
PCC <- NULL

# Create a set of thresholds to compare:
thresholds <- seq(0, max(a$dist), by=0.05)

for(i in 1:length(thresholds)){
  # Classify each bird as nesting or not nesting if they're less than 
  # current threshold:
  classified.nester <- a$dist < thresholds[i]
  
  # Now calculate agreement:
  nonnest.agreement <- !classified.nester & a$observed==0
  nest.agreement <- classified.nester & a$observed==1
  sens[i] <- sum(nest.agreement) / sum(a$observed==1)
  spec[i] <- sum(nonnest.agreement) / sum(a$observed==0)
  PCC[i] <- (sum(nonnest.agreement)+sum(nest.agreement)) / nrow(a)
}
max(PCC)

png("f4bnorth_snnfig.png")
plot(sens ~ thresholds, type="l", col="blue", lwd=2)
points(spec ~ thresholds, type="l", lty=3, lwd=2, col="red")
points(PCC ~ thresholds, type="l", lty=2, lwd=2)
legend(1.7, 0.3, c("sens", "spec", "PCC") , lty=c(1, 2, 3),lwd=2, col=c('blue', 'red', 'black'))
title(main = "Single Image Nearest Neighbor: Sensitivity, Specificity, Percent Correctly Classified")
dev.off()


Results<-as.data.frame(sens)
Results$PCC<-PCC
Results$spec<-spec
Results$thresholds<-thresholds
write.csv(Results,"SNNthesholds_bnorth_F4.csv")
Results

#create a new column in the my data with estimated nesters based on the minimum threshold value (where 
#sensitivity, specificity, and PCC all equal 1, or specificity is maximized)
#a$Nesting<-ifelse(a$dist<1.85, 1, 0) #I'm not really sure how to choose this threshold, Schaller 1964 range is 0.74-1.85m
a$Nesting<-ifelse(a$dist<1.85 & a$dist>0.74, 1, 0) #Schaller 1964 nesting range is 0.74-1.85m
head(a)

#calculate Kappa, sensitivity, specificty, auc for selected threshold value

f<- as.data.frame(a)  #create a dataframe that can be used by package Presence Absence 

f<-data.frame(f$UFID, f$observed, f$Nesting)

cmx<-cmx(f, which.model=1)
kappa<-Kappa(cmx)
sensitivity<-sensitivity(cmx)
specificity<-specificity(cmx)
auc<-auc(f)
png("f4summary_Range_bnorth.png") ##Get ready to export the presence.absence.summary figure
presence.absence.summary(f)
dev.off() #Export the latest figure
png("f4ROC_Range_bnorth.png")
auc.roc.plot(f)
dev.off()


Rf4c<-data.frame(PCC,kappa, sensitivity, specificity, auc, colony="c", flight="f4", stringsAsFactors =FALSE )

ResultsFIX<-rbind(Rf4c)

Resultsf4bnorth<-data.frame(kappa, sensitivity, specificity, auc, colony="bnorth", flight="f4", stringsAsFactors =FALSE )

ResultsAll<-rbind(Resultsf4bsouth, Resultsf3bsouth, Resultsf1bsouth, Resultsf1bnorth, Resultsf3bnorth, Resultsf4bnorth)
ResultsAll
write.csv(ResultsAll,"SNNResults.csv")
