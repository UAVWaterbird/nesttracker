

library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)

#Read the shapefiles
a <- readOGR(dsn="./TestData", layer="AWPE_F1_400_Bsouth")

pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[1,2]), 
                                   yrange=c(a@bbox[2,1], a@bbox[2,2])))

#calculate the nearest neighbor within the shapefile 
## which one is better to use - nndist or nnwhich? 
pointsnn <- nndist(pointsa, k=1, by=marks(a))
a$dist <- pointsnn

#add observed values
obs<- read.csv("TestData/Observed_Values_Bsouth.csv")
obs<-obs[ which(obs$Flight=="F1"), ] 
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

png("f1bsouth_snnfig.png")
plot(sens ~ thresholds, type="l", col="blue", lwd=2)
points(spec ~ thresholds, type="l", lty=3, lwd=2, col="red")
points(PCC ~ thresholds, type="l", lty=2, lwd=2)
legend(2.0, 0.25, c("sens", "spec", "PCC") , lty=c(1, 2, 3),lwd=2, col=c('blue', 'red', 'black'))
title(main = "Single Image Nearest Neighbor: Sensitivity, Specificity, Percent Correctly Classified")
dev.off()


Results<-as.data.frame(sens)
Results$PCC<-PCC
Results$spec<-spec
Results$thresholds<-thresholds
#write.csv(Results,"TempResultsNN.csv")
Results

#create a new column in the my data with estimated nesters based on the minimum threshold value (where 
#sensitivity, specificity, and PCC all equal 1, or specificity is maximized)
a$Nesting<-ifelse(a$dist<1.75, 1, 0) #I'm not really sure how to choose this threshold, Schaller 1964 range is 0.74-1.85m
head(a)

#calculate Kappa, sensitivity, specificty, auc for selected threshold value

f<- as.data.frame(a)  #create a dataframe that can be used by package Presence Absence 

f<-data.frame(f$UFID, f$observed, f$Nesting)

cmx<-cmx(f, which.model=1)
kappa<-Kappa(cmx)
sensitivity<-sensitivity(cmx)
specificity<-specificity(cmx)
auc<-auc(f)
png("f1summary_bsouth.png") ##Get ready to export the presence.absence.summary figure
presence.absence.summary(f)
dev.off() #Export the latest figure
png("f1ROC_bsouth.png")
auc.roc.plot(f)
dev.off()