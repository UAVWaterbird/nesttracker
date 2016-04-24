

library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)

#Read the shapefiles
a <- readOGR(dsn="./TestData", layer="AWPE_F4_300_C")

pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[1,2]), 
                                   yrange=c(a@bbox[2,1], a@bbox[2,2])))


###a Stienen diagram, just for fun
stienen(pointsa)
# plot nearest neighbour links
pointwhich<-nnwhich(pointsa)
Z<- pointsa[pointwhich]
arrows(pointsa$x, pointsa$y, Z$x, Z$y, angle=15, length=0.01,col="red")
# find points which are the neighbour of their neighbour
#Just screwing around 
self <- (pointwhich[pointwhich] == seq(pointwhich))
# plot them
A <- pointsa[self]
B <- pointsa[pointwhich[self]]
plot(pointsa)
segments(A$x, A$y, B$x, B$y)


#calculate the nearest neighbor within the shapefile 
pointsnn <- nndist(pointsa, k=1, by=marks(a))
a$dist <- pointsnn
pointwhich<-nnwhich(pointsa)
a$nnid <- pointwhich
a$nnid2 <- a$UFID[nnwhich(pointsa)]

#sort the points by nn distance
# but first add observed values before sorting
obs<- read.csv("TestData/Observed_Values_C.csv")
obs<-obs[ which(obs$Flight=="F4"), ] 
a$observed <- obs$Observed
head(a)
asort <- a[order(a$dist),] 

##Test a BS workaround to pull 1 bird out of every reciprocal pair
asort$nnidshift<-asort$nnid2[c(2:length(asort$nnid2), 1)]
for(i in 1:nrow(asort)){
  #take a point
  cur_bird<-asort[i,]
  #identify its neighbor from the next row down
  next_bird<-cur_bird$nnidshift
  #If 
  if(cur_bird$UFID == next_bird){
    asort$recip[i]<-0
  } else {
      asort$recip[i]<-1
    }
}



## Now using the BS reciprocal code, pull out "attending mates" and outliers
for(i in 1:nrow(asort)){
  if(asort$recip[i]==0 && asort$dist[i]<.74 || asort$dist[i]>1.85){
    asort$nesting[i]<-0
  } else{
    asort$nesting[i]<-1
  }
}

## Histogram of nearest neighbor distances, just for fun
H<-hist(asort$dist)
xfit<-seq(min(asort$dist),max(asort$dist),length=50) 
yfit<-dnorm(xfit,mean=mean(asort$dist),sd=sd(asort$dist)) 
yfit <- yfit*diff(H$mids[1:2])*length(asort$dist) 
lines(xfit, yfit, lwd=2)
##########################################################################
## Accuracy Assessment


#calculate Kappa, sensitivity, specificty, auc for selected threshold value

f<- as.data.frame(a)  #create a dataframe that can be used by package Presence Absence 

f<-data.frame(f$UFID, f$observed, f$Nesting)

cmx<-cmx(f, which.model=1)
kappa<-Kappa(cmx)
PCC<-pcc(cmx)
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

# Create some empty variables to store data in later:
sens <- NULL
spec <- NULL
PCC <- NULL







####### Find thresholds
### What is the best way to find thresholds on both ends? 
####Schaller 1964 range is 0.74-1.85m
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



