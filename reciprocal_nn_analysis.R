### R script for nearest neighbor analysis using spatstat
# test a change on the server side

library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)

#Read the shapefiles
a <- readOGR(dsn="./TestData", layer="AWPE_F4_300_Bsouth")
b <- readOGR(dsn="./TestData", layer="AWPE_F3_400_Bsouth")

pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[2,1]), 
                                   yrange=c(a@bbox[1,2], a@bbox[2,2])))

pointsb <- as.ppp(b@coords, W=owin(xrange=c(b@bbox[1,1], b@bbox[2,1]), 
                                   yrange=c(b@bbox[1,2], b@bbox[2,2])))



#Nearest neighbors from flight a to flight b
pointsnn <- nncross(pointsa, pointsb)
a$dist <- pointsnn$dist
a$nnid <- b@data[pointsnn$which,3]                              ### be careful here! Make sure you have the correct column
head(a) # "a" is the data for the flight from shapefile


#Now go from flight b to flight a 
pointsnnba <- nncross(pointsb, pointsa)
b$dist <- pointsnnba$dist
b$nnid <- a@data[pointsnnba$which,3]
head(b) # "b" is the data for the flight to shapefile 






######## Reciprocal Nearest Neighbor Distance ###############

#Number of nesting birds from e.g., Flight 1 to Flight 4
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

sum(a$Nesting)
#See list of the birds that were counted as non nesters 
NoNest<-subset(a, Nesting==0)
NoNest
#See list of birds classified as nesters
ActiveNest<-subset(a, Nesting==1)
#ActiveNest # remove the "#" to see the list of birds classified as nesting

boxplot(a$dist ~ a$Nesting)

###Accuracy Assessment for Multitemporal Nearest Neighbor ###

# Add observed values
obs<- read.csv("TestData/Observed_Values.csv")
obs<-obs[ which(obs$Flight=="F4"), ]        #### CHANGE FLIGHT NUMBER HERE (FROM FLIGHT)
obsvalue<-obs$Observed
a$observed<-obsvalue
head(a)  ## this names the column "observed.observed" , need to figure out why is adding the ".observed"
#write.csv(a, "F4F3.csv")                   #### CHANGE FILE NAME

### Here is where I want to create a shapefile of only birds that I think are actively nesting 
### e.g., writeOGR(a, "BSouthnest1", driver="ESRI shapefile")

#calculate Kappa, sensitivity, specificty, auc

f<- as.data.frame(a)  #create a dataframe that can be used by package Presence Absence 

f<-data.frame(f$UFID, f$observed, f$Nesting)

cmx<-cmx(f, which.model=1)
kappa<-Kappa(cmx)
sensitivity<-sensitivity(cmx)
specificity<-specificity(cmx)
auc<-auc(f)
png("F4F3summary.png") ##Get ready to export the presence.absence.summary figure
presence.absence.summary(f)
dev.off() #Export the latest figure
png("F4F3ROC.png")
auc.roc.plot(f)
dev.off()

#accresults<-data.frame("kappa"=character(0), "kappa.sd"=character(0), "sensitivity"=character(0),"sensitivity.sd"=character(0), "specificity"=character(0), "specificity.sd"=character(0), "auc"=character(0),"auc.sd"=character(0), "colony"=character(0), "flight"=character(0), stringsAsFactors = FALSE) #Only use this line for first series
Resultsf4f3c<-data.frame(kappa, sensitivity, specificity, auc, colony="C", flight="f4f3", stringsAsFactors =FALSE )

ResultsAll<-rbind(Resultsf1f3, Resultsf3f1, Resultsf1f4, Resultsf4f1, Resultsf3f4, Resultsf4f3, Resultsf1f3bsouth, Resultsf3f1bsouth, Resultsf1f4bsouth, Resultsf4f1bsouth, Resultsf4f3bsouth, Resultsf3f4bsouth, Resultsf3f4c, Resultsf1f4c, Resultsf1f3c, Resultsf3f1c, Resultsf4f1c, Resultsf4f3c)
ResultsAll
# to delete a row = e.g., ResultsAll<-ResultsAll[-c(2, 4, 6), ]
#to export results
write.csv(ResultsAll,"C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\SPATSTAT_Analysis\\RecipResults.csv")
