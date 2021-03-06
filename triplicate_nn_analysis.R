#### Triplicate

library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)
library(maptools)
# Create spatial points data frames from flight 1, flight 3, and flight 4 shapefiles

c <- readOGR(dsn="./TestData", layer="AWPE_F1_400_SSlopeSHIFT")
b <- readOGR(dsn="./TestData", layer="AWPE_F3_400_SSlope")
a <- readOGR(dsn="./TestData", layer="AWPE_F4_300_SSlopeSHIFT")

triplicatenn <- function(a, b, c){
  # create spatial point pattern using as.ppp function in Spatstat
  pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[1,2]), 
                                     yrange=c(a@bbox[2,1], a@bbox[2,2])))
  
  pointsb <- as.ppp(b@coords, W=owin(xrange=c(b@bbox[1,1], b@bbox[1,2]), 
                                     yrange=c(b@bbox[2,1], b@bbox[2,2])))
  
  pointsc <- as.ppp(c@coords, W=owin(xrange=c(c@bbox[1,1], c@bbox[1,2]), 
                                     yrange=c(c@bbox[2,1], c@bbox[2,2])))
  
  #Nearest neighbors from flight a to flight b
  pointsnn <- nncross(pointsa, pointsb)
  a$dist <- pointsnn$dist
  a$nnid <- b@data[pointsnn$which,]$UFID
  head(a) 
  a$nnid <- as.character(a$nnid)
  
  #Now go from flight b to flight a 
  pointsnnba <- nncross(pointsb, pointsa)
  b$dist <- pointsnnba$dist
  b$nnid <- a@data[pointsnnba$which,]$UFID
  b$nnid <- as.character(b$nnid)
  head(b) 
  
  
  # from flight a to flight c
  pointsnnac <- nncross(pointsa, pointsc)
  ac<-a
  ac$dist <- pointsnnac$dist
  ac$nnid <- c@data[pointsnnac$which,]$UFID 
  ac$nnid <- as.character(ac$nnid)
  head(ac) 
  
  #from flight c to flight a 
  pointsnnca <- nncross(pointsc, pointsa)
  ca<-c
  ca$dist <- pointsnnca$dist
  ca$nnid <- a@data[pointsnnca$which,]$UFID                             
  ca$nnid <- as.character(ca$nnid)
  head(ca)
  
  #from flight b to flight c
  pointsnnbc <- nncross(pointsb, pointsc)
  bc<-b
  bc$dist <- pointsnnbc$dist
  bc$nnid <- c@data[pointsnnbc$which,]$UFID
  bc$nnid <- as.character(bc$nnid)
  head(bc) 
  
  #from flight c to flight b 
  pointsnncb <- nncross(pointsc, pointsb)
  cb<-c
  cb$dist <- pointsnncb$dist
  cb$nnid <- b@data[pointsnncb$which,]$UFID                            
  cb$nnid <- as.character(cb$nnid)
  head(cb)
  
  nesting_birds <- 0
  non.nesters <- NULL
  
  for(i in 1:nrow(a)){
    # take a point from flight a
    cur_bird <- a[i,]
    
    # identify its nearest neighbor in flight b
    nn_a_b_bird <- cur_bird$nnid
    nn_a_c_bird <- ac$nnid[ac$UFID==cur_bird$UFID] 
    
    nn_b_a_bird <- b$nnid[b$UFID==nn_a_b_bird]
    nn_b_c_bird <- bc$nnid[bc$UFID==nn_a_b_bird]
  
    nn_c_a_bird <- ca$nnid[ca$UFID==nn_a_c_bird]
    nn_c_b_bird <- cb$nnid[cb$UFID==nn_b_c_bird]
    
    # if a's nn in b is same as c's nn in b
    # and a's nn in c is same as b's nn in c
    # and b's nn in a is same as c's nn in a
    # and b's nn in a is same as a's nn in b
    # and c's nn in a is same as a's nn in c:
  
    if(nn_a_b_bird == nn_c_b_bird && nn_a_c_bird == nn_b_c_bird && nn_b_a_bird == nn_c_a_bird 
       && nn_b_a_bird == cur_bird$UFID && nn_c_a_bird == cur_bird$UFID){
      # add one to count of nesting birds  
      #nesting_birds <- nesting_birds + 1
      a$Nesting[i]<- 1
      
    } else {
      # if not, record the ID of the non-nesting bird from flight a
      #non.nesters <- c(non.nesters, i)
      a$Nesting[i] <- 0
    }
  }
  return(a)
}


a <- triplicatenn(a, b, c)

nestimate<-sum(a$Nesting) #How many birds did it classify as nesting 
nestimate
#png("boxplotf1f3f4c.png")
#boxplot(a$dist ~ a$Nesting) ### export as a figure after some clean up? 
#dev.off()

write.csv(a, "f1f3f4_sslope.csv")



##################################################
#####Accuracy Assessment 
# Add observed values
obs<- read.csv("TestData/Observed_Values_SSlope.csv")   ### MAKE SURE YOU ARE PULLING THE CORRECT FILE
obs<-obs[ which(obs$Flight=="F4"), ]        #### CHANGE FLIGHT NUMBER HERE (FROM FLIGHT)
obsvalue<-obs$Observed
a$observed<-obsvalue
head(a)


#calculate Kappa, sensitivity, specificty, auc

f<- as.data.frame(a)  #create a dataframe that can be used by package Presence Absence 

f<-data.frame(f$UFID, f$observed, f$Nesting)

cmx<-cmx(f, which.model=1)
PCC<-pcc(cmx)
Kappa<-Kappa(cmx)
sensitivity<-sensitivity(cmx)
specificity<-specificity(cmx)
auc<-auc(f)

### REMEMBER TO CHANGE FIGURE FILE NAMES
png("f4f3f1summary_bluffnorth.png") ##Get ready to export the presence.absence.summary figure
presence.absence.summary(f)
dev.off() #Export the latest figure
png("f4f3f1ROCbluffnorth.png")
auc.roc.plot(f)
dev.off()




## Create (add to) results table 
#Resultsf134sslope<-data.frame(PCC, Kappa, sensitivity, specificity, auc, nestimate, colony="sslope", flight="f1f3f4", stringsAsFactors =FALSE )
#Resultsf143sslope<-data.frame(PCC, Kappa, sensitivity, specificity, auc, nestimate, colony="sslope", flight="f1f4f3", stringsAsFactors =FALSE )
#Resultsf341sslope<-data.frame(PCC, Kappa, sensitivity, specificity, auc, nestimate, colony="sslope", flight="f3f4f1", stringsAsFactors =FALSE )
#Resultsf314sslope<-data.frame(PCC, Kappa, sensitivity, specificity, auc, nestimate, colony="sslope", flight="f3f1f4", stringsAsFactors =FALSE )
#Resultsf413sslope<-data.frame(PCC, Kappa, sensitivity, specificity, auc, nestimate, colony="sslope", flight="f4f1f3", stringsAsFactors =FALSE )
Resultsf431sslope<-data.frame(PCC, Kappa, sensitivity, specificity, auc, nestimate, colony="sslope", flight="f4f3f1", stringsAsFactors =FALSE )
R2<-rbind(Resultsf134amf,Resultsf143amf, Resultsf341amf, Resultsf314amf, Resultsf413amf, Resultsf431amf, Resultsf134afe,
          Resultsf143afe, Resultsf341afe, Resultsf314afe, Resultsf413afe, Resultsf431afe, Resultsf134d, 
          Resultsf143d, Resultsf341d, Resultsf314d, Resultsf413d, Resultsf431d) 
          
R3<-rbind(Resultsf134sslope, Resultsf143sslope, Resultsf341sslope,
          Resultsf314sslope, Resultsf413sslope, Resultsf431sslope)
write.csv(R3, "TriplicateResults3.csv")
ResultsAlltripfix<-rbind(Resultsf134c, Resultsf314c, Resultsf341c, Resultsf431c, Resultsf413c, Resultsf143c,
                         Resultsf143bsouth, Resultsf134bsouth, Resultsf314bsouth, Resultsf341bsouth, 
                         Resultsf431bsouth, Resultsf413bsouth, Resultsf413bnorth, Resultsf143bnorth, Resultsf341bnorth, 
                         Resultsf314bnorth, Resultsf134bnorth, Resultsf431bnorth, Resultsf431saddle, Resultsf413saddle, 
                         Resultsf314saddle, Resultsf341saddle, Resultsf143saddle, Resultsf134saddle, Resultsf134bluffn, 
                         Resultsf143bluffn, Resultsf341bluffn, Resultsf314bluffn, Resultsf413bluffn, Resultsf431bluffn, 
                         Resultsf143bluff, Resultsf143bluffs, Resultsf341bluffs, Resultsf314bluffs, Resultsf413bluffs, 
                         Resultsf431bluffs) #bluff colonies not working for some reason (??)
ResultsAlltripfix
write.csv(ResultsAlltripfix, "TriplicateAccuracyResultsFIX.csv")

wtf<-rbind(Resultsf143bluffs, Resultsf341bluffs, Resultsf314bluffs, Resultsf413bluffs, Resultsf431bluffs, Resultsf143bluffs)


#########################################################

ResultsAlltrip<-rbind(Resultsf1f3f4bs, Resultsf3f1f4bs, Resultsf3f4f1bs, Resultsf4f3f1bs, 
                      Resultsf4f1f3bs, Resultsf1f3f4bn, Resultsf3f1f4bn, Resultsf4f1f3bn, 
                      Resultsf1f4f3bn, Resultsf3f4f1bn, Resultsf4f3f1bn, Resultsf1f3f4c, 
                      Resultsf3f1f4c, Resultsf4f1f3c, Resultsf1f4f3c, Resultsf3f4f1c, Resultsf4f3f1c,
                      Resultsf1f3f4saddle, Resultsf3f1f4saddle, Resultsf1f4f3saddle, Resultsf4f1f3saddle, 
                      Resultsf4f3f1saddle, Resultsf3f4f1saddle, Resultsf4f1f3bluffn, Resultsf1f3f4bluffn, 
                      Resultsf3f1f4bluffn, Resultsf3f4f1bluffn, Resultsf1f4f3bluffn, Resultsf4f3f1bluffn) 
ResultsAlltrip
write.csv(ResultsAlltrip, "TriplicateAccuracyResults.csv")

#write.csv(a, "checkme.csv")

