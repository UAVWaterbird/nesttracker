#### Triplicate

library(spatstat)
library(rgeos)
library(sp)
library(rgdal)
library(PresenceAbsence)
library(maptools)
# Create spatial points data frames from flight 1, flight 3, and flight 4 shapefiles

a <- readOGR(dsn="./TestData", layer="AWPE_F1_400_Bsouth")
b <- readOGR(dsn="./TestData", layer="AWPE_F3_400_Bsouth")
c <- readOGR(dsn="./TestData", layer="AWPE_F4_300_Bsouth")

# create spatial point pattern using as.ppp function in Spatstat
pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[2,1]), 
                                   yrange=c(a@bbox[1,2], a@bbox[2,2])))

pointsb <- as.ppp(b@coords, W=owin(xrange=c(b@bbox[1,1], b@bbox[2,1]), 
                                   yrange=c(b@bbox[1,2], b@bbox[2,2])))

pointsc <- as.ppp(c@coords, W=owin(xrange=c(c@bbox[1,1], c@bbox[2,1]), 
                                   yrange=c(c@bbox[1,2], c@bbox[2,2])))

#Nearest neighbors from flight a to flight b
pointsnn <- nncross(pointsa, pointsb)
a$dist <- pointsnn$dist
a$nnid <- b@data[pointsnn$which,3]                              ### be careful here! Make sure you have the correct column
head(a) 


#Now go from flight b to flight a 
pointsnnba <- nncross(pointsb, pointsa)
b$dist <- pointsnnba$dist
b$nnid <- a@data[pointsnnba$which,3]
head(b) 


# from flight a to flight c
pointsnnac <- nncross(pointsa, pointsc)
ac<-a
ac$dist <- pointsnnac$dist
ac$nnid <- c@data[pointsnnac$which,3]                             
head(ac) 

#from flight c to flight a 
pointsnnca <- nncross(pointsc, pointsa)
ca<-c
ca$dist <- pointsnnca$dist
ca$nnid <- a@data[pointsnnca$which,3]                             
head(ca)

#from flight b to flight c
pointsnnbc <- nncross(pointsb, pointsc)
bc<-b
bc$dist <- pointsnnbc$dist
bc$nnid <- c@data[pointsnnbc$which,3]
head(bc) 

#from flight c to flight b 
pointsnncb <- nncross(pointsc, pointsb)
cb<-c
cb$dist <- pointsnncb$dist
cb$nnid <- b@data[pointsnncb$which,3]                             
head(cb)

for(i in 1:length(unique_pts_a)){
  # take a point from flight a
  cur_bird <- flight_a_data[flight_a_data$Point_ID==unique_pts_a[i],]
  
  # identify its nearest neighbor in flight b
  nn_a_b_bird <- cur_bird[cur_bird$Flight_To == flight_b,]$NN_Pt_ID
  nn_a_c_bird <- cur_bird[cur_bird$Flight_To == flight_c,]$NN_Pt_ID
  
  b_bird <- flight_b_data[flight_b_data$Point_ID==nn_a_b_bird,]
  nn_b_a_bird <- b_bird[b_bird$Flight_To == flight_a,]$NN_Pt_ID
  nn_b_c_bird <- b_bird[b_bird$Flight_To == flight_c,]$NN_Pt_ID
  
  c_bird <- flight_c_data[flight_c_data$Point_ID==nn_a_c_bird,]
  nn_c_a_bird <- c_bird[c_bird$Flight_To == flight_a,]$NN_Pt_ID
  nn_c_b_bird <- c_bird[c_bird$Flight_To == flight_b,]$NN_Pt_ID
  
  # if ITS nearest neighbor is point from flight a, add 1 to our count of nesting birds
  if(nn_a_b_bird == nn_c_b_bird && nn_a_c_bird == nn_b_c_bird && nn_b_a_bird == nn_c_a_bird 
     && nn_b_a_bird == cur_bird$Point_ID[1] && nn_c_a_bird == cur_bird$Point_ID[1]
     && nn_b_c_bird == c_bird$Point_ID[1] && nn_c_b_bird == b_bird$Point_ID[1]){
    nesting_birds <- nesting_birds + 1
    #     flight_from_data$Nesting[i] <- 1
  } else {
    non.nesters <- c(non.nesters, i)
  }
  
}
#############################################################################################
############ Original script #####################

setwd("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\GME_Analysis\\Anaho_USGSRTK_Processed\\HolyGrailRCode\\Bnorth_Colony")
my.data <- read.csv("NN_Analysis_Bnorth.csv")

my.data$Flight_from <- as.character(my.data$Flight_from)
my.data$Flight_To <- as.character(my.data$Flight_To)

flight_a <- "F3"
flight_b <- "F1"
flight_c <- "F4"

#working_data <- my.data[my.data$Flight_from != flight_c,]
#working_data <- working_data[working_data$Flight_To != flight_c,]

flight_a_data <- my.data[my.data$Flight_from == flight_a,]

flight_b_data <- my.data[my.data$Flight_from == flight_b,]

flight_c_data <- my.data[my.data$Flight_from == flight_c,]

# Calculate number of nesting birds based on reciprocal nearest neighbors:

nesting_birds <- 0
non.nesters <- NULL

unique_pts_a <- unique(flight_a_data$Point_ID)

for(i in 1:length(unique_pts_a)){
  # take a point from flight a
  cur_bird <- flight_a_data[flight_a_data$Point_ID==unique_pts_a[i],]
  
  # identify its nearest neighbor in flight b
  nn_a_b_bird <- cur_bird[cur_bird$Flight_To == flight_b,]$NN_Pt_ID
  nn_a_c_bird <- cur_bird[cur_bird$Flight_To == flight_c,]$NN_Pt_ID
  
  b_bird <- flight_b_data[flight_b_data$Point_ID==nn_a_b_bird,]
  nn_b_a_bird <- b_bird[b_bird$Flight_To == flight_a,]$NN_Pt_ID
  nn_b_c_bird <- b_bird[b_bird$Flight_To == flight_c,]$NN_Pt_ID
  
  c_bird <- flight_c_data[flight_c_data$Point_ID==nn_a_c_bird,]
  nn_c_a_bird <- c_bird[c_bird$Flight_To == flight_a,]$NN_Pt_ID
  nn_c_b_bird <- c_bird[c_bird$Flight_To == flight_b,]$NN_Pt_ID
  
  # if ITS nearest neighbor is point from flight a, add 1 to our count of nesting birds
  if(nn_a_b_bird == nn_c_b_bird && nn_a_c_bird == nn_b_c_bird && nn_b_a_bird == nn_c_a_bird 
     && nn_b_a_bird == cur_bird$Point_ID[1] && nn_c_a_bird == cur_bird$Point_ID[1]
     && nn_b_c_bird == c_bird$Point_ID[1] && nn_c_b_bird == b_bird$Point_ID[1]){
    nesting_birds <- nesting_birds + 1
    #     flight_from_data$Nesting[i] <- 1
  } else {
    non.nesters <- c(non.nesters, i)
  }
  
}
flight_a_data[non.nesters,]
flight_b_data[non.nesters,]
flight_c_data[non.nesters,] #I'm not really sure if this is bringing up the correct points

hist(flight_a_data[non.nesters,]$NN)
nesting_birds
