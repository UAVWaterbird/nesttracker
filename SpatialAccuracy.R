## Spatial Accuracy Assessment for colony rasters
## Using FGDC standards for horizontal accuracy assessment: RMSE, NSSDA (95% Credible Level), n=20

accbsouth3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy\\Bsouth_f3.txt", sep=",", header=TRUE)
accbsouth3

accbsouth1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy\\Bsouth_f1.txt", sep=",", header=TRUE)
accbsouth1

accbsouth4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy\\Bsouth_f4.txt", sep=",", header=TRUE)
accbsouth4

#Relative accuracy for B south f3 to f1
acc31<-accbsouth3
acc31$xtest<-accbsouth1$X
acc31$ytest<-accbsouth1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
NSSDA
