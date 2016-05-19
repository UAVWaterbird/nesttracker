## Spatial Accuracy Assessment for colony rasters
## Using FGDC standards for horizontal accuracy assessment: RMSE, NSSDA (95% Credible Level), n=20

accbsouth3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bsouth_f3.txt", sep=",", header=TRUE)
accbsouth3

accbsouth1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bsouth_f1.txt", sep=",", header=TRUE)
accbsouth1

accbsouth4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bsouth_f4.txt", sep=",", header=TRUE)
accbsouth4

accbnorth3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bnorth_f3.txt", sep=",", header=TRUE)
accbnorth3

accbnorth1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bnorth_f1.txt", sep=",", header=TRUE)
accbnorth1

accbnorth4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bnorth_f4.txt", sep=",", header=TRUE)
accbnorth4

accc3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\C_f3.txt", sep=",", header=TRUE)
accc3

accc1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\C_f1.txt", sep=",", header=TRUE)
accc1

accc4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\C_f4.txt", sep=",", header=TRUE)
accc4

#Relative horizontal positional accuracy between B south f3 to f1
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
RMSE
NSSDA
Rbs31<-data.frame(RMSE, NSSDA, colony="Bsouth", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between B south f3 to f4
acc34<-accbsouth3
acc34$xtest<-accbsouth4$X
acc34$ytest<-accbsouth4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbs34<-data.frame(RMSE, NSSDA, colony="Bsouth", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between B south f1 to f4
acc14<-accbsouth1
acc14$xtest<-accbsouth4$X
acc14$ytest<-accbsouth4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbs14<-data.frame(RMSE, NSSDA, colony="Bsouth", flights="1 and 3", stringsAsFactors =FALSE )







## B NORTH
#Relative horizontal positional accuracy between B North f3 to f1
acc31<-accbnorth3
acc31$xtest<-accbnorth1$X
acc31$ytest<-accbnorth1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbn31<-data.frame(RMSE, NSSDA, colony="Bnorth", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between B north f3 to f4
acc34<-accbnorth3
acc34$xtest<-accbnorth4$X
acc34$ytest<-accbnorth4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbn34<-data.frame(RMSE, NSSDA, colony="Bnorth", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between B north f1 to f4
acc14<-accbnorth1
acc14$xtest<-accbnorth4$X
acc14$ytest<-accbnorth4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbn14<-data.frame(RMSE, NSSDA, colony="Bnorth", flights="1 and 3", stringsAsFactors =FALSE )




## C colony

#Relative horizontal positional accuracy between C f3 to f1
acc31<-accc3
acc31$xtest<-accc1$X
acc31$ytest<-accc1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rc31<-data.frame(RMSE, NSSDA, colony="C", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between B south f3 to f4
acc34<-accc3
acc34$xtest<-accc4$X
acc34$ytest<-accc4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rc34<-data.frame(RMSE, NSSDA, colony="C", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between B south f1 to f4
acc14<-accc1
acc14$xtest<-accc4$X
acc14$ytest<-accc4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rc14<-data.frame(RMSE, NSSDA, colony="C", flights="1 and 3", stringsAsFactors =FALSE )






Accuracy_Results<-rbind(Rbs31, Rbs34, Rbs14, Rbn31, Rbn34, Rbn14, Rc31, Rc34, Rc14)
Accuracy_Results
write.csv(Accuracy_Results, "C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Relative_HP_Accuracy_Results.csv")
