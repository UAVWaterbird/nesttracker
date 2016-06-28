## Spatial Accuracy Assessment for colony rasters
## Using FGDC standards for horizontal accuracy assessment: RMSE, NSSDA (95% Credible Level), n=20

## I should probably just write a function to clean this up

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

accsad3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Saddle_f3.txt", sep=",", header=TRUE)
accsad3

accsad1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Saddle_f1.txt", sep=",", header=TRUE)
accsad1

accsad4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Saddle_f4.txt", sep=",", header=TRUE)
accsad4

accslop3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\SSlope_f3.txt", sep=",", header=TRUE)
accslop3

accslop1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\SSlope_f1.txt", sep=",", header=TRUE)
accslop1

accslop4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\SSlope_f4.txt", sep=",", header=TRUE)
accslop4

accbln1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bluffn_f1.txt", sep="," , header=TRUE)
accbln1

accbln3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bluffn_f3.txt", sep=",", header=TRUE)
accbln3

accbln4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bluffn_f4.txt", sep=",", header=TRUE)
accbln4

accbls1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bluffs_f1.txt", sep="," , header=TRUE)
accbls1

accbls3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bluffs_f3.txt", sep=",", header=TRUE)
accbls3

accbls4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Bluffs_f4.txt", sep=",", header=TRUE)
accbls4

accafe1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\AFissEast_f1.txt", sep="," , header=TRUE)
accafe1

accafe3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\AFissEast_f3.txt", sep=",", header=TRUE)
accafe3

accafe4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\AFissEast_f4.txt", sep=",", header=TRUE)
accafe4

accarm1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\ARocksMush_f1.txt", sep="," , header=TRUE)
accarm1

accarm3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\ARocksMush_f3.txt", sep=",", header=TRUE)
accarm3

accarm4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\ARocksMush_f4.txt", sep=",", header=TRUE)
accarm4


accamf1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\AMushFiss_f1.txt", sep="," , header=TRUE)
accamf1

accamf3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\AMushFiss_f3.txt", sep=",", header=TRUE)
accamf3

accamf4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\AMushFiss_f4.txt", sep=",", header=TRUE)
accamf4


accd3<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\D_f3.txt", sep=",", header=TRUE)
accd3

accd1<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\D_f1.txt", sep=",", header=TRUE)
accd1

accd4<-read.table("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\D_f4.txt", sep=",", header=TRUE)
accd4

## B South Colony
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

#Relative horizontal positional accuracy between C f3 to f4
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

#Relative horizontal positional accuracy between C f1 to f4
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



## Saddle colony
#Relative horizontal positional accuracy between Saddle f3 to f1
acc31<-accsad3
acc31$xtest<-accsad1$X
acc31$ytest<-accsad1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rsad31<-data.frame(RMSE, NSSDA, colony="Saddle", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between Saddle f3 to f4
acc34<-accsad3
acc34$xtest<-accsad4$X
acc34$ytest<-accsad4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rsad34<-data.frame(RMSE, NSSDA, colony="Saddle", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between Saddle f1 to f4
acc14<-accsad1
acc14$xtest<-accsad4$X
acc14$ytest<-accsad4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rsad14<-data.frame(RMSE, NSSDA, colony="Saddle", flights="1 and 3", stringsAsFactors =FALSE )


## South Slope Colony

#Relative horizontal positional accuracy between Soout Slope f3 to f1
acc31<-accslop3
acc31$xtest<-accslop1$X
acc31$ytest<-accslop1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rslop31<-data.frame(RMSE, NSSDA, colony="South Slope", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between South Slope f3 to f4
acc34<-accslop3
acc34$xtest<-accslop4$X
acc34$ytest<-accslop4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rslop34<-data.frame(RMSE, NSSDA, colony="South Slope", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between South Slope f1 to f4
acc14<-accslop1
acc14$xtest<-accslop4$X
acc14$ytest<-accslop4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rslop14<-data.frame(RMSE, NSSDA, colony="South Slope", flights="1 and 3", stringsAsFactors =FALSE )


## Bluff North colony

#Relative horizontal positional accuracy between Soout Slope f3 to f1
acc31<-accbln3
acc31$xtest<-accbln1$X
acc31$ytest<-accbln1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbln31<-data.frame(RMSE, NSSDA, colony="Bluff North", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between South Slope f3 to f4
acc34<-accbln3
acc34$xtest<-accbln4$X
acc34$ytest<-accbln4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbln34<-data.frame(RMSE, NSSDA, colony="Bluff North", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between South Slope f1 to f4
acc14<-accbln1
acc14$xtest<-accbln4$X
acc14$ytest<-accbln4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbln14<-data.frame(RMSE, NSSDA, colony="Bluff North", flights="1 and 3", stringsAsFactors =FALSE )


## Bluff South colony

#Relative horizontal positional accuracy between Soout Slope f3 to f1
acc31<-accbls3
acc31$xtest<-accbls1$X
acc31$ytest<-accbls1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbls31<-data.frame(RMSE, NSSDA, colony="Bluff South", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between South Slope f3 to f4
acc34<-accbls3
acc34$xtest<-accbls4$X
acc34$ytest<-accbls4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbls34<-data.frame(RMSE, NSSDA, colony="Bluff South", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between South Slope f1 to f4
acc14<-accbls1
acc14$xtest<-accbls4$X
acc14$ytest<-accbls4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rbls14<-data.frame(RMSE, NSSDA, colony="Bluff South", flights="1 and 3", stringsAsFactors =FALSE )






## A Fissure to East Colony
#Relative horizontal positional accuracy between afe f3 to f1
acc31<-accafe3
acc31$xtest<-accafe1$X
acc31$ytest<-accafe1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rafe31<-data.frame(RMSE, NSSDA, colony="AFissEast", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between afe f3 to f4
acc34<-accafe3
acc34$xtest<-accafe4$X
acc34$ytest<-accafe4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rafe34<-data.frame(RMSE, NSSDA, colony="AFissEast", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between afe f1 to f4
acc14<-accafe1
acc14$xtest<-accafe4$X
acc14$ytest<-accafe4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rafe14<-data.frame(RMSE, NSSDA, colony="AFissEast", flights="1 and 3", stringsAsFactors =FALSE )




## A Rocks to Mushroom Colony
#Relative horizontal positional accuracy between afe f3 to f1
acc31<-accarm3
acc31$xtest<-accarm1$X
acc31$ytest<-accarm1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rarm31<-data.frame(RMSE, NSSDA, colony="ARocksMush", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between afe f3 to f4
acc34<-accarm3
acc34$xtest<-accarm4$X
acc34$ytest<-accarm4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rarm34<-data.frame(RMSE, NSSDA, colony="ARocksMush", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between afe f1 to f4
acc14<-accarm1
acc14$xtest<-accarm4$X
acc14$ytest<-accarm4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rarm14<-data.frame(RMSE, NSSDA, colony="ARocksMush", flights="1 and 3", stringsAsFactors =FALSE )





## A Mushroom to Fissure Colony
#Relative horizontal positional accuracy between afe f3 to f1
acc31<-accamf3
acc31$xtest<-accamf1$X
acc31$ytest<-accamf1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Ramf31<-data.frame(RMSE, NSSDA, colony="AMushFiss", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between afe f3 to f4
acc34<-accamf3
acc34$xtest<-accamf4$X
acc34$ytest<-accamf4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Ramf34<-data.frame(RMSE, NSSDA, colony="AMushFiss", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between afe f1 to f4
acc14<-accamf1
acc14$xtest<-accamf4$X
acc14$ytest<-accamf4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Ramf14<-data.frame(RMSE, NSSDA, colony="AMushFiss", flights="1 and 3", stringsAsFactors =FALSE )





## D colony

#Relative horizontal positional accuracy between C f3 to f1
acc31<-accd3
acc31$xtest<-accd1$X
acc31$ytest<-accd1$Y
acc31$xdif<-acc31$X-acc31$xtest
acc31$ydif<-acc31$Y-acc31$ytest
acc31$xdif2<-(acc31$xdif)^2
acc31$ydif2<-(acc31$ydif)^2
acc31$x2y2<-acc31$xdif2+acc31$ydif2


RMSE<-sqrt(sum(acc31$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rd31<-data.frame(RMSE, NSSDA, colony="D", flights="1 and 2", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between C f3 to f4
acc34<-accd3
acc34$xtest<-accd4$X
acc34$ytest<-accd4$Y
acc34$xdif<-acc34$X-acc34$xtest
acc34$ydif<-acc34$Y-acc34$ytest
acc34$xdif2<-(acc34$xdif)^2
acc34$ydif2<-(acc34$ydif)^2
acc34$x2y2<-acc34$xdif2+acc34$ydif2


RMSE<-sqrt(sum(acc34$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rd34<-data.frame(RMSE, NSSDA, colony="D", flights="2 and 3", stringsAsFactors =FALSE )

#Relative horizontal positional accuracy between C f1 to f4
acc14<-accd1
acc14$xtest<-accd4$X
acc14$ytest<-accd4$Y
acc14$xdif<-acc14$X-acc14$xtest
acc14$ydif<-acc14$Y-acc14$ytest
acc14$xdif2<-(acc14$xdif)^2
acc14$ydif2<-(acc14$ydif)^2
acc14$x2y2<-acc14$xdif2+acc14$ydif2


RMSE<-sqrt(sum(acc14$x2y2)/20)
NSSDA<-RMSE*1.7308
RMSE
NSSDA
Rd14<-data.frame(RMSE, NSSDA, colony="D", flights="1 and 3", stringsAsFactors =FALSE )





Accuracy_Results<-rbind(Rbs31, Rbs34, Rbs14, Rbn31, Rbn34, Rbn14, Rc31, Rc34, Rc14, Rsad31, Rsad34, Rsad14, 
                        Rslop31, Rslop34, Rslop14, Rbln31, Rbln34, Rbln14, Rbls31, Rbls34, Rbls14, Rafe31, Rafe34, Rafe14, 
                        Rarm31, Rarm34, Rarm14, Ramf31, Ramf34, Ramf14, Rd31, Rd34, Rd14)
Accuracy_Results
write.csv(Accuracy_Results, "C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Accuracy_Positional\\Relative_HP_Accuracy_Results.csv")

## Let's take a look
mydata<-read.csv("ALLresults_TEMP.csv", stringsAsFactors = FALSE)
mydata<-mydata[mydata$Method%in%c("Double"),]
mydata$Kappa<-as.numeric(mydata$Kappa)
mydata$RMSE<-as.numeric(mydata$RMSE)

acclm<-lm(mydata$Kappa ~ mydata$RMSE)
summary(acclm)
plot(mydata$Kappa ~ mydata$RMSE, ylim=c(0,1.5), xlab="RMSE", ylab="Kappa", pch=19)
abline(1.05246, -0.699)
text(.5,1.4, as.expression(~R^2~ "= 0.5576"))

accglm<-glm(mydata$Kappa ~ mydata$RMSE, family = gaussian) # does this make any sense? 
summary(accglm)


