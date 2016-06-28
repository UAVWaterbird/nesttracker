## Test Figures for Anaho UAS
install.packages("ggplot2")
install.packages("plyr")
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)
library(GISTools)

setwd("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Nesttracker\\nesttracker")
test<-read.csv("ALLresults_TEMP.csv")
test
test$NestimateStd<-test$nestimate/test$Observed
test$order<-ifelse(test$Method=="Single",1,
       ifelse(test$Method=="Double", 2,
              ifelse(test$Method=="Triple",3,"na")))
as.factor(test$order)

observed <- test$Count[test$Count_Type=="Observed"]
test <- subset(test, Count_Type != "Observed")
test$Count_Type <- droplevels(test$Count_Type)

boxplot(Count ~ Count_Type, data=test, ylim=c(400, 900))
abline(h=observed, lty=2)

x<-test$Sub_Type

dotchart(test$Count,labels=row.names(x),cex=.7,
         main="Number of Active Nests by Method", 
         xlab="Estimated Number of Active Nests")

###################################################################################################
# fun with boxplots in ggplot
#http://www.zijdeman.nl/files/r_examples/boxplot_example.html

x<-ggplot(test, aes(factor(test$Count_Type), test$Count))
x1<- x + geom_boxplot(outlier.shape=3)
x1
x1 + geom_point(position = position_jitter(width = 0.2))
x2 <- x + geom_boxplot(outlier.colour = NA)
x2 + geom_point(position = position_jitter(width = 0.2)) +
  geom_hline(yintercept = 811)



##############################################################################################
# test some barplot code
#http://www.r-bloggers.com/building-barplots-with-error-bars/


test.means <- aggregate(test$NestimateStd,
                    by = list(method = test$Method3),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))

test.means <- do.call(data.frame, test.means)
test.means$se <- test.means$x.sd / sqrt(test.means$x.n)
colnames(test.means) <- c("Method", "mean", "sd", "n", "se")
test.means$names <- c(paste(test.means$Method))


plotTop <- max(test.means$mean) +
  test.means[test.means$mean == max(test.means$mean), 5] * 3

barCenters <- barplot(height = test.means$mean,
                      names.arg = test.means$names,
                      beside = true, las = 2,
                      ylim = c(0, plotTop),
                      cex.names = 0.75, xaxt = "n",
                      main = "Estimated Number of Active Nests: Ground vs. UAS",
                      ylab = "Count of Active Nests",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[1] - 0, srt = 0,
     adj = c(0.5,3), labels = test.means$names, xpd = TRUE)

segments(barCenters, test.means$mean - test.means$se * 2, barCenters,
         test.means$mean + test.means$se * 2, lwd = 1.5)

arrows(barCenters, test.means$mean - test.means$se * 2, barCenters,
       test.means$mean + test.means$se * 2, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)


### comparisons of mean totals from ground, ground image, and uas

adultmeans<-read.csv("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\GroundCount_Comparison\\TotalMeans.csv")
limits<-aes(ymax=adultsmeans$Mean + adultmeans$SD, ymin = adultmeans$Mean - adultmeans$SD)
dodge<-position_dodge(width=0.9)
am<-ggplot(adultsmeans, aes(fill=adultmeans$Method, y=adultmeans$Mean, x=adultmeans$Colony, ordered=T))
am + geom_bar(stat="identity", position="dodge") + geom_errorbar(limits, position =dodge, width = 0.25)
#that's kind of ugly...


###############################################################################3
#strip charts in ggplot2
bsouth<-subset(test, test$colony=="bsouth")
bnorth<-subset(test, test$colony=="bnorth")
bluffs<-subset(test, test$colony=="bluffs")
bluffn<-subset(test, test$colony=="bluffn")
saddle<-subset(test, test$colony=="saddle")
c<-subset(test, test$colony=="c")
sslope<-subset(test, test$colony=="sslope")
amf<-subset(test, test$colony=="amushfiss")
afe<-subset(test, test$colony=="afisseast")
arm<-subset(test, test$colony=="arocksmush")
d<-subset(test, test$colony=="d")


#bnorth strip
strip1<-ggplot(bnorth, aes(x=bnorth$Method, y= bnorth$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip1
  #add stats
bnorth_aerial<-strip1 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("B North Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bnorth$Observed, linetype=2) #Add Observed Values 
bnorth_aerial

#bsouth strip
strip2<-ggplot(bsouth, aes(x=bsouth$Method, y= bsouth$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip2
  #add stats
bsouth_aerial<-strip2 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("B South Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bsouth$Observed, linetype=2) #Add Observed Values 
bsouth_aerial

#bluffs strip
strip3<-ggplot(bluffs, aes(x=bluffs$Method, y= bluffs$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip3
  #add stats
bluffs_aerial<-strip3 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("Bluff South Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bluffs$Observed, linetype=2) #Add Observed Values 
bluffs_aerial

#bluffn strip
strip4<-ggplot(bluffn, aes(x=bluffn$Method, y= bluffn$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip4
  #add stats
bluffn_aerial<-strip4 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("Bluff North Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bluffn$Observed, linetype=2) #Add Observed Values 
bluffn_aerial

#saddle strip
strip5<-ggplot(saddle, aes(x=saddle$Method, y= saddle$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip5
  #add stats
saddle_aerial<-strip5 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("Saddle Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = saddle$Observed, linetype=2) #Add Observed Values 
saddle_aerial

#c strip
strip6<-ggplot(c, aes(x=c$Method, y= c$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip6
  #add stats
c_aerial<-strip6 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("C Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = c$Observed, linetype=2) #Add Observed Values 
c_aerial


#put all colonies together
grid.arrange(bnorth_aerial, bsouth_aerial, bluffs_aerial, bluffn_aerial, saddle_aerial, c_aerial, ncol=2)

#For all colonies 
stripAll<-ggplot(test, aes(x=test$Method, y= test$NestimateStd)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
stripAll
#add stats
All_aerial<-stripAll + stat_summary(fun.y=mean, geom="point", shape=18,
                                            size=3, color="red") + ggtitle("Nest Estimates Standardized Across Colonies") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = 1, linetype=2)
All_aerial


#######################################################################################################

# Boxplots of stats 
test<-read.csv("ALLresults_TEMP.csv", stringsAsFactors = FALSE)
test
#remove na values 
test<-subset(test[which(test$TSS!="na"),])
#Order the Method by Single, Double, Triple instead of alphabetically
test$order<-ifelse(test$Method=="Single",1,
                   ifelse(test$Method=="Double", 2,
                          ifelse(test$Method=="Triple",3,"na")))
as.factor(test$order)
test$Method <- factor(test$Method, levels = test$Method[order(test$order)])
  

#TSS

tssstats<-ggplot(test, aes(x=test$Method, y=as.numeric(test$TSS))) + 
  geom_boxplot()  + ggtitle("TSS") +
  labs(x="Method",y="TSS") +geom_jitter(shape=16, position=position_jitter(0.2))
tssstats
tssstats01<-tssstats + ylim(0,1)
tssstats01


  #Kappa
kappastats<-ggplot(test, aes(x=test$Method, y=as.numeric(test$Kappa))) + 
  geom_boxplot()  + ggtitle("Kappa") +
  labs(x="Method",y="Kappa") +geom_jitter(shape=16, position=position_jitter(0.2))
kappastats
kappastats01<-kappastats + ylim(0,1)
kappastats01
  #PCC
PCCstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$PCC))) + 
  geom_boxplot()  + ggtitle("Percent Correctly Classified") +
  labs(x="Method",y="PCC") +geom_jitter(shape=16, position=position_jitter(0.2))
PCCstats
PCCstats01<-PCCstats + ylim(0,1)
PCCstats01
  #Sensitivity
sensstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$sensitivity))) + 
  geom_boxplot()  + ggtitle("Sensitivity") +
  labs(x="Method",y="Sensitivity") +geom_jitter(shape=16, position=position_jitter(0.2))
sensstats
sensstats01<-sensstats + ylim(0,1)
sensstats01
  #Specificity
specstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$specificity))) + 
  geom_boxplot()  + ggtitle("Specificity") +
  labs(x="Method",y="Specificity") +geom_jitter(shape=16, position=position_jitter(0.2))
specstats
specstats01<-specstats + ylim(0,1)
specstats01

  #AUC
aucstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$AUC))) + 
  geom_boxplot()  + ggtitle("Area Under the Curve Scores") +
  labs(x="Method",y="AUC") +geom_jitter(shape=16, position=position_jitter(0.2))
aucstats
aucstats01<-aucstats + ylim(0,1)
aucstats01

#Grid of all statistics
grid.arrange(tssstats, kappastats, PCCstats, sensstats, specstats, aucstats, ncol=2)
# Grid with ylim 0,1
grid.arrange(tssstats01, kappastats01, PCCstats01, sensstats01, specstats01, aucstats01, ncol=2)
######################################################################################################
# Boxplots of stats for within day vs. across 2 days 

test1<-test[test$Method1%in%c("SameDay","AcrossDay"),]
test1$TSS<-as.numeric(test1$TSS)


#TSS
tssstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$TSS))) + 
  geom_boxplot()  + ggtitle("TSS") +
  labs(x="Method",y="TSS") +geom_jitter(shape=16, position=position_jitter(0.2))
tssstats1
tssstats101<-tssstats1 + ylim(0,1)
tssstats101

#Kappa
kappastats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$Kappa))) + 
  geom_boxplot()  + ggtitle("Kappa") +
  labs(x="Method",y="Kappa Score") +geom_jitter(shape=16, position=position_jitter(0.2))
kappastats1
kappastats101<-kappastats1 + ylim(0,1)
kappastats101
#PCC
PCCstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$PCC))) + 
  geom_boxplot()  + ggtitle("Percent Correctly Classified") +
  labs(x="Method",y="PCC") +geom_jitter(shape=16, position=position_jitter(0.2))
PCCstats1
PCCstats101<-PCCstats1 + ylim(0,1)
PCCstats101
#Sensitivity
sensstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$sensitivity))) + 
  geom_boxplot()  + ggtitle("Sensitivity") +
  labs(x="Method",y="Sensitivity") +geom_jitter(shape=16, position=position_jitter(0.2))
sensstats1
sensstats101<-sensstats1 + ylim(0,1)
sensstats101

#Specificity
specstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$specificity))) + 
  geom_boxplot()  + ggtitle("Specificity") +
  labs(x="Method",y="Specificity") +geom_jitter(shape=16, position=position_jitter(0.2))
specstats1
specstats101<-specstats1 + ylim(0,1)
specstats101

#AUC
aucstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$AUC))) + 
  geom_boxplot()  + ggtitle("Area Under the Curve Scores") +
  labs(x="Method",y="AUC") +geom_jitter(shape=16, position=position_jitter(0.2))
aucstats1
aucstats101<-aucstats1 + ylim(0,1)
aucstats101

#Grid of all statistics
grid.arrange(tssstats1, kappastats1, PCCstats1, sensstats1, specstats1, aucstats1, ncol=2)
# Grid when scaled to 0,1
grid.arrange(tssstats101, kappastats101, PCCstats101, sensstats101, specstats101, aucstats101, ncol=2)

###########################################################################################
# Display count method (ground v UAS) by percent error

Errors<-read.csv("Errormeans.csv")
errors1<-Errors[Errors$Method%in%c("ground"),]
errors2<-Errors[Errors$Method%in%c("groundimage"),]
#errors3<-Errors[Errors$Method%in%c("UAS"),]
limits1 <- aes(ymax = errors1$mean + errors1$sd, ymin=errors1$mean - errors1$sd)
limits2 <- aes(ymax = errors2$mean + errors2$sd, ymin=errors2$mean - errors2$sd)
#limits3 <- aes(ymax = errors3$mean + errors3$sd, ymin=errors3$mean - errors3$sd)

PctError1 <- ggplot(errors1, aes(y=errors1$mean, x=errors1$Colony)) + geom_point(shape = 16, size=3) + 
  geom_errorbar(limits1, width = 0.2) + labs(title="Ground Count", 
                                             x = "Colony", y = "Percent Error from Observed") + 
  scale_y_continuous(limits = c(0,100))+theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))
PctError1

PctError2 <- ggplot(errors2, aes(y=errors2$mean, x=errors2$Colony)) + geom_point(shape = 15, size=3) + 
  geom_errorbar(limits2, width = 0.2) + labs(title="Ground Image", x = "Colony", y = "Percent Error from Observed")+
  scale_y_continuous(limits = c(0,100))+theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))
PctError2

#PctError3 <- ggplot(errors3, aes(y=errors3$mean, x=errors3$Colony)) + geom_point(shape = 18, size=3) + 
 # geom_errorbar(limits3, width = 0.2) + labs(title="UAS", x = "Colony", y = "Percent Error from Observed") + scale_y_continuous(limits = c(0,50))
#PctError3

grid.arrange(PctError1, PctError2, ncol=2)
#############################################################################################

#Boxplots to compare Kappa means by flight intervals

daycomp<-read.csv("TwoDayResults_TEMP.csv")

tH<-subset(daycomp, daycomp$Method == "2hours")
tfH<-subset(daycomp, daycomp$Method == "24hours")
tsH<-subset(daycomp, daycomp$Method == "26hours")

par(mfrow=c(1,3))

boxplot(tH$TSS, tfH$TSS, ylim=c(0,1), ylab = "TSS", xlab="Flight Intervals", names=c("2 hours", "24 hours"))
boxplot(tH$TSS, tsH$TSS, ylim=c(0,1), ylab = "TSS", xlab="Flight Intervals", names=c("2 hours", "26 hours"))
boxplot(tfH$TSS, tsH$TSS, ylim=c(0,1), ylab = "TSS", xlab="Flight Intervals", names=c("24 hours", "26 hours"))


#if you just want the background, do theme(panel.background = element_rect(colour = 'white')

#theme(axis.text=element_text(size=20, colour = 'black'),
 #     axis.title=element_text(size=24, colour = 'black'),
  #    axis.line.x = element_line(size = 1, colour = 'black'),
   #   axis.line.y = element_line(size = 1, colour = 'black'),
    #  axis.ticks = element_line(size = 1, colour = 'black'),
     # axis.ticks.length = unit(.3, 'cm'),
     # panel.background = element_rect(fill = 'white'),
    #  legend.background = element_rect(colour = 'white'),
    #  legend.key = element_rect(fill = 'white'),
    #  legend.text = element_text(size = 20),
    #  legend.key.size = unit(1.5, "cm")) 

###############################################################################################################
# Plot RMSE
mydata<-read.csv("RMSE_Colony.csv", stringsAsFactors = FALSE)
#mydata<-mydata[mydata$Method%in%c("Triple"),]
mydata$TSS<-as.numeric(mydata$Mean_TSS)
#mydata$Kappa<-as.numeric(mydata$Kappa)
mydata$maxRMSE<-as.numeric(mydata$maxRMSE)

acclm<-lm(mydata$TSS ~ mydata$maxRMSE)
summary(acclm)
-0.052193 - (1.96*0.006873)
-0.052193 + (1.96*0.006873)
plot(mydata$TSS ~ mydata$maxRMSE, ylim=c(0.9,1.1), xlab="RMSE", ylab="TSS", pch=16)
abline(1.014303, -0.052193)
text(0.9,1.09, as.expression(~R^2~ "= 0.5397"))

#Try it in GGplot
rmseplot<-ggplot(mydata, aes(mydata$RMSE, mydata$Kappa, colour=mydata$colony, group=mydata$colony)) + geom_point()
rmseplot

boxplot(mydata$RMSE~mydata$Method1)
head(mydata)

daycomp<-read.csv("TwoDayResults_TEMP.csv")
boxplot(daycomp$RMSE ~ daycomp$Method, xlab = RMSE,  ylab = "RMSE")




##############################################################################################################
# plot 

a <- readOGR(dsn="./TestData", layer="AWPE_F4_300_ARocksMush")

pointsa <- as.ppp(a@coords, W=owin(xrange=c(a@bbox[1,1], a@bbox[1,2]), 
                                   yrange=c(a@bbox[2,1], a@bbox[2,2])))
pointsnn <- nndist(pointsa, k=1, by=marks(a))
a$dist <- pointsnn

# Set new column values to appropriate colours
a$color[a$dist<0.7]="red"
a$color[a$dist>0.7]="dark gray"
plot(a, col=a$color, pch=20, cex=a$dist)
north.arrow(285690, 4425609, km2ft(0.0009), col="dark gray", cex = 0.5)
legend("topleft", pch=c(20,20), col=c("red","dark gray"), c("<0.7m", ">0.7m"), cex=.8, box.col="white")

OR

a$color[a$dist<0.7]="red"
a$color[a$dist>0.7]="dark gray"
plot(a, col=a$color, pch=20)
north.arrow(285600, 4425609, km2ft(0.0009), col="dark gray", cex = 0.5)
legend("topleft", pch=c(20,20), col=c("red","dark gray"), c("<0.7m", ">0.7m"), bty="o", cex=.8, box.col="white")



