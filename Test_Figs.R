## Test Figures for Anaho UAS
install.packages("ggplot2")
install.packages("plyr")
library(ggplot2)
library(plyr)
library(reshape2)
library(gridExtra)

setwd("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Nesttracker\\nesttracker")
test<-read.csv("ALLresults_TEMP.csv")
test
test$NestimateStd<-test$nestimate/test$Observed
test$order<-ifelse(test$Method=="Single",1,
       ifelse(test$Method=="Double", 2,
              ifelse(test$Method=="Triple",3,"na")))
as.factor(test$order)

#observed <- test$Count[test$Count_Type=="Observed"]
#test <- subset(test, Count_Type != "Observed")
#test$Count_Type <- droplevels(test$Count_Type)

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


###############################################################################3
#strip charts in ggplot2
bsouth<-subset(test, test$colony=="bsouth")
bnorth<-subset(test, test$colony=="bnorth")
bluffs<-subset(test, test$colony=="bluffs")
bluffn<-subset(test, test$colony=="bluffn")
saddle<-subset(test, test$colony=="saddle")
c<-subset(test, test$colony=="c")


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
test<-subset(test[which(test$Kappa!="na"),])
#Order the Method by Single, Double, Triple instead of alphabetically
test$order<-ifelse(test$Method=="Single",1,
                   ifelse(test$Method=="Double", 2,
                          ifelse(test$Method=="Triple",3,"na")))
as.factor(test$order)
test$Method <- factor(test$Method, levels = test$Method[order(test$order)])
  
  #Kappa
kappastats<-ggplot(test, aes(x=test$Method, y=as.numeric(test$Kappa))) + 
  geom_boxplot()  + ggtitle("Kappa by Method") +
  labs(x="Method",y="Kappa Score") +geom_jitter(shape=16, position=position_jitter(0.2))
kappastats

  #PCC
PCCstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$PCC))) + 
  geom_boxplot()  + ggtitle("Percent Correctly Classified by Method") +
  labs(x="Method",y="PCC") +geom_jitter(shape=16, position=position_jitter(0.2))
PCCstats + ylim(0,1)

  #Sensitivity
sensstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$sensitivity))) + 
  geom_boxplot()  + ggtitle("Sensitivity by Method") +
  labs(x="Method",y="Sensitivity") +geom_jitter(shape=16, position=position_jitter(0.2))
sensstats

  #Specificity
specstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$specificity))) + 
  geom_boxplot()  + ggtitle("Specificity by Method") +
  labs(x="Method",y="Specificity") +geom_jitter(shape=16, position=position_jitter(0.2))
specstats

  #AUC
aucstats<-ggplot(test, aes(x=test$Method, y= as.numeric(test$AUC))) + 
  geom_boxplot()  + ggtitle("Area Under the Curve Scores by Method") +
  labs(x="Method",y="AUC") +geom_jitter(shape=16, position=position_jitter(0.2))
aucstats

#Grid of all statistics
grid.arrange(kappastats, PCCstats, sensstats, specstats, aucstats, ncol=2)

######################################################################################################
# Boxplots of stats for within day vs. across 2 days 

test1<-test[test$Method1%in%c("SameDay","AcrossDay"),]


#Kappa
kappastats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$Kappa))) + 
  geom_boxplot()  + ggtitle("Kappa") +
  labs(x="Method",y="Kappa Score") +geom_jitter(shape=16, position=position_jitter(0.2))
kappastats1 + ylim(0,1)

#PCC
PCCstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$PCC))) + 
  geom_boxplot()  + ggtitle("Percent Correctly Classified") +
  labs(x="Method",y="PCC") +geom_jitter(shape=16, position=position_jitter(0.2))
PCCstats1

#Sensitivity
sensstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$sensitivity))) + 
  geom_boxplot()  + ggtitle("Sensitivity") +
  labs(x="Method",y="Sensitivity") +geom_jitter(shape=16, position=position_jitter(0.2))
sensstats1

#Specificity
specstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$specificity))) + 
  geom_boxplot()  + ggtitle("Specificity") +
  labs(x="Method",y="Specificity") +geom_jitter(shape=16, position=position_jitter(0.2))
specstats1

#AUC
aucstats1<-ggplot(test1, aes(x=test1$Method1, y= as.numeric(test1$AUC))) + 
  geom_boxplot()  + ggtitle("Area Under the Curve Scores") +
  labs(x="Method",y="AUC") +geom_jitter(shape=16, position=position_jitter(0.2))
aucstats1

#Grid of all statistics
grid.arrange(kappastats1, PCCstats1, sensstats1, specstats1, aucstats1, ncol=2)
###########################################################################################
# Display count method (ground v UAS) by percent error

Errors<-read.csv("Errormeans.csv")

Method<-Errors$Method

PctError <- ggplot(Errors, aes(colour=Method, y=Errors$mean, x=Errors$Colony)) + geom_point()
PctError

limits <- aes(ymax = Errors$mean + Errors$sd, ymin=Errors$mean - Errors$sd)

#Change colors to color blind friendly (http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/)
PctError+ geom_point(size=3) + geom_errorbar(limits, width=0.2, size=1) +
  labs(x="Colony",y="Percent Error") + scale_fill_discrete(name="Method")
#OR
PctError <- ggplot(Errors, aes(shape=Method, y=Errors$mean, x=Errors$Colony)) + geom_point()
PctError
PctError+ geom_point(size=4) + geom_errorbar(limits, width=0.1, size=1) +
  labs(x="Colony",y="Percent Error") + scale_fill_discrete(name="Method")






