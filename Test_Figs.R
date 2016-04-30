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

test.means <- aggregate(test$Count,
                    by = list(method = test$Count_Type),
                    FUN = function(x) c(mean = mean(x), sd = sd(x),
                                        n = length(x)))

test.means <- do.call(data.frame, test.means)
test.means$se <- test.means$x.sd / sqrt(test.means$x.n)
colnames(test.means) <- c("Count_Type", "mean", "sd", "n", "se")
test.means$names <- c(paste(test.means$Count_Type))


plotTop <- max(test.means$mean) +
  test.means[test.means$mean == max(test.means$mean), 5] * 3

barCenters <- barplot(height = test.means$mean,
                      names.arg = test.means$names,
                      beside = true, las = 2,
                      ylim = c(0, plotTop),
                      cex.names = 0.75, xaxt = "n",
                      main = "Estimated Number of Active Nests by Method",
                      ylab = "Count of Active Nests",
                      border = "black", axes = TRUE)

# Specify the groupings. We use srt = 45 for a
# 45 degree string rotation
text(x = barCenters, y = par("usr")[1] - 0, srt = 0,
     adj = c(0.5,2), labels = test.means$names, xpd = TRUE)

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
bnorth_groundVaerial<-strip1 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("B North Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bnorth$Observed, linetype=2) #Add Observed Values 
bnorth_groundVaerial

#bsouth strip
strip2<-ggplot(bsouth, aes(x=bsouth$Method, y= bsouth$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip2
  #add stats
bsouth_groundVaerial<-strip2 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("B South Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bsouth$Observed, linetype=2) #Add Observed Values 
bsouth_groundVaerial

#bluffs strip
strip3<-ggplot(bluffs, aes(x=bluffs$Method, y= bluffs$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip3
  #add stats
bluffs_groundVaerial<-strip3 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("Bluff South Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bluffs$Observed, linetype=2) #Add Observed Values 
bluffs_groundVaerial

#bluffn strip
strip4<-ggplot(bluffn, aes(x=bluffn$Method, y= bluffn$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip4
  #add stats
bluffn_groundVaerial<-strip4 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("Bluff North Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = bluffn$Observed, linetype=2) #Add Observed Values 
bluffn_groundVaerial

#saddle strip
strip5<-ggplot(saddle, aes(x=saddle$Method, y= saddle$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip5
  #add stats
saddle_groundVaerial<-strip5 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("Saddle Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = saddle$Observed, linetype=2) #Add Observed Values 
saddle_groundVaerial

#c strip
strip6<-ggplot(c, aes(x=c$Method, y= c$nestimate)) + 
  geom_jitter(position=position_jitter(0.3), size=2)
strip6
  #add stats
c_groundVaerial<-strip6 + stat_summary(fun.y=mean, geom="point", shape=18,
                                size=3, color="red") + ggtitle("C Colony") +
  labs(x="Method",y="Nesting Bird Estimate") + geom_hline(yintercept = c$Observed, linetype=2) #Add Observed Values 
c_groundVaerial


#put all colonies together


grid.arrange(bnorth_groundVaerial, bsouth_groundVaerial, bluffs_groundVaerial, bluffn_groundVaerial, saddle_groundVaerial, c_groundVaerial, ncol=2)



# stripchart using kappa stats
test2<-read.csv("test_data2.csv")
test2
stripstats<-ggplot(test2, aes(x=test2$Method, y= test2$Kappa)) + 
  geom_jitter(position=position_jitter(0.3), size=4)
stripstats + ylim(0, 1)

test3<-read.csv("test_data3.csv")
test3
stripstats2<-ggplot(test3, aes(x=test3$Stat, y= test3$X3image)) + 
  geom_jitter(position=position_jitter(0.3), size=4)
stripstats2
