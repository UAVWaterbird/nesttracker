## Test Figures for Anaho UAS
install.packages("ggplot2")
install.packages("plyr")
library(ggplot2)
library(plyr)
library(reshape2)

setwd("C:\\Users\\sd1249\\Documents\\Sharon\\Thesis\\Anaho_UAS\\Data\\Nesttracker\\nesttracker")
test<-read.csv("test_data.csv")
test

boxplot(Count ~ Count_Type, data=test, ylim=c(400, 900))

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
x2 + geom_point(position = position_jitter(width = 0.2))

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

#using "test" data: all count methods ground and aerial 
ggplot(test, aes(x=test$Count_Type, y= test$Count)) + geom_jitter()
# Change the position
# 0.3 : degree of jitter in x direction
strip<-ggplot(test, aes(x=test$Count_Type, y= test$Count)) + 
  geom_jitter(position=position_jitter(0.3), size=4)
strip
#try it flipped
stripflip<-strip + coord_flip()
stripflip

#add some stats
stripmean<-strip + stat_summary(fun.y=mean, geom="point", shape=18,
             size=4, color="red")
stripmean

# stripchart using kappa stats
test2<-read.csv("test_data2.csv")
test2
stripstats<-ggplot(test2, aes(x=test2$Method, y= test2$Kappa)) + 
  geom_jitter(position=position_jitter(0.3), size=4)
stripstats

test3<-read.csv("test_data3.csv")
test3
stripstats2<-ggplot(test3, aes(x=test3$Stat, y= test3$X3image)) + 
  geom_jitter(position=position_jitter(0.3), size=4)
stripstats2
