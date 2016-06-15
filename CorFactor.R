
gobs<-read.csv("ALLresults_TEMP.csv", stringsAsFactors = FALSE)

corfac<-gobs[gobs$Method3%in%c("ground","groundimage"),]
corfac
corfacg<-corfac[corfac$Method3%in%c("ground"),]
corfacg



plot(corfacg$nestimate~corfacg$Observed, ylim=c(0,2800), xlim=c(0,2800), pch=19)
abline(-361.87, 1.23, col="red")
abline(1,1, col="black")

#https://heuristically.wordpress.com/2011/09/28/paired-sample-t-test-in-r/
corfacg$diff<-corfacg$nestimate - corfacg$Observed
hist(corfacg$diff)
mean(corfacg$nestimate) #752.5
mean(corfacg$Observed) #900.9
#Because n is small, distribution of differences should be about normal, but test this first
boxplot(corfacg$diff)
qqnorm(corfacg$diff)
qqline(corfacg$diff) #one outlier
shapiro.test(corfacg$diff) #p-value = 0.004937, so reject null hypothesis that values are normally distributed
#OK data is not normal, let's try a non parametric test?
#paired
wilcox.test(corfacg$nestimate, corfacg$Observed, mu=0, alt="two.sided", paired=TRUE, conf.int=T, conf.level=0.95, exact=FALSE)
#The 2 sided test p = 0.04455, true location shift is not equal to 0
wilcox.test(corfacg$nestimate, corfacg$Observed, mu=0, alt="less", paired=TRUE, conf.int=T, conf.level=0.95, exact=FALSE)

cflm<-lm(corfacg$Observed~corfacg$nestimate)
plot(cflm)
summary(cflm)


dist<-as.numeric(corfacg$DisttoColony)

plot(corfacg$PctError~dist)
Errorlm<-lm(corfacg$PctError~dist)
plot(Errorlm) #looks like this violates a lot of assumptions
summary(Errorlm)

Errorglm<-glm(corfacg$PctError~dist, family = gaussian)
summary(Errorglm)


cflm2<-lm(corfacg$nestimate~corfacg$Observed + dist)
plot(cflm2)
summary(cflm2)

#Both Poisson and quasipoisson have bad GOF
cfglm<-glm(corfacg$nestimate~corfacg$Observed, family=poisson)
summary(cfglm)
cfglm2<-glm(corfacg$nestimate~corfacg$Observed + dist, family=poisson)
summary(cfglm2)

cfqglm<-glm(corfacg$nestimate~corfacg$Observed, family = quasipoisson)
summary(cfqglm)


## Just the means (total adults) from the data
cfmeans<-read.csv("GroundCompare_Means.csv")
hist(cfmeans$totalg)
hist(cfmeans$totaluas)
boxplot(cfmeans$totalg, cfmeans$totaluas)
cfmeans$diff<-cfmeans$totalg - cfmeans$totaluas
hist(cfmeans$diff)
qqnorm(cfmeans$diff)
qqline(cfmeans$diff)
shapiro.test(cfmeans$diff) #nope, still not normal 

#Compare ground to ground imagery
plot(cfmeans$totalg~cfmeans$totali, pch=19)
abline(109.6, 0.83)
cfmeans$diffg<-cfmeans$totalg - cfmeans$totali
hist(cfmeans$diffg)
qqnorm(cfmeans$diffg)
qqline(cfmeans$diffg)
shapiro.test(cfmeans$diffg) #data not normal because of bluff north outlier
wilcox.test(cfmeans$totalg, cfmeans$totali, mu=0, alt="two.sided", paired=TRUE, conf.int=T, conf.level=0.95, exact=FALSE)


