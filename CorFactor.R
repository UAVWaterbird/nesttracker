
gobs<-read.csv("ALLresults_TEMP.csv", stringsAsFactors = FALSE)

corfac<-gobs[gobs$Method3%in%c("ground","groundimage"),]
corfac
corfacg<-corfac[corfac$Method3%in%c("ground"),]
corfacg

plot(corfacg$nestimate~corfacg$Observed, ylim=c(0,2800), xlim=c(0,2800))
abline(1,1)


cflm<-lm(corfacg$Observed~corfacg$nestimate)
plot(cflm)
summary(cflm)

dist<-as.numeric(corfacg$DisttoColony)
cflm2<-lm(corfacg$nestimate~corfacg$Observed + dist)
plot(cflm2)
summary(cflm2)

aovcf<-aov(corfacg$Observed~corfacg$nestimate)
aovcf1<-aov(corfacg$Observed~corfacg$nestimate + corfacg$Method2)
