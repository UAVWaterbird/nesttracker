
gobs<-read.csv("ALLresults_TEMP.csv", stringsAsFactors = FALSE)

corfac<-gobs[gobs$Method3%in%c("ground","groundimage"),]
corfac
corfacg<-corfac[corfac$Method3%in%c("ground"),]
corfacg

plot(corfacg$Observed~corfacg$nestimate, ylim=c(0,2800), xlim=c(0,2800))
abline(1,1)

# percent error ~ distance (mean distance?) to colony 
# percent error ~ angle 
# percent error ~ size of colony 
# random effects: observer and colony 

cflm<-lm(corfacg$Observed~corfacg$nestimate)
plot(cflm)
summary(cflm)

aovcf<-aov(corfacg$Observed~corfacg$nestimate)
aovcf1<-aov(corfacg$Observed~corfacg$nestimate + corfacg$Method2)
