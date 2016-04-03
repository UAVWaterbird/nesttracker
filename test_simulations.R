### Function to simulate nests with a variety of parameters,
### test accuracy of reciprocal (and other?) tests

library(PresenceAbsence)

### Compare ratio of nn.sd to sd.move

# Create standard deviation of nesting nearest neighbors
nn.sd <- seq(from=0, to=1, length=50)
# Create standard deviation of between image n.n's
#   -for now, hold steady at observed sd
sd.move <- 0.01
acc.df <- NULL

for(i in 1:length(nn.sd)){
  a <- initiatenests(nest.n=100, loafers.n=10, mean.nn=1, nn.sd=nn.sd[i])
  b <- newflight(a, loafers.n=25, mean.move=0, sd.move=sd.move, image.err=0,
                 flight.id="B")
  
  a <- SpatialPointsDataFrame(data.frame(a$x, a$y), data=a)
  b <- SpatialPointsDataFrame(data.frame(b$x, b$y), data=b)
  
  a <- reciprocalnn(a, b)
  presabs.df <- data.frame(a$UFID, a$observed, a$Nesting)
  my.cmx <- cmx(presabs.df)
  pcc <- pcc(my.cmx, st.dev=F)
  sens <- sensitivity(my.cmx, st.dev=F)
  spec <- specificity(my.cmx, st.dev=F)
  nn.move.ratio <- nn.sd[i] / sd.move
  results <- data.frame(nn.sd[i], nn.move.ratio, pcc, sens, spec)
  colnames(results) <- c("nn.sd", "ratio", "pcc", "sens", "spec")
  acc.df <- rbind(acc.df, results)  
}

plot(pcc ~ ratio, data=acc.df, ylim=c(0,1))
points(acc.df$sens ~ acc.df$ratio, col="blue")
points(acc.df$spec ~ acc.df$ratio, col="red")

pcc.lm <- lm(pcc ~ ratio, data=acc.df)
summary(pcc.lm)

pcc.lm <- lm(sens ~ ratio, data=acc.df)
summary(pcc.lm)

pcc.lm <- lm(spec ~ ratio, data=acc.df)
summary(pcc.lm)

# Results: appears to be no relationship b/w accuracy and
#   nn.sd and sd.move

### Compare relationship b/w accuracy and sd.move

# Create standard deviation of nesting nearest neighbors
nn.sd <- 0
# Create standard deviation of between image n.n's
#   -for now, hold steady at observed sd
sd.move <- seq(from=0, to=10, length=100)
acc.df2 <- NULL
loafers.n1 = 100
loafers.n2 = 100
mean.nn=10

for(i in 1:length(sd.move)){
  a <- initiatenests(nest.n=100, loafers.n=loafers.n1, mean.nn=mean.nn, nn.sd=nn.sd)
  b <- newflight(a, loafers.n=loafers.n2, mean.move=0, sd.move=sd.move[i], image.err=0,
                 flight.id="B")
  
  a <- SpatialPointsDataFrame(data.frame(a$x, a$y), data=a)
  b <- SpatialPointsDataFrame(data.frame(b$x, b$y), data=b)
  
  a <- reciprocalnn(a, b)
  presabs.df <- data.frame(a$UFID, a$observed, a$Nesting)
  my.cmx <- cmx(presabs.df)
  pcc <- pcc(my.cmx, st.dev=F)
  sens <- sensitivity(my.cmx, st.dev=F)
  spec <- specificity(my.cmx, st.dev=F)
  nn.move.ratio <- sd.move[i] / mean.nn
  results <- data.frame(sd.move[i], nn.move.ratio, pcc, sens, spec)
  colnames(results) <- c("sd.move", "ratio", "pcc", "sens", "spec")
  acc.df2 <- rbind(acc.df2, results)  
}

plot(pcc ~ ratio, data=acc.df2, ylim=c(0,1))
points(acc.df2$sens ~ acc.df2$ratio, col="blue")
points(acc.df2$spec ~ acc.df2$ratio, col="red")
abline(h=0.5)

pcc.lm <- lm(pcc ~ sd.move, data=acc.df2)
summary(pcc.lm)

pcc.lm <- lm(sens ~ sd.move, data=acc.df2)
summary(pcc.lm)

pcc.lm <- lm(spec ~ sd.move, data=acc.df2)
summary(pcc.lm)

# Results: pcc goes down quickly from ~95% with increase in
#   sd.move, levels off at a little above 50% after sd.move~=1.5?
#   (when mean.nn = 1)
# Question: what is effect of ratio of mean.nn/sd.move?
# Question: what is role of prevalence (i.e. nesters vs. non-nesters)?

### Extra Code:
# Observed std. dev b/w F3 & F4, B south = 0.2206345
# (i.e. mean euclid. distance between n.n's * sqrt(2)/sqrt(pi))
nn.sd <- 0
sd.move <- 0
nest.n <- 100
loafers.n1 <- 10
loafers.n2 <- 10

a <- initiatenests(nest.n=nest.n, loafers.n=loafers.n1, mean.nn=1, nn.sd=nn.sd)
b <- newflight(a, loafers.n=loafers.n2, mean.move=0, sd.move=sd.move, image.err=0,
               flight.id="B")

a <- SpatialPointsDataFrame(data.frame(a$x, a$y), data=a)
b <- SpatialPointsDataFrame(data.frame(b$x, b$y), data=b)

a <- reciprocalnn(a, b)
presabs.df <- data.frame(a$UFID, a$observed, a$Nesting)
my.cmx <- cmx(presabs.df)
pcc <- pcc(my.cmx)
sens <- sensitivity(my.cmx)
spec <- specificity(my.cmx)
pcc
sens
spec
