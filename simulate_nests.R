# Simulate some nests

library(spatstat)

## Function to generate inital nests and loafers
## [TODO]: incorporate image.err?
##    -if image.err, it will be incorporated as mean in generating
##      grid.noise?
## [TODO]: how are loafers/attending mates dispersed within colony?
##    -based on initial simulations, not randomly!

## NOTE: if nest.n is not a perfect square, the nest.n will be rounded
## to closest one (for now)
initiatenests <- function(nest.n, loafers.n, mean.nn, nn.sd, image.err=0,
                          flight.id="A"){
  
  # Calculate size of square grid:
  grid.size <- sqrt(nest.n)
  # Create coordinates along the grid
  grid.coords <- expand.grid(x=(1:grid.size)*mean.nn, y=(1:grid.size)*mean.nn)
  # Generate random noise
  grid.noise <- rnorm(n=nrow(grid.coords), mean=0, sd=nn.sd)
  noise.angles <- runif(n=nrow(grid.coords), min=0, max=2*pi) 
  grid.coords$newx <- grid.coords$x + grid.noise*sin(noise.angles)
  grid.coords$newy <- grid.coords$y + grid.noise*cos(noise.angles)
  
  # REMOVE:
  #grid.coords$diffx <- grid.coords$x - grid.coords$newx
  #grid.coords$diffy <- grid.coords$y - grid.coords$newy
  #grid.coords$dist <- sqrt(grid.coords$diffx^2 + grid.coords$diffy^2)
  
  # Create ppp with nests
  nest.pts <- as.ppp(data.frame(grid.coords$newx, grid.coords$newy), 
                    W=owin(xrange=c((min(grid.coords$newx)-mean.nn), 
                                     (max(grid.coords$newx)+mean.nn)), 
                                     yrange=c((min(grid.coords$newy)-mean.nn), 
                                               (max(grid.coords$newy)+mean.nn))))
  # Generate loafers
  loafers <- runifpoint(n=loafers.n, win=nest.pts$window)
  
  # Combine nests and loafers
  all.pts.ppp <- superimpose(nest.pts, loafers)
  all.pts.df <- as.data.frame(all.pts.ppp)
  
  # Mark points as either nesting or non-nesting
  all.pts.df$observed <- c(rep.int(1, nrow(grid.coords)), rep.int(0, loafers.n))
  
  # Generate IDs for each point
  ids <- seq(from=1, to=nrow(all.pts.df), length.out=nrow(all.pts.df))
  all.pts.df$UFID <- paste(flight.id, ids, sep="_")

  return(all.pts.df)
}

### [TODO]: incorporate image error?
### Or is there a difference b/w mean.move and image.err? 
### sd.move captures the between-image error associated with
### birds moving on the nest, the mean.move implies some specific 
### spatial bias with the image. If mean.move != 0, that implies
### a directional movement by all nests

newflight <- function(initpts, loafers.n, mean.move=0, sd.move, image.err=0,
                      flight.id="B"){
  init.nests <- subset(initpts, observed==1)
  nest.n <- nrow(init.nests)
  # Generate random noise
  grid.noise <- rnorm(n=nest.n, mean=mean.move, sd=sd.move)
  noise.angles <- runif(n=nest.n, min=0, max=2*pi) 
  init.nests$newx <- init.nests$x + grid.noise*sin(noise.angles)
  init.nests$newy <- init.nests$y + grid.noise*cos(noise.angles)
  
  # REMOVE:
  #init.nests$diffx <- init.nests$x - init.nests$newx
  #init.nests$diffy <- init.nests$y - init.nests$newy
  #init.nests$dist <- sqrt(init.nests$diffx^2 + init.nests$diffy^2)
  
  # Create ppp with nests
  nest.pts <- as.ppp(data.frame(init.nests$newx, init.nests$newy), 
                     W=owin(xrange=c((min(init.nests$newx)-mean.nn), 
                                     (max(init.nests$newx)+mean.nn)), 
                            yrange=c((min(init.nests$newy)-mean.nn), 
                                     (max(init.nests$newy)+mean.nn))))
  # Generate loafers
  loafers <- runifpoint(n=loafers.n, win=nest.pts$window)
  
  # Combine nests and loafers
  all.pts.ppp <- superimpose(nest.pts, loafers)
  all.pts.df <- as.data.frame(all.pts.ppp)
  
  # Mark points as either nesting or non-nesting
  all.pts.df$observed <- c(rep.int(1, nest.n), rep.int(0, loafers.n))
  
  # Generate IDs for each point
  ids <- seq(from=1, to=nrow(all.pts.df), length.out=nrow(all.pts.df))
  all.pts.df$UFID <- paste(flight.id, ids, sep="_")
  
  return(all.pts.df)
}

#### REMOVE:
#### Test SD 
# Under the half normal, E[Y]= mu = sigma*sqrt(2)/(sqrt(pi))
# so sigma = mu*sqrt(pi)/sqrt(2)
# st.dev(Y) = sqrt(s^2*(1-2/pi))

test.m <- 0
test.sd <- 0.1
test.size <- 100
test.table <- expand.grid(x=1:test.size, y=1:test.size)

#origin.col <- rep(0, test.size)
#test.table <- data.frame(origin.col, origin.col)
colnames(test.table) <- c("x", "y")

test.dist <- rnorm(n=test.size^2, mean=test.m, sd=test.sd)
test.angles <- runif(n=test.size^2, min=0, max=2*pi) 
test.table$newx <- test.table$x + test.dist*sin(test.angles)
test.table$newy <- test.table$y + test.dist*cos(test.angles)
test.table$diffx <- test.table$x - test.table$newx
test.table$diffy <- test.table$y - test.table$newy
test.table$dist <- sqrt(test.table$diffx^2 + test.table$diffy^2)
sd(test.table$dist)

#plot(test.table$y ~ test.table$x)
#points(test.table$newy ~ test.table$x, col="red")

plot(test.table$x ~ test.table$diffx)
test.negs <- sample(c(-1,1), size=(test.size^2), replace=T)
dist <- test.table$dist * test.negs
hist(dist)
sd(dist)

# Compare expected / observed means:
#mu = sigma*sqrt(2)/(sqrt(pi))
test.sd*sqrt(2)/sqrt(pi) / mean(test.table$dist)

# Compare expected / observed standard deviations:
sqrt((test.sd^2)*(1-2/pi)) / sd(test.table$dist)

mean(ActiveNest$dist)/(sd.move*sqrt(2)/sqrt(pi))

sd(ActiveNest$dist)/sqrt((sd.move^2)*(1-2/pi))
