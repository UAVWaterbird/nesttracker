# Simulate some nests

library(spatstat)

# Number of nesters (preferably a square)
nest.n <- 100
# Mean nearest neighbor distance of nests
mean.nn <- 1
# Standard deviation of n.n.
nn.sd <- 0.2
# Image error
image.err <- 0
# Number of loafers 
loafers.n <- 10
# Create flight ID
flight.id <- "a"

## Function to generate inital nests and loafers
## [TODO]: incorporate image.err?
initiatenests <- function(nest.n, loafers.n, mean.nn, nn.sd, image.err=0,
                          flight.id="A"){
  
  # Calculate size of square grid:
  grid.size <- sqrt(nest.n)
  # Create coordinates along the grid
  grid.coords <- expand.grid(x=1:grid.size, y=1:grid.size)
  # Generate random noise
  # [TODO: adjust so that standard dev of distance = nn.sd]
  grid.noise <- rnorm(n=2*nest.n, mean=0, sd=nn.sd/2)
  grid.coords$newx <- grid.coords$x+grid.noise[1:nest.n]
  grid.coords$newy <- grid.coords$y + grid.noise[(nest.n+1):(2*nest.n)]
  
  # Create ppp with nests
  nest.pts <- as.ppp(data.frame(grid.coords$newy, grid.coords$newx), 
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
  all.pts.df$observed <- c(rep.int(1, nest.n), rep.int(0, loafers.n))
  
  # Generate IDs for each point
  ids <- seq(from=1, to=nrow(all.pts.df), length.out=nrow(all.pts.df))
  all.pts.df$UFID <- paste(flight.id, ids, sep="_")

  return(all.pts.df)
}

newflight <- function(initpts, loafers.n, mean.move, sd.move, image.err=0,
                      flight.id="B"){
  init.nests <- subset(initpts, observed==1)
  nest.n <- nrow(init.nests)
  # Generate random noise
  # [TODO: adjust so that standard dev of distance is right!]
  grid.noise <- rnorm(n=2*nest.n, mean=0, sd=sd.move)
  init.nests$newx <- init.nests$x+grid.noise[1:nest.n]
  init.nests$newy <- init.nests$y + grid.noise[(nest.n+1):(2*nest.n)]
  
  # Create ppp with nests
  nest.pts <- as.ppp(data.frame(init.nests$newy, init.nests$newx), 
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



### TEST:
new.nests <- initiatenests(nest.n=100, loafers.n=10, mean.nn=1, nn.sd=0.1)
plot(new.nests$y ~ new.nests$x)

second.nests <- newflight(new.nests, loafers.n=6, mean.move=0.01, sd.move=0.01, 
                          image.err=0, flight.id="B")
points(second.nests$y ~ second.nests$x, col="red")




