library(geoR)
set.seed(32123) # for reproducibility
n <- 60
lons <- runif( n, min=90, max=110 )
lats <- runif( n, min=35, max=45 )
y <- 30.0 + 1.3*(lons-100) + 1.5*(lats-40) + rnorm(n,mean=0,sd=1.6)
mydata <- as.geodata( cbind(lons,lats,y) )
plot(mydata)