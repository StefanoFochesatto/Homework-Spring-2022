# call this function with an argument (my.lonlat)
# that is a matrix with 2 columns: longitude & latitude,
# where these are the points in the region we wish to bound.

# this function returns a 2 x ? matrix with borders
# in the two columns; lon in 1st column, lat in 2nd column.

# The argument 'frac' says how far to extend in each
# direction; e.g. frac=.1 says make the border extend 10%
# in all directions beyond the convex hull of the points
# that are listed inside the region.

# if the argument 'frac' is omitted, add 15%, which seems
# to work reasonably well in various test cases.

get.borders <- function( my.lonlat, concavity_param = 1 , frac=0.15)
{
  if( !is.matrix(my.lonlat) )
    stop("get.borders expects a matrix (w/lon in 1st col, lat in 2nd)")
  if( ncol(my.lonlat) != 2 )
    stop(paste("get.borders expects a matrix with",
         " exactly 2 columns, not ",ncol(my.lonlat),"columns"))

  lons <- my.lonlat[,1]
  n.distinct.lons <- length(unique(lons))
  lats <- my.lonlat[,2]
  n.distinct.lats <- length(unique(lats))

  minlon <- min(lons);  maxlon <- max(lons)
  minlat <- min(lats);  maxlat <- max(lats)

  if( n.distinct.lons == 1 ) {
    dd <- (maxlat - minlat) * frac
    my.borders <- cbind(
     lons[1] + dd*c(-1,1,1,-1),
     c(minlat-dd,minlat-dd,maxlat+dd,maxlat+dd) )
  } else if (n.distinct.lats == 1 ) {
    dd <- (maxlon - minlon) * frac
    my.borders <- cbind(
     c(minlon-dd,maxlon+dd,maxlon+dd,minlon-dd),
     lats[1] + dd*c(-1,-1,1,1))
  } else {
  #Get concave hull from concaveman library. 
    tmp <- concaveman(my.lonlat, concavity = concavity_param)
  #Create rgeos spatial polygon object for the concave hull. 
    SP_object <- SpatialPolygons(list(Polygons(list(Polygon(tmp)), ID=sample(1e12, size=1))),
                    proj4string=CRS("+proj=merc"))
  #Use gBuffer from rgeos package to expand the concave hull. 
    largePoly <- gBuffer(SP_object, width=frac)
  #Pull coordinates from buffered polygon. 
    my.borders <- largePoly@polygons[[1]]@Polygons[[1]]@coords
  }
  return( my.borders )
}

if(FALSE) {
#  set.seed(58346)
  n <- 20
  lons <- runif(n)
  lats <- runif(n)
  yyy <- rnorm(n)
  library(geoR)
  mygeo <- as.geodata( cbind(lons,lats,yyy) )
  mygeo$borders <- get.borders( cbind(lons,lats) )
  plot(mygeo)

  n <- 20
  x <- runif(n)
  y <- rep(1,n)
  mygeo <- as.geodata( cbind( x,y,rnorm(n)),
      coords.col=1:2, data.col=3 )
  mygeo$borders <- get.borders( cbind(x,y), .1 )
  plot( mygeo )

  n <- 20
  y <- runif(n)
  x <- rep(1,n)
  mygeo <- as.geodata( cbind( x,y,rnorm(n)),
    coords.col=1:2, data.col=3 )
  mygeo$borders <- get.borders( cbind(x,y), .1 )
  plot( mygeo )
}

