_________________________________________________________________________
Hredwood <- hextess(redwood, s=.2) 
plot(quadratcount(redwood, tess=Hredwood))

## Counts
Hcounts <- c(5,5,11,6,11,9,2)

m <- length(Hcounts)
ybar <- mean(Hcounts)
ssquared <- var(Hcounts)
I_Test_Statistic <- ((m-1)*ssquared)/ybar


## Computing test statistic, and two.tailed chi-squared csr test
regularpvalue <- pchisq(I_Test_Statistic, (m-1), lower.tail = TRUE)
clusterpvalue <- pchisq(I_Test_Statistic, (m-1), lower.tail = FALSE)

if (regularpvalue > clusterpvalue){
  twosidedpvalue <- 2*clusterpvalue
} else {
  twosidedpvalue <- 2*regularpvalue
}

## regularpvalue = 0.875348
## clusterpvalue = 0.124652
## twosidedpvalue = 0.249304
## I_Test_Statistic = 10



_________________________________________________________________________
Hcells <- hextess(cells, s=.2) 
plot(quadratcount(cells, tess=Hcells))

## Counts
Hcounts <- c(4,5,5,5,5,5,5)

m <- length(Hcounts)
ybar <- mean(Hcounts)
ssquared <- var(Hcounts)
I_Test_Statistic <- ((m-1)*ssquared)/ybar


## Computing test statistic, and two.tailed chi-squared csr test
regularpvalue <- pchisq(I_Test_Statistic, (m-1), lower.tail = TRUE)
clusterpvalue <- pchisq(I_Test_Statistic, (m-1), lower.tail = FALSE)

if (regularpvalue > clusterpvalue){
  twosidedpvalue <- 2*clusterpvalue
} else {
  twosidedpvalue <- 2*regularpvalue
}

## regularpvalue = 0.0001071765
## clusterpvalue = 0.9998928
## twosidedpvalue = 0.0002143529
## I_Test_Statistic = 0.1764706


_________________________________________________________________________
Hblackpine <- hextess(blackpine, s=.2)
plot(quadratcount(blackpine, tess=Hblackpine))

## Counts
Hcounts <- c(11,8,2,8,6,9,2)

m <- length(Hcounts)
ybar <- mean(Hcounts)
ssquared <- var(Hcounts)
I_Test_Statistic <- ((m-1)*ssquared)/ybar


## Computing test statistic, and two.tailed chi-squared csr test
regularpvalue <- pchisq(I_Test_Statistic, (m-1), lower.tail = TRUE)
clusterpvalue <- pchisq(I_Test_Statistic, (m-1), lower.tail = FALSE)

if (regularpvalue > clusterpvalue){
  twosidedpvalue <- 2*clusterpvalue
} else {
  twosidedpvalue <- 2*regularpvalue
}

## regularpvalue = 0.9088986
## clusterpvalue = 0.0911014
## twosidedpvalue = 0.1822028
## I_Test_Statistic = 10.91304
_________________________________________________________________________