dat <- structure(c(216, 99, 100, 172, 102, 126, 179, 111, 113, 210, 103, 78, 171, 105, 110, 197, 87, 109, 191, 94, 99, 223, 
                   109, 91, 185, 93, 122, 177, 107, 110, 191, 104, 90, 232, 79, 86, 171, 107, 104, 207, 83, 109, 165, 114, 123,
                   204, 111, 90, 210, 111, 86, 160, 112, 117, 208, 90, 96, 167, 103, 117, 317, 112, 96, 261, 115, 107, 279, 108, 103,
                   287, 121, 116, 330, 84, 99, 261, 104, 123, 328, 83, 85, 284, 99, 101, 298, 95, 100, 276, 119, 101, 267, 115, 94,
                   324, 90, 94, 291, 95, 103, 277, 105, 116, 329, 93, 93, 321, 88, 93, 278, 104, 109, 335, 97, 101, 294, 92, 102,
                   308, 95, 120, 291, 95, 112, 288, 103, 101, 306, 95, 106, 291, 111, 100, 297, 100, 114, 306, 103, 106, 320, 87, 85,
                   288, 103, 101, 302, 108, 91, 296, 110, 96, 400, 107, 110, 389, 110, 108, 375, 99, 104, 390, 91, 107, 375, 102,
                   100, 391, 87, 104, 387, 94, 83, 411, 100, 102, 353, 106, 118, 400, 104, 89), .Dim = c(60L,3L), .Dimnames = list(NULL, c("x1", "x2", "x3")))

dat <- as.data.frame(dat)

library(car)
library(car)

## Simulateing Clusters and Computing rSquared.
rsquared <- rep(NA, 10)
for (i in 1:10){  
  tmp <- kmeans(dat, centers = i)
  rsquared[i] <- tmp$betweenss/tmp$totss
}
plot(rsquared, type="l", lwd=2, xlab = '# of Clusters')

## Plotting data. 
scatter3d(x1~x2+x3, data=dat, neg.res.col="white",
          pos.res.col="white", surface.alpha=0.0)


## Plotting optimal (subjective) cluster.
tmp <- kmeans(dat, centers = 2)
scatter3d(x1~x2+x3, data = dat, point.col=tmp$cluster, surface = FALSE)



## 
library(ade4)

## Generating Distance
D <- dist(dat)
## Generating Dendogram
tmp <- hclust(D, method="single")
tmp2 <- hclust(D, method="complete")

## Colored Clustered Dendogram
library(factoextra)
cluster1 <- fviz_dend(tmp, cex = 0.4,
          k = 2, # Cut in four groups
          palette = "jco", # Color palette
)
plot(cluster1)

cluster2 <- fviz_dend(tmp2, cex = 0.4,
                      k = 2, # Cut in four groups
                      palette = "jco", # Color palette
)
plot(cluster2)

## Cluster Plot to show same cluster
library(dendextend)
cut1 <- cutree(tmp, k = 2)
cut2 <- cutree(tmp2, k = 2)
plot(cut2, cex = 1.25, ylab = 'Cluster')
points(cut1, col = 'blue',pch = 4, cex = .75 )
legend(0,1.5, c('Single', 'Complete'), col = c('black', 'blue'), pch = c(1, 4))

## Checking if the order is the same
isTRUE(tmp$order == tmp2$order)
## [1] FALSE


## Reading in Data
dat <- read.csv('WoodySpecies.csv', header=TRUE)
dat_t <- t(dat[,-1])
colnames(dat_t) <- dat[,1]




## Simulating Clusters and Computing rSquared.
rsquared <- rep(NA, 10)
for (i in 1:10){  
  tmp <- kmeans(dat_t, centers = i)
  rsquared[i] <- tmp$betweenss/tmp$totss
}
plot(rsquared, type="l", lwd=2, xlab = '# of Clusters')
barplot((rsquared[2:10] - rsquared[1:9]), names.arg = seq(2,10),
        ylab = 'Proportion of Var Explained (r^2)', 
        xlab = 'Number of Clusters' )

kmeansWoodySpecies <- kmeans(dat_t, centers = 4)



## Generating Distance
D <- dist(dat_t)


## Generating Dendogram
plot(HclustWoodySpecies)
cluster <- fviz_dend(HclustWoodySpecies,
                      k = 3, # Cut in three groups
                      palette = "jco", # Color palette
)

plot(cluster)
cluster <- fviz_dend(HclustWoodySpecies,
                     k = 4, # Cut in four groups
                     palette = "jco", # Color palette
)
plot(cluster)
## Comparing Clusters
kmeansWoodySpecies <- kmeans(dat_t, centers = 4)
> kmeansWoodySpecies$cluster

Cypress.Creek    Crooked.Creek      Forked.Lake Cucumber.Creek.l 
3                4                4                4 
Cucumber.Creek2      Refuge.East      Refuge.West            Caney 
4                4                4                2 
Refuge.North      H.shoe.Lake     Brick.Slough      Grassy.Lake 
2                1                1                1 
Buzzards.Roost      Holly.Creek 
1                1 


cutCompare <- cutree(HclustWoodySpecies, k = 4)

> cutCompare
Cypress.Creek    Crooked.Creek      Forked.Lake Cucumber.Creek.l 
1                2                2                2 
Cucumber.Creek2      Refuge.East      Refuge.West            Caney 
2                2                2                3 
Refuge.North      H.shoe.Lake     Brick.Slough      Grassy.Lake 
3                4                4                4 
Buzzards.Roost      Holly.Creek 
4                4 

D <- dist(dat_t)
ClustD <- cophenetic(HclustWoodySpecies)
cor(ClustD, D)
## [1] 0.8223099


D <- dist(dat_t)
HclustAverage <- hclust(D, method = 'average')
HclustSingle <- hclust(D, method = 'single')
tanglegram(HclustSingle, HclustAverage)
cor_cophenetic(HclustAverage, HclustSingle)
## [1] 0.9706721



D <- dist(dat_t)
library(cluster)
res.diana <- diana(D, stand = TRUE)

# Plot the dendrogram
library(factoextra)
fviz_dend(res.diana, cex = 0.5,
          k = 4, # Cut in four groups
          palette = "jco" # Color palette
)

tanglegram(as.dendrogram(res.diana),  HclustAverage)
cor_cophenetic(as.dendrogram(res.diana),  HclustAverage)
## [1] 0.8989625



WaterDat <- read.csv('water_potability.csv', header=TRUE)
WaterDat <- na.omit(WaterDat)
TrueClustering <- WaterDat[, length(WaterDat)]
WaterDat <- WaterDat[, -length(WaterDat)]

rsquared <- rep(NA, 10)
for (i in 1:10){  
  tmp <- kmeans(WaterDat, centers = i)
  rsquared[i] <- tmp$betweenss/tmp$totss
}
plot(rsquared, type="l", lwd=2, xlab = '# of Clusters')
barplot((rsquared[2:10] - rsquared[1:9]), names.arg = seq(2,10),
        ylab = 'Proportion of Var Explained (r^2)', 
        xlab = 'Number of Clusters' )

WaterKmeansClust <- kmeans(WaterDat, centers = 2)
WaterKmeansClust <- WaterKmeansClust$cluster - 1


kClassification <- WaterKmeansClust == TrueClustering
length(kClassification[kClassification == TRUE])
length(kClassification)
## 1062/2011 classified correctly.


D <- dist(WaterDat)
AgglomerativeClust <- hclust(D, method = 'average')
AgglomerativeClust <- cutree(AgglomerativeClust, k = 2)
AgglomerativeClust <- AgglomerativeClust - 1

aClassification <- AgglomerativeClust == TrueClustering
length(aClassification[aClassification == TRUE])
length(aClassification)
## 1188/2011 classified correctly


