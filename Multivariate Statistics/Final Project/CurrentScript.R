Train <- read.csv('ptarmigangbif_trainFH2OVRLcPRESBACK.csv')
Train <- Train[,3:14]
Train <- na.omit(Train)

Test <- read.csv('ptarmigan_transect_test.csv')
Lattice <- read.csv('regpoints1000FH2seloOVRLbXYc.csv')

TestLatLong <- read.csv('PresenceWLatLong')
LatticeLatlong <- read.csv('LatticeWLatLong')



## Generating Figures for Enviornmental Layers
#####################################################
g <- list(
  projection = list(type = 'mercator'),
  showland = TRUE,
  landcolor = toRGB("gray95"),
  subunitcolor = toRGB("gray85"),
  countrycolor = toRGB("gray85"),
  countrywidth = 0.5,
  subunitwidth = 0.5,
  resolution = 50, 
  visible = F,
  lonaxis = list(
    range = c(-31, -6)
  ),
  lataxis = list(
    range = c(63, 67)
  )
)

fig <- plot_geo(LatticeLatlong, lat = ~lat, lon = ~long)
fig <- fig %>% add_markers(
  color = ~NDVImax250, symbol = I("square"), size = I(1))

fig <- fig %>% colorbar(title = "NDVImax250")
fig <- fig %>% layout(
  title = 'Iceland NDVImax250', geo = g
)



library(htmlwidgets)
# Save viewer settings (e.g. RStudio viewer pane)
op <- options()
# Set viewer to web browser
options(viewer = NULL)
# Use web browser to save image
fig %>% htmlwidgets::onRender(
  "function(el, x) {
  var gd = document.getElementById(el.id); 
  Plotly.downloadImage(gd, {format: 'png', width:3000 , height:1500 , filename: 'RockPtarmiganNDVI'});
  }"
)

# Restore viewer to old setting (e.g. RStudio)
options(viewer = op$viewer)

#####################################################
## Binding Together The data.
library(sp)
library(gstat)
x <- LatticeLatlong$long
y <- LatticeLatlong$lat
Data <- cbind(x, y, LatticeLatlong[,4:14])
Data <- Data[,-c(7,8)]
Data$remoteness <- as.factor(Data$remoteness) 
Data$soil250 <- as.factor(Data$soil250)
Data$veg250 <- as.factor(Data$veg250)
str(Data)



## log(Data$JJA_ppt_av)
## log(Data$JJA_mean_w)



## Converting to SP object, Since area is so large eculidan distances with 
## computed with projected coordinates can be problematic. Set projection, in SP object. 
crs = CRS("+init=epsg:4326")
coordinates(Data) <- ~ x + y
proj4string(Data) <- crs
summary(Data)


## Subsampling the data observations for computable matrices
Data.Subsample<- Data[ seq(1, length(Data$NDVImax250), by=100),]

crs = CRS("+init=epsg:4326")
coordinates(Data.Subsample) <- ~ x + y
proj4string(Data.Subsample) <- crs
summary(Data)

## Generating Empirical SemiVariograms and Empirical Pseudo Cross Variograms. 
## Trend Analysis was done by computing each Empirical Semvariogram and seeing which 
## would give the easiest fit/best stationarity. 
library(gstat)
g <- gstat(NULL, id = "NDVImax250", form = scale(NDVImax250) ~ x + y + (x^2) + (y^2) + x*y, data=Data.Subsample)
g <- gstat(g, id = "JJA_tavg25", form = JJA_tavg25 ~  x + y + I(x^2) + I(y^2), data=Data.Subsample)
g <- gstat(g, id = "JJA_ppt_av", form = JJA_ppt_av ~  x + y + I(x^2) + I(y^2) + I(x*y), data=Data.Subsample)
g <- gstat(g, id = "JJA_mean_w", form = JJA_mean_w ~ 1, data=Data.Subsample)
g <- gstat(g, id = "dem250"    , form = dem250 ~ 1, data=Data.Subsample)
g <- gstat(g, id = "remoteness", form = remoteness ~ 1, data=Data.Subsample)
g <- gstat(g, id = "soil250"   , form = soil250 ~ 1, data=Data.Subsample)
g <- gstat(g, id = "veg250"    , form = veg250 ~ 1, data=Data.Subsample)
g <- gstat(g, id = "slope250"  , form = slope250 ~ 1 , data=Data.Subsample)

v.cross <- variogram(g)
plot(v.cross, pl=T)

## Fitting each semivariogram. gstat needs an eyefit function...
NDVImax250object <- gstat(NULL, id = "NDVImax250", form = NDVImax250 ~ x + y + (x^2) + (y^2) + x*y, data=Data.Subsample)
NDVImax250Empirical <- variogram(NDVImax250object)
## vgm() returns the best fitting type
NDVImax250WLS <- fit.variogram(NDVImax250Empirical, vgm(c("Sph", "Exp")))
plot(NDVImax250Empirical, pl=T, model=NDVImax250WLS)


JJA_tavg25object <- gstat(NULL, id = "JJA_tavg25", form = JJA_tavg25 ~  x + y + I(x^2) + I(y^2), data=Data.Subsample)
JJA_tavg25Empirical <- variogram(JJA_tavg25object)
JJA_tavg25WLS <- fit.variogram(JJA_tavg25Empirical, vgm(c("Sph", "Exp", "Gau")))
plot(JJA_tavg25Empirical, pl=T, model=JJA_tavg25WLS)


JJA_ppt_avObject <- gstat(NULL, id = "JJA_ppt_av", form = JJA_ppt_av ~  x + y + I(x^2) + I(y^2) + I(x*y), data=Data.Subsample)
JJA_ppt_avEmpirical <- variogram(JJA_ppt_avObject)
JJA_ppt_avWLS <- fit.variogram(JJA_ppt_avEmpirical, vgm(c("Sph", "Exp", "Gau")))
plot(JJA_ppt_avEmpirical, pl=T, model=JJA_ppt_avWLS)

JJA_mean_wObject <- gstat(NULL, id = "JJA_mean_w", form = JJA_mean_w ~ 1, data=Data.Subsample)
JJA_mean_wEmpirical <- variogram(JJA_mean_wObject)
JJA_mean_wWLS <- fit.variogram(JJA_mean_wEmpirical, vgm(c("Sph", "Exp", "Gau")))
plot(JJA_mean_wEmpirical, pl=T, model=JJA_mean_wWLS)


dem250Object <- gstat(NULL, id = "dem250"    , form = dem250 ~ 1, data=Data.Subsample)
dem250Empirical <- variogram(dem250Object)
dem250WLS <- fit.variogram(dem250Empirical, vgm(c("Sph", "Exp", "Gau")))
plot(dem250Empirical, pl=T, model=dem250WLS)


remotenessObject <- gstat(NULL, id = "remoteness", form = remoteness ~ 1, data=Data.Subsample)
remotenessEmpirical <- variogram(remotenessObject)
remotenessWLS <- fit.variogram(remotenessEmpirical, vgm(.125, 'Sph', nugget = .075))
plot(remotenessEmpirical, pl=T, model=remotenessWLS)


soil250Object <- gstat(NULL, id = "soil250"   , form = soil250 ~ 1, data=Data.Subsample)
soil250Empirical <- variogram(soil250Object)
soil250WLS <- fit.variogram(soil250Empirical, vgm(c("Sph", "Exp", "Gau")))
plot(soil250Empirical, pl=T, model=soil250WLS)


veg250Object <- gstat(NULL, id = "veg250", form = veg250 ~ 1, data=Data.Subsample)
veg250Empirical <- variogram(veg250Object)
veg250WLS <- vgm(17,"Exp",.5,7) #could not get convergence, fitting by hand. 
plot(soil250Empirical, pl=T, model=veg250WLS)


slope250Object <- gstat(NULL, id = "slope250"  , form = slope250 ~ 1 , data=Data.Subsample)
slope250Empirical <- variogram(slope250Object)
slope250WLS <- fit.variogram(slope250Empirical, vgm(c("Sph", "Exp", "Gau")))
plot(slope250Empirical, pl=T, model=slope250WLS)

#################### Adding models to gstat object
## sets initial values for cross variograms equal to NDVImax250 variogram
g <- gstat(g, id = "NDVImax250", model = NDVImax250WLS, fill.all=T)
################################
## Setting the rest of our models
g <- gstat(g, id = "NDVImax250", model = NDVImax250WLS)
g <- gstat(g, id = "JJA_tavg25", model = JJA_tavg25WLS)
g <- gstat(g, id = "JJA_ppt_av", model = JJA_ppt_avWLS)
g <- gstat(g, id = "JJA_mean_w", model = JJA_mean_wWLS)
g <- gstat(g, id = "dem250"    , model = dem250WLS)
g <- gstat(g, id = "remoteness", model = remotenessWLS)
g <- gstat(g, id = "soil250"   , model = soil250WLS)
g <- gstat(g, id = "veg250"    , model = veg250WLS)
g <- gstat(g, id = "slope250"  , model = slope250WLS)
################################
v.cross <- variogram(g)
g <- fit.lmc(v.cross, g, fit.method = 7, correct.diagonal=1.01)
plot(variogram(g), model=g$model)

Prediction <- data.frame('x' = TestLatLong$long, 'y' = TestLatLong$lat)
crs = CRS("+init=epsg:4326")
coordinates(Prediction) <- ~ x + y
proj4string(Prediction) <- crs


Prediction.Subsample<- Prediction[ seq(1, length(Prediction$x), by=50),]

PredLayers <- predict(g, Prediction.Subsample)

