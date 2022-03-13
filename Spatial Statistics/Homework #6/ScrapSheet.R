# Generate Second Order Model
secondOrder <- lm(WolfData ~ clat + clon + I(clat^2) + I(clon^2) + I(clon*clat))

# New geodata object with residuals of 2nd order model
WolfcampR <- as.geodata(cbind(wolfcampCentered$coords[,1], 
                                wolfcampCentered$coords[,2], 
                                secondOrder$residuals))

# Consider empirical semivariogram with original spatial order
orig_vario <- variog(WolfcampRes, trend="cte", estimator.type="modulus" )
lines(orig_vario, col = 'red', lwd=3 )

# Conducting randomization test
n <- length(wolfcamp$data)
for( i in 1:100) {
  # Sample a permutation 
  new.order <- sample(1:n, size=n, replace=FALSE )
  # Generate Reordered data
  reordered <- WolfcampRes$data[new.order]
  reordered.geodata <- as.geodata(
    cbind(WolfcampRes$coords,reordered))
  # Generate reordered semivariogram
  reordered_vario <- variog(reordered.geodata,
                       trend="cte", estimator.type="modulus" )
  # Plot reordered semivariograms
  lines(reordered_vario, col="gray")
}


directional_vario <- variog4(WolfcampOutlier, trend="1st",
                             estimator.type="modulus")
plot(directional_vario)






######## Fitting the robust empirical second order trend semivariogram
robust_vario <- variog(wolfcamp, trend="2nd", estimator.type="modulus")


### Fitting the WLS estimator for each type of semivariogram
WLS_exponential <- variofit(robust_vario, cov.model = 'exponential', 
                            max.dist = 408, 
                            ini.cov.pars = c(2311,45.34), 
                            nugget = 924.73)


WLS_gaussian <- variofit(robust_vario, cov.model = 'gaussian', 
                            max.dist = 408, 
                            ini.cov.pars = c(2311,45.34), 
                            nugget = 924.73)


WLS_spherical <- variofit(robust_vario, cov.model = 'spherical', 
                         max.dist = 408, 
                         ini.cov.pars = c(2311,45.34), 
                         nugget = 924.73)


## Generating plot of WLS estimators
plot(robust_vario)
lines(WLS_spherical, col = 'blue')
lines(WLS_gaussian)
lines(WLS_exponential, col = 'red')
legend(x = 0, y = 8000, legend = c('Spherical', 'Gaussian', 'Exponential'), 
       col = c('blue', 'black','red'), 
       lty=1, cex=0.8)



## Storing initial values for ML and REMLm
init_pars_exp <- WLS_exponential$cov.pars
init_pars_gauss <- WLS_gaussian$cov.pars
init_pars_sphere <- WLS_spherical$cov.pars

init_tau_exp <- WLS_exponential$nugget
init_tau_gauss <- WLS_gaussian$nugget
init_tau_sphere <- WLS_spherical$nugget





### Fitting the ML estimator for each type of semivariogram
ML_exponential <- likfit(wolfcamp, cov.model = 'exponential',
                         trend = '2nd',                          
                         ini.cov.pars = init_pars_exp, 
                         nugget = init_tau_exp, 
                         lik.method = 'ML')



ML_gaussian <- likfit(wolfcamp, cov.model = 'gaussian',
                         trend = '2nd', 
                         ini.cov.pars = init_pars_gauss, 
                         nugget = init_tau_gauss, 
                         lik.method = 'ML')



ML_spherical <- likfit(wolfcamp, cov.model = 'spherical',
                         trend = '2nd', 
                         ini.cov.pars = init_pars_sphere, 
                         nugget = init_tau_sphere, 
                         lik.method = 'ML')


## Generating plot of ML estimators.
plot(robust_vario)
lines(ML_spherical, col = 'blue')
lines(ML_gaussian)
lines(ML_exponential, col = 'red')
legend(x = 0, y = 8000, legend = c('Spherical', 'Gaussian', 'Exponential'), 
       col = c('blue', 'black','red'), 
       lty=1, cex=0.8)





## Fitting the REML estimator for each family of semivariograms.
REML_exponential <- likfit(wolfcamp, cov.model = 'exponential',
                         trend = '2nd',                          
                         ini.cov.pars = init_pars_exp, 
                         nugget = init_tau_exp, 
                         lik.method = 'REML')



REML_gaussian <- likfit(wolfcamp, cov.model = 'gaussian',
                      trend = '2nd', 
                      ini.cov.pars = init_pars_gauss, 
                      nugget = init_tau_gauss, 
                      lik.method = 'REML')



REML_spherical <- likfit(wolfcamp, cov.model = 'spherical',
                       trend = '2nd', 
                       ini.cov.pars = init_pars_sphere, 
                       nugget = init_tau_sphere, 
                       lik.method = 'REML')


## Generating the plot of each REML estimator. 
plot(robust_vario)
lines(REML_spherical, col = 'blue')
lines(REML_gaussian)
lines(REML_exponential, col = 'red')
legend(x = 0, y = 8000, legend = c('Spherical', 'Gaussian', 'Exponential'), 
       col = c('blue', 'black','red'), 
       lty=1, cex=0.8)


## AIC Table
AIC(ML_exponential)
AIC(ML_gaussian)
AIC(ML_spherical)

plot(robust_vario)
lines(ML_exponential, col = 'blue')
lines(WLS_exponential)
legend(x = 0, y = 8000, legend = c('ML', 'WLS'), 
       col = c('blue', 'black'), 
       lty=1, cex=0.8)






robust_vario <- variog(wolfcamp, trend="2nd", estimator.type="modulus")

wolfcamp$borders <- get.borders(
    cbind(wolfcamp$coords[,1],wolfcamp$coords[,2]), 
    concavity_param = 3, frac = 15)

plot(wolfcamp)


### Fitting the WLS estimator for Starting points. 
WLS_exponential <- variofit(robust_vario, cov.model = 'exponential', 
                            max.dist = 408, 
                            ini.cov.pars = c(2311,45.34), 
                            nugget = 924.73)


## Storing initial values for ML
init_pars_exp <- WLS_exponential$cov.pars
init_tau_exp <- WLS_exponential$nugget


ML_exponential <- likfit(wolfcamp, cov.model = 'exponential',
                         trend = '2nd',                          
                         ini.cov.pars = init_pars_exp, 
                         nugget = init_tau_exp, 
                         lik.method = 'ML')


## Generating the kriging object. 
my_kr_obj <- krige.control(
    type.krige = 'OK',
    trend.d = '2nd',
    trend.l = '2nd',
    cov.model = 'exponential',
    cov.pars = ML_exponential$cov.pars,
    nugget = ML_exponential$nugget, 
)

## Pulling the bounding lon, lat pairs. 
lonmin <- min(wolfcamp$borders[,1])
lonmax <- max(wolfcamp$borders[,1])
latmin <- min(wolfcamp$borders[,2])
latmax <- max(wolfcamp$borders[,2])

## Resolution was computed by dividing the range of both 
## the lat and lon by 100 and picking the smaller one
## for me this generates a ~200*400 pred grid
res = 2

## Generating the prediction grid
my_grid <- pred_grid(
  c(lonmin,lonmax),
  c(latmin,latmax),
  by = res )


## Generating prediction surface
my_kr_results <- krige.conv(wolfcamp, 
                            krige = my_kr_obj,
                            locations = my_grid)


## Plotting the prediction surface
my_grays <- gray( seq(25/63, 59/63,length = 40 ))


one_plot(my_kr_results$predict,
         lonmin, lonmax, 
         latmin, latmax, res,
         'longitude', 'latitude', 
         wolfcamp$border, my_grays, 
         wolfcamp$coords, 
         'Universal Kriging 2nd Order ML Exponential Estimator')



