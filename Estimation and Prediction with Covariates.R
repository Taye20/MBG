require(pacman)
p_load(sf, PrevMap, tidyverse, tmap, geoR, readr, readxl, car, dplyr)   ############ you do not have to install everything.


# get working directory
getwd()

###### load the data ##
data <- read_csv("cleandataMBG.csv")


########## convert to meters 
coords <- data %>% st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  st_transform(., crs = 26331) %>% st_coordinates()


d2 <- glm(octi ~ ., data = data, family = "binomial")

# recode covariates
data <- d %>% mutate(Bed_net_Usg = if_else(Bed_net_Usg == "Yes", 1, 0),
                     Win_Protctn = if_else(Win_Protctn == "Yes", 1, 0),)



###### create the dataset to analyse
data2 <- data.frame(coords, Z = data$Mal_Outcome, A = data$Win_Protctn, B = data$Bed_net_Usg, 
                    C = data$WaterSource)


# final model
fit <- glm(formula = Z ~ A + B + C, data = data2, family = "binomial")
summary(fit)


emp.var <- variog(coords=data2[,1:2], data =residuals(fit))
plot(emp.var)

#### create a geodata data.frame 
gdata <- as.geodata(data.frame(coords, Z = residuals(fit)))
####### envelope 
emp.var.env <- variog.mc.env(geodata = gdata, obj.variog = emp.var)
plot(emp.var, envelope = emp.var.env)

######
thedist <- dist(coords)
hist(thedist)
plot(ecdf(thedist))


#######   Modelling - parameter estimation ###################
c.mcmc <- control.mcmc.MCML(n.sim = 10000, burnin = 2000,
                            thin = 8, h = (1.65)/(nrow(data2) ^ (1/6)))
par0 <- c(fit$coefficients, 1, 1000)
par0
data2$m <- 1 

set.seed(234)
fit.MCML2 <- binomial.logistic.MCML(formula = Z ~ A + B + C,  units.m = ~m, par0 = par0,
                                    coords = ~ X + Y, data = data2,
                                    control.mcmc = c.mcmc, fixed.rel.nugget = TRUE,
                                    kappa = 0.5, start.cov.pars = c(par0[6]))

summary(fit.MCML2, log.cov.par=T)


#saveRDS(object = fit.MCML1, file = "OCTwithCOVAR.rds")
#readRDS()

##### load boundary ####################################
Akure <- shapefile("C:/Users/Taye Bayode/Desktop/TJ/Extent/Boundary.shp")
plot(Akure, col = "lightgrey")

# change coordinates
BB <- Akure %>% st_as_sf(., coords = c("long", "lat"), crs = 4326) %>%
  st_transform(., crs = 26331) %>% st_coordinates()

##### read predictors ##############################
#######################################
#OCTwithCOVAR <- readRDS("C:/Users/Taye Bayode/Documents/PaperProject/SpatialAnalysisU5Malaria/OCTwithCOVAR.rds")

#summary(OCTwithCOVAR, log.cov.par=F)


A <- readRDS("C:/Users/Taye Bayode/Desktop/TJ/predWINPrtcn2023_100.rds")

B <- readRDS("C:/Users/Taye Bayode/Desktop/TJ/predITNusage2023_100.rds")

C <- readRDS("C:/Users/Taye Bayode/Desktop/TJ/predWATERsource2023_100.rds")



library(raster)
grid.pred <- gridpts(poly, xs = 100, ys = 100)  #### create grid 

#first
shpp <- rasterFromXYZ(data.frame(A$grid, predictions = A$prevalence$predictions),crs = "+init=epsg:26331 +units=m")

WindowProtection <- raster::extract(shpp, grid.pred)
WindowProtection

anyNA(WindowProtection)

# second
shpp2 <- rasterFromXYZ(data.frame(B$grid, predictions = B$prevalence$predictions),crs = "+init=epsg:26331 +units=m")

ITNusage <- raster::extract(shpp2, grid.pred)
anyNA(ITNusage)

# third
shpp3 <- rasterFromXYZ(data.frame(C$grid, predictions = C$prevalence$predictions),crs = "+init=epsg:26331 +units=m")

WATERsource <- raster::extract(shpp3, grid.pred)
anyNA(WATERsource)



PREDICTOR <- data.frame(A = WindowProtection, B = ITNusage, C = WATERsource)


#a <- raster("putfilepath")
#shpp <- raster("putfilepath")

################# prediction ##########
pred.MCML <- spatial.pred.binomial.MCML(object = fit.MCML2, grid.pred = grid.pred,
                                        control.mcmc = c.mcmc, type = "joint", predictors = PREDICTOR,
                                        scale.predictions = "prevalence",
                                        standard.errors = TRUE, thresholds = 0.1,
                                        scale.thresholds = "prevalence")


par(mfrow = c(1,3))
plot(pred.MCML, type = "prevalence",
     summary = "predictions", 
     main = "Malaria Prevalence - predictions enhanced with Covariates \n (classical analysis)")

contour(pred.MCML, type = "prevalence", summary = "predictions",
        levels = c(0.2, 0.25, 0.3), add = TRUE)

plot(pred.MCML, type = "prevalence",
     summary = "standard.errors", zlim = c(0,0.3),main = "Malaria Prevalence - standard errors \n (classical analysis)")

plot(pred.MCML, summary = "exceedance.prob",
     zlim = c(0,1),
     main = "Malaria Prevalence  - exceedance probabilities enhanced with Covariates \n (classical analysis)")
