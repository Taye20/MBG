require(pacman)
p_load(sf, PrevMap, tidyverse, tmap, geoR, readr, readxl, car, dplyr, Rmisc, stargazer, mfx) ############ you do not have to install everything.

# get working directory
getwd()

###### load the data ##
data <- read_csv("cleandata4OddsRatio.csv")

names(data)


d2 <- glm(octi ~ ., data = data, family = "binomial")

summary(d2)

d3 <- glm(data$octi ~ data$WaterSource + data$Bed_net_Usg + data$Win_Protctn, family = "binomial")

summary(d3)

# Confidenceinterval for Estimates
confint(d3)


# Confidence interval for OR
exp(cbind("Odds ratio" = coef(d3), confint.default(d3, level = 0.95)))


# Using the --mfx-- package to derive the OR
AOR <- logitor(data$octi ~ data$WaterSource + data$Bed_net_Usg + data$Win_Protctn, data = data)
AOR


# Estimate the relative risk ratios which is exponentiated value of the logit coefficients
RiskRatio <- exp(coef(d3))
RiskRatio # note, the Risk ratio is the Odds ratio (AOR)


##### BIVARIATE ANALYSIS ########################
d4 <- glm(data$octi ~ data$WaterSource, data = data, family = "binomial")

summary(d4)

exp(cbind("Odds ratio" = coef(d4), confint.default(d4, level = 0.95)))



# Using the --mfx-- package to derive the OR
OR4 <- logitor(data$octi ~ data$WaterSource, data = data)
OR4




### BEDNET
d5 <- glm(data$octi ~ data$Bed_net_Usg, data = data, family = "binomial")

summary(d5)


exp(cbind("Odds ratio" = coef(d5), confint.default(d5, level = 0.95)))


# Confidenceinterval
confint(d5)

# Using the --mfx-- package to derive the OR
OR5 <- logitor(data$octi ~ data$Bed_net_Usg, data = data)
OR5


### WINDOW PRTCN
d6 <- glm(data$octi ~ data$Win_Protctn, data = data, family = "binomial")

summary(d6)


exp(cbind("Odds ratio" = coef(d6), confint.default(d6, level = 0.95)))

# Confidenceinterval

confint(d6)

# Using the --mfx-- package to derive the OR
OR6 <- logitor(data$octi ~ data$Win_Protctn, data = data)
OR6
