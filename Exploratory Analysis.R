###Exploratory analysis for table summary, GLM and odds ratio
#Load packages
require(pacman)
p_load(sf, PrevMap, tidyverse, tmap, geoR, readr, readxl, car, dplyr, Rmisc, stargazer, mfx, arsenal) ############ you do not have to install everything.

# get working directory
getwd()

###### load the data ##
data <- read_csv("CleandataMBG.csv")
names(data)

#++++++++++++++++++++++++++++++++++++++++++++
# Build summary statistics 
table_summary <- tableby(data$Mal_Outcome ~ ., data = data, cat.test="chisq")
summary (table_summary, title = "U5 Malaria Summary")

# export result 
write2word(table_summary, "~/U5MSummary.doc", title="U5 Malaria Summary")

# Build logistic model
d2 <- glm(Mal_Outcome ~ ., data = data, family = "binomial")

summary(d2)

# Check for the best-fit model
stepAIC(d2, direction = c("both", "backward", "forward"), k = 5)

# Build the best model with the least AIC
d3 <- glm(data$Mal_Outcome ~ data$WaterSource + data$Bed_net_Usg + data$Win_Protctn, family = "binomial")

summary(d3)

# Check for multicollinearity (VIF)
vif(glm(data$Mal_Outcome ~ data$WaterSource + data$Bed_net_Usg + data$Win_Protctn, data = data, family = "binomial"))

alias(glm(data$Mal_Outcome ~ data$WaterSource + data$Bed_net_Usg + data$Win_Protctn, data = data, family = "binomial"))

vif_values = vif(d3)

# Plot VIF
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue") #create horizontal bar chart to display each VIF value

abline(v = 1, lwd = 3, lty = 2)    #add vertical line at 1 to low absence of multicollinearity 
abline(v = 5, lwd = 3, lty = 2)    #add a vertical line at 5 to check high correlation or multicollinearity  

# Confidenceinterval for Estimates
confint(d3)

# Confidence interval for OR
exp(cbind("Odds ratio" = coef(d3), confint.default(d3, level = 0.95)))


# Using the --mfx-- package to derive the OR
AOR <- logitor(data$Mal_Outcome ~ data$WaterSource + data$Bed_net_Usg + data$Win_Protctn, data = data)
AOR


# Estimate the relative risk ratios which is exponentiated value of the logit coefficients
RiskRatio <- exp(coef(d3))
RiskRatio # note, the Risk ratio is the Odds ratio (AOR)


##### BIVARIATE ANALYSIS ########################
d4 <- glm(data$Mal_Outcome ~ data$WaterSource, data = data, family = "binomial")

summary(d4)

exp(cbind("Odds ratio" = coef(d4), confint.default(d4, level = 0.95)))

# Using the --mfx-- package to derive the OR
OR4 <- logitor(data$Mal_Outcome ~ data$WaterSource, data = data)
OR4


### BEDNET
d5 <- glm(data$Mal_Outcome ~ data$Bed_net_Usg, data = data, family = "binomial")

summary(d5)


exp(cbind("Odds ratio" = coef(d5), confint.default(d5, level = 0.95)))


# Confidenceinterval
confint(d5)

# Using the --mfx-- package to derive the OR
OR5 <- logitor(data$Mal_Outcome ~ data$Bed_net_Usg, data = data)
OR5


### WINDOW PRTCN
d6 <- glm(data$Mal_Outcome ~ data$Win_Protctn, data = data, family = "binomial")

summary(d6)


exp(cbind("Odds ratio" = coef(d6), confint.default(d6, level = 0.95)))

# Confidenceinterval

confint(d6)

# Using the --mfx-- package to derive the OR
OR6 <- logitor(data$Mal_Outcome ~ data$Win_Protctn, data = data)
OR6
