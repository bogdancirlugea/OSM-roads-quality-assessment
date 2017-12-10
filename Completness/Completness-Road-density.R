# Predicting road density via spatial model
# 8/22/2017
# BC, PKB


rm(list = ls())
library(maptools)
library(rgdal)
library(spdep)
library(ctv)
library(spgwr)
library(HH)
library(fmsb)
library(lmtest)
library(car)
library(dplyr)
setwd("//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Scripts/analysis4paper")
source("viffun.R")


# Input
# data_location <- "C:/Users/Bogdan/Desktop/Computations/New/New_data"
data_location <- "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Data/zonalstats"
x <- readOGR(data_location,'grid-summary-all-roads-pop-elev-wealth-final',verbose=FALSE)
x@data$log_popdens <- ifelse(x@data$popdens == 0, 0, log(x@data$popdens))
x@data$log_roadlength <- ifelse(x@data$roadlength == 0, 0, log(x@data$roadlength))
x@data$log_roaddens <- ifelse(x@data$roaddens == 0, 0, log(x@data$roaddens))
x@data$log_wi_mean <- ifelse(x@data$wi_mean == 0, 0, log(x@data$wi_mean))
x@data$log_elev_std <- ifelse(x@data$elev_std == 0, 0, log(x@data$elev_std))

# save file for tagging input:
outputs_location <- "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Paper/analysis/paola-outputs"
writeOGR(obj = x, dsn = outputs_location, layer = "grid-summary-all-logs", driver = "ESRI Shapefile")


#1. Descriptives
correlation_test <- x@data[, c(13, 15:17)]
cor(correlation_test, use = "everything", method = "spearman") #based on ranks
cor(correlation_test, use = "everything", method = "pearson", conf.level = 0.95) #based on values, linear relationship
corr1 <- cor.test(x@data$roaddens, x@data$popdens, method = "spearman")
corr1
corr2 <- cor.test(x@data$roaddens, x@data$wi_mean, method = "spearman")
corr2
corr3 <- cor.test(x@data$roaddens, x@data$elev_std, method = "spearman")
corr3
corr4 <- cor.test(x@data$popdens, x@data$wi_mean, method = "spearman")
corr4
corr5 <- cor.test(x@data$popdens, x@data$elev_std, method = "spearman")
corr5
corr6 <- cor.test(x@data$elev_std, x@data$wi_mean, method = "spearman")
corr6
par(mfrow = c(1, 2))
hist(x@data$popdens, breaks = 20)
hist(x@data$log_popdens, breaks = 20)
par(mfrow = c(1, 2))
hist(x@data$roaddens, breaks = 20)
hist(x@data$log_roaddens, breaks = 20)
par(mfrow = c(1, 2))
hist(x@data$wi_mean, breaks = 20)
hist(x@data$log_wi_mean, breaks = 20)
par(mfrow = c(1, 2))
hist(x@data$elev_std, breaks = 20)
hist(x@data$log_elev_std, breaks = 20)

#2. VIF tests for multicollinearity
vif_func(in_frame = x@data[, c(13,16,17)], thresh = 5)


#3. Ordinary linear regressions (OLS) & Breusch-Pagan test for heteroskedasticity
regression_lm <- stepAIC(lm(log_roaddens ~ log_popdens + log_wi_mean + log_elev_std, data = x))
summary(regression_lm)
bptest(regression_lm)


#4. Residual plots
residualPlots(regression_lm)


#5. Outliers' removal
outlierTest(regression_lm)
x$residuals <- residuals(regression_lm)
outlier_threshold <- 3*sd(residuals(regression_lm)) #8 observations excluded, as outliers, from 1040 to 1032 obs
x$outs<-ifelse(abs(x$residuals) > outlier_threshold, 1, 0)
y <-x[!x$outs,]


#6. Ordinary linear regressions (OLS), without outliers & Breusch-Pagan test for heteroskedasticity
regression_lm2 <- lm(log_roaddens ~ log_popdens + log_wi_mean, data = y) #using only these 2 variables because those were the ones with the best fit earlier.
summary(regression_lm2)
bptest(regression_lm2)


#7. Construction of spatial model, is there spatial dependence?
admin_queen1 <- poly2nb(y)
coords <- coordinates(y)
plot(x)
plot(admin_queen1, coords, add=T)
admin_queen1_wb <- nb2listw(admin_queen1, style = "B", zero.policy = TRUE) #using queen 1. Queen 2 produces larger AIC values.
moran.test(regression_lm2$residuals, admin_queen1_wb)


#8. Which model to use? Lagrange Multiplier tests for Spatial dependence
summary(lm.LMtests(regression_lm2, admin_queen1_wb, test = "all"))
# SEM robust model has a higher significance value than SAR, then I'll use SEM.
# Just to check, I'll compare with Durbin

# SEM:
regression_err <- errorsarlm(log_roaddens ~ log_popdens + log_wi_mean, data = y, admin_queen1_wb)
summary(regression_err)
bptest.sarlm(regression_err) 
moran.test(regression_err$residuals, admin_queen1_wb) 


# Durbin:
regression_durb <- lagsarlm(log_roaddens ~ log_popdens + log_wi_mean, data = y, admin_queen1_wb, type = "mixed")
summary(regression_durb)
bptest.sarlm(regression_durb)
moran.test(regression_durb$residuals, admin_queen1_wb)


# Looking at residuals#
y@data$ols_res <- residuals.lm(regression_lm2)
at <- c(min(y@data$ols_res), -3, -2, -1, 0, 1, 2, 3, max(y@data$ols_res))
spplot(obj = y, zcol = "ols_res", at = at, col.regions = rev(brewer.pal(11, "RdBu")))

y@data$sem_res <- residuals.sarlm(regression_err)
at <- c(min(y@data$sem_res), -3, -2, -1, 0, 1, 2, 3, max(y@data$sem_res))
spplot(obj = y, zcol = "sem_res", at = at, col.regions = rev(brewer.pal(11, "RdBu")))


y@data$durb_res <- residuals.sarlm(regression_durb)
at <- c(min(y@data$ols_res), -4, -3, -2, -1, 0, 1, 2, 3, max(y@data$ols_res))
spplot(obj = y, zcol = "durb_res", at = at, col.regions = rev(brewer.pal(11, "RdBu")))



# Conclusion:
# Durbin model has a slightly better fit than SEM (i.e. AIC score) but coefficients in SEM are all significant, as opposed to Durbin, in which only half of them are.
# In both models Moran's I is not significant, which means that the model has successfully accounted for spatial dependency in residuals.
# However both models reject the BP test, which means the model suffers from mispecification.
# I do not know why in the Durbin model the LM test for residual autocorrelation is still significant 
# test value: 12.493, p-value: 0.00040844; whereas in SEM the value is null. I think we should take SEM.


#9. Save file
outputs_location <- "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Paper/analysis/paola-outputs"
writeOGR(obj = y, dsn = outputs_location, layer = "grid-all-spatial-model", driver = "ESRI Shapefile")



