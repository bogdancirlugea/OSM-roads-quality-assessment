# Analysis for paper
# Discrete Classification
# PKB, 8/12/2017
# Last edit: 8/22/2017


# RUN THIS SCRIPT AFTER SPATIAL MODEL
# BECAUSE IT USES THE OUTPUT FILE FROM SEM AS INPUT.


library(pastecs)
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)
library(foreign)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)
library(classInt)
library(RColorBrewer)
library(RANN)
library(Matrix)
library(spdep)
library(gridExtra)
library(rmarkdown)
library(xtable)
library(Rlab)
library(grid)
library(leaflet)



#### Input
rm(list = ls())
# setwd("//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Data/zonalstats")
# data_location <- "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Data/zonalstats"
# data <- readOGR(data_location,"grid-summary-all-roads-pop-elev-wealth-final",verbose=FALSE)
data_location <- "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Paper/analysis/paola-outputs"
data <- readOGR(data_location,  "grid-summary-all-logs", verbose = FALSE)
head(data@data)
dim(data@data)


#### Analysis
# High pop, low roads --using log form
data@data$highpop_lowroads <- ifelse((data@data$lg_ppdn > quantile(data@data$lg_ppdn, 0.75) & data@data$lg_rddn < quantile(data@data$lg_rddn, 0.25)), 1,0)
table(data@data$highpop_lowroads)
plot(data, col ="lightgrey")
plot(data[data@data$highpop_lowroads == 1,] ,col = "red", add = T)
# still figuring out leaflet
# base <- leaflet(data) %>% setView(lng = -7.523665, lat = 12.640035, zoom = 6) 
# base %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(stroke = FALSE, fillOpacity = 0.5)

# High rel-wealth, low roads-- using log form
data@data$highwi_lowroads <- ifelse((data@data$lg_w_mn > quantile(data@data$lg_w_mn, 0.75) & data@data$lg_rddn < quantile(data@data$lg_rddn, 0.25)), 1,0)
table(data@data$highwi_lowroads)
plot(data, col ="lightgrey")
plot(data[data@data$highwi_lowroads == 1,] ,col = "blue", add = T)


# some preliminary visualizations-- final viz was done in ArcMap
#par(mfrow = c(1,1))
plot(data, col ="lightgrey")
plot(data[data@data$highpop_lowroads == 1,] ,col = "red", add = T)
plot(data[data@data$highwi_lowroads == 1,] ,col = "blue", add = T)
plot(data[data@data$highwi_lowroads == 1 & data@data$highpop_lowroads == 1,] ,col = "green", add = T)
# greens are overlaps between high pop & high wealth w/ low roads
# red is high pop w/ low roads only
# blue is high wealth w/ low roads only


#### Save file
outputs_location <- "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Paper/analysis/paola-outputs"
writeOGR(obj = data, dsn = outputs_location, layer = "grid-all-tagging-log-final", driver = "ESRI Shapefile")


# end
