# Analysis for paper
# Settlements
# PKB, last edit: 8/25/2017


library(pastecs)
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
library(xtable)
library(Rlab)
library(grid)
library(plyr)
library(dplyr)
library(pastecs)


#### Input
rm(list = ls())
#setwd("//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Data/settlements/ghsl_smod")
#data <- as.data.frame(read.table("smod_point_final.txt", header = TRUE, sep = ","))
setwd("//dataserver0/ScienceData$/pblanco/Documents/PKB/papers/_20170617_groads/data/smod-final")
ghana <- readOGR(dsn = "//dataserver0/ScienceData$/pblanco/Documents/PKB/papers/_20170617_groads/data/smod-final", layer = "smod_gha", verbose = FALSE)
gha <- as.data.frame(ghana@data)
rm(ghana)
guinea <- readOGR(dsn = "//dataserver0/ScienceData$/pblanco/Documents/PKB/papers/_20170617_groads/data/smod-final", layer = "smod_guinea", verbose = FALSE)
gin <- as.data.frame(guinea@data)
rm(guinea)
liberia <- readOGR(dsn = "//dataserver0/ScienceData$/pblanco/Documents/PKB/papers/_20170617_groads/data/smod-final", layer = "smod_liberia", verbose = FALSE)
lib <- as.data.frame(liberia@data)
rm(liberia)
senegal <- readOGR(dsn = "//dataserver0/ScienceData$/pblanco/Documents/PKB/papers/_20170617_groads/data/smod-final", layer = "smod_senegal", verbose = FALSE)
sen <- as.data.frame(senegal@data)
rm(senegal)
data <- rbind(gha, gin, lib, sen)
data <- data %>% select(TARGET_FID:NEAR_DIST, id_rdm:country)
head(data)
shp <- readOGR(dsn = "//dataserver0/GlobalRoads/OSM_evaluation/Methods_Testing/Data/zonalstats", layer = "grid-summary-all-roads-pop-elev-wealth-final", verbose=FALSE)

mkTable <- function(X, sort = 1, filter = 0, cumPer = 0){ 
  Table <- data.frame( table(X) ) 
  Table$Percent <- round((prop.table( Table$Freq ) * 100), digits = 2)
  if(cumPer==1)  Table$CumPct <- cumsum( Table$Percent )
  if(sort==1)    t1 <- Table[order(-Table$Freq),] else if
  (sort==0)    t1 <- Table else stop("must specify sort = 1 or 0")
  if(filter==1)  t1 <- filter(t1, Freq>0) else if     
  (filter==0)  t1 <- t1 else stop("must specify filter = 1 or 0")
  t1
}

#### Processing
# How much of the settlement's grid is there per cellgrid?
t1 <- data %>% group_by(id_rdm, country) %>% count() %>% arrange(desc(n))
# the complete grid includes 1040 cells, and this summary only provided 965 cells-- which means, there are 75 cells missing, or with value zero.

# How many settlement points are there 5km or more away from the nearest road?
# points do not represent a count of settlements. They are points that classify human settlements on the base of
# the built-up and population density.
t2 <- data %>% filter(NEAR_DIST >= 5000) %>% group_by(id_rdm, country) %>% count() %>% arrange(desc(n))
dim(t2)[1] #494 observations --we are using 10km in the final results
t3 <- data %>% filter(NEAR_DIST >= 10000) %>% group_by(id_rdm, country) %>% count() %>% arrange(desc(n))
dim(t3)[1] #185 observations

# Merge with t1 to obtain % of settlements area
t1$setkm <- t1$n * 1
colnames(t2)[3] <- "set5km" # this is the settlement area that is far from the nearest road -- which means, unconnected-- 
colnames(t3)[3] <- "set10km"
t4 <- left_join(t1, t2, by = c("id_rdm", "country"))
t4 <- left_join(t4, t3, by = c("id_rdm", "country"))
t4[is.na(t4)] <- 0
t4$pct5km <- (t4$set5km/ t4$setkm) * 100
t4$pct10km <- (t4$set10km/ t4$setkm) * 100
par(mfrow = c(1, 2))
hist(t4$pct5km[t4$pct5km > 0], breaks = 20) 
hist(t4$pct10km[t4$pct10km > 0], breaks = 20)
# what does this mean?
# there is a jump right at 95%, hence I will use that as the cutting point to identify which cells have 

# Merge with shp, complete fishnet
shp@data <- left_join(shp@data, t4[, c(1, 2, 7, 8)], by = c("id_rdm", "country"))
shp@data[is.na(shp@data)] <- 0
head(shp@data)

# not used 
# par(mfrow = c(1, 2))
# plot(shp, col ="lightgrey")
# plot(shp[shp@data$pct5km >= 80,] ,col = "red", add = T)
# plot(shp, col ="lightgrey")
# plot(shp[shp@data$pct10km >= 80,] ,col = "blue", add = T)


#### Save file
writeOGR(obj = shp, dsn = "//dataserver0/ScienceData$/pblanco/Documents/PKB/papers/_20170617_groads/data/smod-final", 
         layer = "grid-all-settlements-update20170825", driver = "ESRI Shapefile")












