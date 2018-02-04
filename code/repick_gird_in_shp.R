library(sp)
library(rgdal)
library(raster)
library(maptools)
#----
library(dplyr)
library(scales)
library(spatialEco)
library(rgeos)
library(geosphere)

setwd("C:/crime predition project")
crs_string <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#----repick Taoyuan map----
County  <- readOGR(dsn="./shp",layer="County",encoding = "utf8")
TAO <- as(County, "SpatialPolygons")
TAO <- TAO[17]
cdata <- County@data[17,]
TAO2 <-  SpatialPolygonsDataFrame(TAO,cdata) #split sp frame the ID must be the same
TAO3 <-  spTransform(TAO2,CRS(crs_string))
#plot(TAO3)

#----create raster----
x = y = base = 60
xy <- matrix(rnorm(x*y),x,y)
rast <- raster(xy)
extent(rast) <- c(120.982, 121.48, 24.58646, 25.12362)#taoyuan x,y boundaries
projection(rast) <- CRS(crs_string) #projection WGS84
rast2 <- rasterToPolygons(rast,dissolve=TRUE) #to spframe
rast2@data$ID <- c(1:nrow(rast2@data))
#----intersect two sp frame----
inTAO <- intersect(rast2,TAO3)
#----
crime <- read.csv("10608_2.csv",stringsAsFactors = F,fileEncoding = "big5")
crime <- crime[!is.na(crime$lat),]
crime <- crime[crime$lat >24.58646 & crime$lat <25.12362,]
crime <- crime[crime$lon >120.982 & crime$lon <121.48,]
crime$time <- substr(crime$time,1,5)
crime <- crime[,c("time","lat","lon")]
#---
gdata <- inTAO@data
add_feature <- function(gdata,cdata,fname,polygon){
  point <- SpatialPointsDataFrame(coords = cdata[,c("lon","lat")], data = cdata,proj4string = CRS(crs_string))
  #plot(crime_sp)
  pdata <-  point.in.poly(point,polygon)
  inpoly_ID <- pdata@data$ID
  gdata[,fname] <- sapply(gdata$ID,function(x){sum(x == inpoly_ID)})
  return(gdata)
}
gdata2 <- add_feature(gdata,crime,"crime",inTAO)
#----remove empty grid----
exist <- !gdata2$crime == 0
inTAO_re <- inTAO[exist,]
gdata3 <- gdata2[exist,]
cbind(data = nrow(gdata3),sp = length(inTAO_re))
plot(inTAO_re)
#----split by time to build frame---
time_seq <- sort(unique(crime$time))

time_frame <- list()
for(i in 1:length(time_seq)){
  time_frame[[i]] <- add_feature(gdata,crime[crime$time == time_seq[i],],"crime",inTAO_re)
  time_frame[[i]]$time <- time_seq[i]
  time_frame[[i]] <- time_frame[[i]][,c("ID","time","crime")]
}

#----base line predict----
for(i in 1:length(time_seq)){
  time_frame[[i]][time_frame[[i]][,"crime"] > 1,]  <- 1
}#tran to 0-1
time_frame[[1]]$crime

model_performance <- function(matrix){
  precision_t <- matrix[2,2] / sum(matrix[,2])
  recall_t <- matrix[2,2] / sum(matrix[2,])
  accuracy <- (matrix[1,1] + matrix[2,2]) / sum(matrix)
  fmeature <- 2 * (precision_t * recall_t)/(precision_t + recall_t)
  if(is.na(fmeature)){fmeature = 0}
  performance <- cbind(precision_t,recall_t,accuracy,fmeature)
  return(performance)
}
#actual_predict
performance <- data.frame(precision_t = 0,recall_t = 0,accuracy = 0,fmeature = 0)[0,]

for(i in 1 : (length(time_seq) -1)){
  cmatrix <- table(time_frame[[i+1]]$crime,time_frame[[i]]$crime)
  performance[i,] <- model_performance(cmatrix)
}

performance_mean <- data.frame(precision_t = mean(performance$precision_t),
                               recall_t = mean(performance$recall_t),
                               accuracy = mean(performance$accuracy),
                               fmeature = mean(performance$fmeature))

write.csv(performance,paste0("./performance/baseline",base^2,".csv"),row.names = F,fileEncoding = "big5")
write.csv(performance_mean,paste0("./performance/baseline_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")
