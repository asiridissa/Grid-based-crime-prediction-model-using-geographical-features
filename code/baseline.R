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
plot(TAO3)

#inTAO <- intersect(rast2,TAO3)
#plot(inTAO)

#----create raster----
for(base_c in 1:20){
  x = y = base = base_c * 5
  xy <- matrix(rnorm(x*y),x,y)
  rast <- raster(xy)
  extent(rast) <- c(120.982, 121.48, 24.58646, 25.12362)#taoyuan x,y boundaries
  projection(rast) <- CRS(crs_string) #projection WGS84
  rast2 <- rasterToPolygons(rast,dissolve=TRUE) #to spframe
  rast2@data$ID <- c(1:nrow(rast2@data))
  plot(rast2)
  #----intersect two sp frame----
  inTAO <- intersect(rast2,TAO3)
  plot(inTAO)
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
    time_frame[[i]] <- add_feature(gdata3,crime[crime$time == time_seq[i],],"crime",inTAO_re)
    time_frame[[i]]$time <- time_seq[i]
    time_frame[[i]] <- time_frame[[i]][,c("ID","time","crime")]
  }
  
  #----base line predict----
  for(i in 1:length(time_seq)){
    time_frame[[i]][time_frame[[i]][,"crime"] > 1,]  <- 1
  }#tran to 0-1
  time_frame[[1]]$crime
  
  model_performance <- function(matrix){
    if(ncol(matrix) == 2){
      PPV <- matrix[2,2] / sum(matrix[,2])
      TPR <- matrix[2,2] / sum(matrix[2,])
      NPV <- matrix[1,1] / sum(matrix[,1])
      TNR <- matrix[1,1] / sum(matrix[1,])
      accuracy <- (matrix[1,1] + matrix[2,2]) / sum(matrix)
      F1 <- 2 * (PPV * TPR)/(PPV + TPR)
      if(is.na(F1)){F1 = 0}
      performance <- cbind(PPV,TPR,NPV,TNR,accuracy,F1)
      return(performance)
    }else{
      return(data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0))
    }
  }
  performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]

  for(i in 24 : (length(time_seq) -1)){
    cmatrix <- table(time_frame[[i+1]]$crime,time_frame[[i]]$crime)
    performance[i-23,] <- model_performance(cmatrix)
  }

  performance_mean <- data.frame(PPV = mean(performance$PPV),
                                 TPR = mean(performance$TPR),
                                 NPV = mean(performance$NPV),
                                 TNR = mean(performance$TNR),
                                 accuracy = mean(performance$accuracy),
                                 F1 = mean(performance$F1))

  write.csv(performance,paste0("./performance/baseline",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(performance_mean,paste0("./performance/baseline_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")

  #----moving average----
  #actual_predict
  performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  for(i in 24 : (length(time_seq) -1)){
    cmatrix <- table(time_frame[[i+1]]$crime,as.integer(time_frame[[i]]$crime + time_frame[[i-1]]$crime + time_frame[[i-2]]$crime > 1))
    performance[i-23,] <- model_performance(cmatrix)
  }

  performance_mean <- data.frame(PPV = mean(performance$PPV),
                                 TPR = mean(performance$TPR),
                                 NPV = mean(performance$NPV),
                                 TNR = mean(performance$TNR),
                                 accuracy = mean(performance$accuracy),
                                 F1 = mean(performance$F1))

  write.csv(performance,paste0("./performance/3m",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(performance_mean,paste0("./performance/3m_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")

  performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  for(i in 24 : (length(time_seq) -1)){
    cmatrix <- table(time_frame[[i+1]]$crime,as.integer(time_frame[[i]]$crime + time_frame[[i-1]]$crime + time_frame[[i-2]]$crime + time_frame[[i-3]]$crime + time_frame[[i-4]]$crime > 2))
    performance[i-23,] <- model_performance(cmatrix)
  }

  performance_mean <- data.frame(PPV = mean(performance$PPV),
                                 TPR = mean(performance$TPR),
                                 NPV = mean(performance$NPV),
                                 TNR = mean(performance$TNR),
                                 accuracy = mean(performance$accuracy),
                                 F1 = mean(performance$F1))

  write.csv(performance,paste0("./performance/5m",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(performance_mean,paste0("./performance/5m_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")

   performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  for(i in 24 : (length(time_seq) -1)){
    cmatrix <- table(time_frame[[i+1]]$crime,as.integer(time_frame[[i]]$crime + time_frame[[i-1]]$crime + time_frame[[i-2]]$crime + time_frame[[i-3]]$crime + time_frame[[i-4]]$crime + time_frame[[i-5]]$crime + time_frame[[i-6]]$crime > 3))
    performance[i-23,] <- model_performance(cmatrix)
  }

  performance_mean <- data.frame(PPV = mean(performance$PPV),
                                 TPR = mean(performance$TPR),
                                 NPV = mean(performance$NPV),
                                 TNR = mean(performance$TNR),
                                 accuracy = mean(performance$accuracy),
                                 F1 = mean(performance$F1))

  write.csv(performance,paste0("./performance/7m",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(performance_mean,paste0("./performance/7m_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")

  performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  for(i in 24 : (length(time_seq) -1)){
    cmatrix <- table(time_frame[[i+1]]$crime,as.integer(time_frame[[i]]$crime + time_frame[[i-1]]$crime + time_frame[[i-2]]$crime + time_frame[[i-3]]$crime + time_frame[[i-4]]$crime + time_frame[[i-5]]$crime + time_frame[[i-6]]$crime + time_frame[[i-7]]$crime + time_frame[[i-8]]$crime> 4))
    performance[i-23,] <- model_performance(cmatrix)
  }

  performance_mean <- data.frame(PPV = mean(performance$PPV),
                                 TPR = mean(performance$TPR),
                                 NPV = mean(performance$NPV),
                                 TNR = mean(performance$TNR),
                                 accuracy = mean(performance$accuracy),
                                 F1 = mean(performance$F1))

  write.csv(performance,paste0("./performance/9m",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(performance_mean,paste0("./performance/9m_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")

  
  performance <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  base11_prediction_list <- list()
  
  for(i in 24 : (length(time_seq) -1)){
    base11_prediction <- as.integer(time_frame[[i]]$crime + time_frame[[i-1]]$crime + time_frame[[i-2]]$crime + time_frame[[i-3]]$crime + time_frame[[i-4]]$crime + time_frame[[i-5]]$crime + time_frame[[i-6]]$crime + time_frame[[i-7]]$crime + time_frame[[i-8]]$crime + time_frame[[i-9]]$crime + time_frame[[i-10]]$crime> 5)
    base11_prediction_list[[i]] <- base11_prediction
    cmatrix <- table(time_frame[[i+1]]$crime,base11_prediction)
    performance[i-23,] <- model_performance(cmatrix)
  }
  #base11_prediction_list[[24]]
  performance_mean <- data.frame(PPV = mean(performance$PPV),
                                 TPR = mean(performance$TPR),
                                 NPV = mean(performance$NPV),
                                 TNR = mean(performance$TNR),
                                 accuracy = mean(performance$accuracy),
                                 F1 = mean(performance$F1))

  write.csv(performance,paste0("./performance/11m",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(performance_mean,paste0("./performance/11m_mean",base^2,".csv"),row.names = F,fileEncoding = "big5")
}


#----
base11_prediction_list2 <- list()
for(i in 1 :8){
  base11_prediction_list2[[i]] <- base11_prediction_list[[i+23]]
}

dif_actual <- list()
dif_dp <- list()
dif_11 <- list()

matrix_dp <- list()
matrix_11 <-list()

per_dp <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
per_11 <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]

for(i in 1 :7){
  dif_actual[[i]] <-  as.integer(test_list[[i]]$hotspot != test_list[[i+1]]$hotspot)
  dif_dp[[i]] <- as.integer(dp_prediction_list[[i]]$predict != dp_prediction_list[[i+1]]$predict)     
  dif_11[[i]] <-  as.integer(base11_prediction_list2[[i]] != base11_prediction_list2[[i+1]])
  matrix_dp[[i]] <- table(dif_actual[[i]],dif_dp[[i]])
  matrix_11[[i]] <- table(dif_actual[[i]],dif_11[[i]])
  per_dp[i,] <- model_performance(matrix_dp[[i]])
  per_11[i,] <- model_performance(matrix_11[[i]])
}

write.csv(per_dp,"dfp_dp.csv",row.names = F,fileEncoding = "big5")
write.csv(per_11,"dfp_11.csv",row.names = F,fileEncoding = "big5")



matrix_dp[[6]]
matrix_11[[6]]


model_performance(matrix_dp[[5]])
model_performance(matrix_11[[5]])



