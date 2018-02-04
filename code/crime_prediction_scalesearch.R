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
#----
library(e1071)
library(unbalanced)
library(class)


setwd("C:/crime predition project")
crs_string <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#----learning enviroment----
library(h2o)
Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_144')
localH2O <- h2o.init(ip = 'localhost', port = 54321, max_mem_size = '6g')
#----repick Taoyuan map----
County  <- readOGR(dsn="./shp",layer="County",encoding = "utf8")
TAO <- as(County, "SpatialPolygons")
TAO <- TAO[17]
cdata <- County@data[17,]
TAO2 <-  SpatialPolygonsDataFrame(TAO,cdata) #split sp frame the ID must be the same
TAO3 <-  spTransform(TAO2,CRS(crs_string))
#plot(TAO3)

#----create raster----
for(base_c in 19:20){
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
  #----add place----
  place <- list.files("./place")
  
  for(i in 1 : length(place)){
    p <- read.csv(paste0("./place/",place[i]),stringsAsFactors = F,fileEncoding = "big5")
    names(p)[c(2,3)] <- c("lat","lon")
    gdata2 <- add_feature(gdata2,p,p$type[1],inTAO)
  }
  #----remove empty grid----
  exist <- !gdata2$crime == 0
  inTAO_re <- inTAO[exist,]
  gdata3 <- gdata2[exist,]
  cbind(data = nrow(gdata3),sp = length(inTAO_re))
  exist_index <- as.integer(row.names(gdata3))
  plot(inTAO_re)
  
  #----central----
  leng <- length(inTAO@polygons)
  central <- data.frame(long=0,lat=0)[0,]
  for(i in 1:leng){
    central[i,] <- inTAO@polygons[[i]]@labpt
  }
  central <- central[,c("lat","long")]
  dist_matrix <- as.matrix(dist(central),nrow = leng,byrow = T)
  #----test for boundries----
  for(i in 1:(nrow(dist_matrix)-1)){
    d1 <- sort(dist_matrix[,i+1])[1:9]
    d2 <- sort(dist_matrix[,i])[1:9]
    if(sum(d1 == d2) == 9){
      bound <- d1[9]
      #0.1
      #0.07324922
      #0.04883282
      break
    }
  }
  
  #----near list----
  near_list <- list()
  for(i in 1 : nrow(dist_matrix)){
    near_list[[i]] <- as.integer(row.names(dist_matrix[dist_matrix[,i] <= bound &dist_matrix[,i] > 0,]))
  }
  near_list2 <- list()
  for(i in 1 : length(gdata3$ID)){
    near_list2[[i]] <- near_list[[exist_index[i]]]
  }
  
  #----split by time to build frame---
  time_seq <- sort(unique(crime$time))
  summary(factor(crime$time))
  time_frame <- list()
  for(i in 1:length(time_seq)){
    time_frame[[i]] <- add_feature(gdata3,crime[crime$time == time_seq[i],],"crime",inTAO_re)
    time_frame[[i]]$time <- time_seq[i]
    time_frame[[i]] <- time_frame[[i]][,c(-1,-3,-4:-6)]
  }
  
  t <- time_frame[[13]]
  
  #----
  for(i in 1:length(time_seq)){
    time_frame[[i]][time_frame[[i]][,"crime"] >= 1,"hotspot"]  <- 1
    time_frame[[i]][time_frame[[i]][,"crime"] == 0,"hotspot"]  <- 0
  }#tranfer to 0-1
  
  #----time stack----
  for(i in  13 : length(time_seq)){
    time_frame[[i]][,"last_1"]  <- time_frame[[i-1]][,"hotspot"]
    time_frame[[i]][,"last_3"]  <- time_frame[[i-1]][,"hotspot"] + time_frame[[i-2]][,"hotspot"] + time_frame[[i-3]][,"hotspot"]
    time_frame[[i]][,"last_6"]  <- time_frame[[i-1]][,"hotspot"] + time_frame[[i-2]][,"hotspot"] + time_frame[[i-3]][,"hotspot"] + time_frame[[i-4]][,"hotspot"] + time_frame[[i-5]][,"hotspot"] + time_frame[[i-6]][,"hotspot"]
    time_frame[[i]][,"last_9"]  <- time_frame[[i-1]][,"hotspot"] + time_frame[[i-2]][,"hotspot"] + time_frame[[i-3]][,"hotspot"] + time_frame[[i-4]][,"hotspot"] + time_frame[[i-5]][,"hotspot"] + time_frame[[i-6]][,"hotspot"] + time_frame[[i-7]][,"hotspot"] + time_frame[[i-8]][,"hotspot"] + time_frame[[i-9]][,"hotspot"]
    time_frame[[i]][,"last_12"]  <- time_frame[[i-1]][,"hotspot"] + time_frame[[i-2]][,"hotspot"] + time_frame[[i-3]][,"hotspot"] + time_frame[[i-4]][,"hotspot"] + time_frame[[i-5]][,"hotspot"] + time_frame[[i-6]][,"hotspot"] + time_frame[[i-7]][,"hotspot"] + time_frame[[i-8]][,"hotspot"] + time_frame[[i-9]][,"hotspot"] + time_frame[[i-10]][,"hotspot"] + time_frame[[i-11]][,"hotspot"] + time_frame[[i-12]][,"hotspot"]
    #   time_frame[[i]][time_frame[[i]][,"last_3"] > 1 ,"last_3"] <- 1
    #   time_frame[[i]][time_frame[[i]][,"last_6"] > 1 ,"last_6"] <- 1
    #   time_frame[[i]][time_frame[[i]][,"last_9"] > 1 ,"last_9"] <- 1
    #   time_frame[[i]][time_frame[[i]][,"last_12"] > 1 ,"last_12"] <- 1
    time_frame[[i]][,"before_12"]  <- time_frame[[i-12]][,"hotspot"]
  }
  
  #----space stack----
  #----add near result----
  for(i in  13 : length(time_seq)){
    for(j in 1 : length(near_list2)){
      time_frame[[i]][j,"near_1"]  <- sum(time_frame[[i]][near_list2[[j]],"last_1"])
      time_frame[[i]][j,"near_3"]  <- sum(time_frame[[i]][near_list2[[j]],"last_3"])
      time_frame[[i]][j,"near_6"]  <- sum(time_frame[[i]][near_list2[[j]],"last_6"])
      time_frame[[i]][j,"near_9"]  <- sum(time_frame[[i]][near_list2[[j]],"last_9"])
      time_frame[[i]][j,"near_12"]  <- sum(time_frame[[i]][near_list2[[j]],"last_12"])
      time_frame[[i]][j,"near_b12"]  <- sum(time_frame[[i]][near_list2[[j]],"before_12"])
    }
    time_frame[[i]][is.na(time_frame[[i]][,"near_1"]),"near_1"] <- 0
    time_frame[[i]][is.na(time_frame[[i]][,"near_3"]),"near_3"] <- 0
    time_frame[[i]][is.na(time_frame[[i]][,"near_6"]),"near_6"] <- 0
    time_frame[[i]][is.na(time_frame[[i]][,"near_9"]),"near_9"] <- 0
    time_frame[[i]][is.na(time_frame[[i]][,"near_12"]),"near_12"] <- 0
    time_frame[[i]][is.na(time_frame[[i]][,"near_b12"]),"near_b12"] <- 0
  }
  
  #----split to train & test frame----
  train_list <- list()
  test_list <- list()
  for(i in 1 : 8){
    tmp <- time_frame[[i+24]]
    tmp <- tmp[,c(-1,-2,-87)]
    tmp <- tmp[,c(85:97,1:84)]
    test_list[[i]] <- tmp
  }
  
  for(i in 1 : 8){
    train_frame <- test_list[[1]][0,]
    for(j in 13 : (i + 23)){
      tmp <- time_frame[[j]]
      tmp <- tmp[,c(-1,-2,-87)]
      tmp <- tmp[,c(85:97,1:84)]
      train_frame <- rbind(train_frame,tmp)
    }
    train_list[[i]] <- train_frame
  }
  
  #----regularization----
  maxmin <- function(x){(x-min(x))/(max(x)-min(x))}
  
  for(i in 1 : 8){
    for(j in 3:ncol(train_list[[1]])){
      train_list[[i]][,j] <- maxmin(train_list[[i]][,j])
      test_list[[i]][,j] <- maxmin(test_list[[i]][,j])
    }
  }
  
  #----factor all----
  for(i in 1 : 8){
    train_list[[i]]$hotspot <- factor(train_list[[i]]$hotspot)
    test_list[[i]]$hotspot <- factor(test_list[[i]]$hotspot)
  }
  
  #----SMOTE----
  # summary(factor(train_list[[1]]$hotspot))
  # 
  # SMOTE <- function(dataset){
  #   output <- dataset[,1]
  #   input <- dataset[ ,-1]
  #   Sdata <- ubSMOTE(X=input, Y= output,perc.over = 100,perc.under = 300)
  #   Sdata <- cbind(Sdata$Y,Sdata$X)
  #   names(Sdata)[1] <- "hotspot"
  #   return(Sdata)
  # }
  # 
  # train_list_nSMOTE <- train_list
  # 
  # for(i in 1 : 8){
  #   train_list[[i]] <- SMOTE(train_list[[i]])
  # }
  # 
  # summary(factor(train_list[[1]]$hotspot))
  # summary(factor(train_list_nSMOTE[[1]]$hotspot))
  
  #----start learning----
  dependent <- colnames(train_list[[1]])[1]
  independent <- colnames(train_list[[1]])[c(2:97)]
  independent_noloc <- colnames(train_list[[1]])[c(2:13)]
  independent_onlyloc <- colnames(train_list[[1]])[c(14:97)]
  dependent
  independent
  #----
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
  #----
  # model_dp_list <- list()
  # matrix_dp_list <- list()
  # dp_prediction_list <- list()
  
  model_dp_list2 <- list()
  matrix_dp_list2 <- list()
  dp_prediction_list2 <- list()
  
  model_dp_list3 <- list()
  matrix_dp_list3 <- list()
  dp_prediction_list3 <- list()
  #model_rf_list <- list()
  #matrix_rf_list <- list()
  #rf_prediction_list <- list()

  
  #rf_per <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  #dp_per <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  dp_per2 <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  dp_per3 <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  
  # svm_per <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  # knn1_per <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  # knn3_per <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
  # knn5_per <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]

  for(i in 1 : 8){
    train_frame <- train_list[[i]]
    test_frame <- test_list[[i]]
    
    dtrain <- as.h2o(train_frame)
    dtest <- as.h2o(test_frame)
    
    # model_rf <- h2o.randomForest("randomForest",
    #                              x = independent,
    #                              y = dependent,
    #                              training_frame = dtrain)
    
    # model_dp <- h2o.deeplearning(x = independent,
    #                              y = dependent,
    #                              training_frame = dtrain,
    #                              hidden = c(100,100,100,100,100,100,100,100,100),
    #                              activation = "RectifierWithDropout",
    #                              adaptive_rate = F,
    #                              input_dropout_ratio = 0.2,
    #                              hidden_dropout_ratios = rep(0.2,9),
    #                              rate = 0.001,
    #                              epochs = 45)
    
    model_dp2 <- h2o.deeplearning(x = independent_noloc,
                                 y = dependent,
                                 training_frame = dtrain,
                                 hidden = c(100,100,100,100,100,100,100,100,100),
                                 activation = "RectifierWithDropout",
                                 adaptive_rate = F,
                                 input_dropout_ratio = 0.2,
                                 hidden_dropout_ratios = rep(0.2,9),
                                 rate = 0.001,
                                 epochs = 45)
    
    model_dp3 <- h2o.deeplearning(x = independent_onlyloc,
                                 y = dependent,
                                 training_frame = dtrain,
                                 hidden = c(100,100,100,100,100,100,100,100,100),
                                 activation = "RectifierWithDropout",
                                 adaptive_rate = F,
                                 input_dropout_ratio = 0.2,
                                 hidden_dropout_ratios = rep(0.2,9),
                                 rate = 0.001,
                                 epochs = 45)
    # model_svm <- svm(hotspot ~.,data = train_frame)
    
    #----
    #train_time <- cbind(rf = model_rf@model$run_time,dp = model_dp@model$run_time)
    #rf_varimp <- as.data.frame(h2o.varimp(model_rf))
    #rf_prediction <- as.data.frame(h2o.predict(model_rf, newdata = dtest))
    #dp_prediction <- as.data.frame(h2o.predict(model_dp, newdata = dtest))
    dp_prediction2 <- as.data.frame(h2o.predict(model_dp2, newdata = dtest))
    dp_prediction3 <- as.data.frame(h2o.predict(model_dp3, newdata = dtest))
    
    # svm_prediction <- data.frame(predict =predict(model_svm,test_frame))
    # knn1_prediction <- knn(train_frame[,-1], test_frame[,-1], train_frame[,1], k = 1)
    # knn3_prediction <- knn(train_frame[,-1], test_frame[,-1], train_frame[,1], k = 3)
    # knn5_prediction <- knn(train_frame[,-1], test_frame[,-1], train_frame[,1], k = 5)
    
    #----
    #rf_matrix <- table(test_frame$hotspot,rf_prediction$predict)
    #dp_matrix <- table(test_frame$hotspot,dp_prediction$predict)
    dp_matrix2 <- table(test_frame$hotspot,dp_prediction2$predict)
    dp_matrix3 <- table(test_frame$hotspot,dp_prediction3$predict)
    
    # svm_matrix <- table(test_frame$hotspot,svm_prediction$predict)
    # knn1_matrix <-  table(test_frame$hotspot,knn1_prediction)
    # knn3_matrix <-  table(test_frame$hotspot,knn3_prediction)
    # knn5_matrix <-  table(test_frame$hotspot,knn5_prediction)
    
    #model_rf_list[[i]] <- model_rf
    #model_dp_list[[i]] <- model_dp
    model_dp_list2[[i]] <- model_dp2
    model_dp_list3[[i]] <- model_dp3
    
    #model_svm_list[[i]] <- model_svm
    
    #----
    #matrix_rf_list[[i]] <- rf_matrix
    #matrix_dp_list[[i]] <- dp_matrix
    matrix_dp_list2[[i]] <- dp_matrix2
    matrix_dp_list3[[i]] <- dp_matrix3
    #matrix_svm_list[[i]] <- svm_matrix
    
    #----
    #rf_prediction_list[[i]] <- rf_prediction
    #dp_prediction_list[[i]] <- dp_prediction
    dp_prediction_list2[[i]] <- dp_prediction2
    dp_prediction_list3[[i]] <- dp_prediction3
    
    #svm_prediction_list[[i]] <- svm_prediction
    
    #----
    #rf_per[i,] <- model_performance(rf_matrix)
    #dp_per[i,] <- model_performance(dp_matrix)
    dp_per2[i,] <- model_performance(dp_matrix2)
    dp_per3[i,] <- model_performance(dp_matrix3)
    # svm_per[i,] <- model_performance(svm_matrix)
    # knn1_per[i,] <- model_performance(knn1_matrix)
    # knn3_per[i,] <- model_performance(knn3_matrix)
    # knn5_per[i,] <- model_performance(knn5_matrix)
    # 
  }
  
  #rf_per
  #dp_per
  #svm_per
  
  #write.csv(rf_per,paste0("./performance/rf_per_","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  #write.csv(dp_per,paste0("./performance/dp_ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(dp_per2,paste0("./performance/dp_noloc_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(dp_per3,paste0("./performance/dp_onlyloc_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  
  # write.csv(svm_per,paste0("./performance/svm_per","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # write.csv(knn1_per,paste0("./performance/knn1_per","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # write.csv(knn3_per,paste0("./performance/knn3_per","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # write.csv(knn5_per,paste0("./performance/knn5_per","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # 
  
  #rf_per_mean <- data.frame(PPV = mean(rf_per$PPV),TPR = mean(rf_per$TPR),NPV = mean(rf_per$NPV),TNR = mean(rf_per$TNR),accuracy = mean(rf_per$accuracy),F1 = mean(rf_per$F1))
  #dp_per_mean <- data.frame(PPV = mean(dp_per$PPV),TPR = mean(dp_per$TPR),NPV = mean(dp_per$NPV),TNR = mean(dp_per$TNR),accuracy = mean(dp_per$accuracy),F1 = mean(dp_per$F1))
  dp_per_mean2 <- data.frame(PPV = mean(dp_per2$PPV),TPR = mean(dp_per2$TPR),NPV = mean(dp_per2$NPV),TNR = mean(dp_per2$TNR),accuracy = mean(dp_per2$accuracy),F1 = mean(dp_per2$F1))
  dp_per_mean3 <- data.frame(PPV = mean(dp_per3$PPV),TPR = mean(dp_per3$TPR),NPV = mean(dp_per3$NPV),TNR = mean(dp_per3$TNR),accuracy = mean(dp_per3$accuracy),F1 = mean(dp_per3$F1))
  
  # svm_per_mean <- data.frame(PPV = mean(svm_per$PPV),TPR = mean(svm_per$TPR),NPV = mean(svm_per$NPV),TNR = mean(svm_per$TNR),accuracy = mean(svm_per$accuracy),F1 = mean(svm_per$F1))
  # knn1_per_mean <- data.frame(PPV = mean(knn1_per$PPV),TPR = mean(knn1_per$TPR),NPV = mean(knn1_per$NPV),TNR = mean(knn1_per$TNR),accuracy = mean(knn1_per$accuracy),F1 = mean(knn1_per$F1))
  # knn3_per_mean <- data.frame(PPV = mean(knn3_per$PPV),TPR = mean(knn3_per$TPR),NPV = mean(knn3_per$NPV),TNR = mean(knn3_per$TNR),accuracy = mean(knn3_per$accuracy),F1 = mean(knn3_per$F1))
  # knn5_per_mean <- data.frame(PPV = mean(knn5_per$PPV),TPR = mean(knn5_per$TPR),NPV = mean(knn5_per$NPV),TNR = mean(knn5_per$TNR),accuracy = mean(knn5_per$accuracy),F1 = mean(knn5_per$F1))
  
  
  #rf_per_mean
  #dp_per_mean
  #svm_per_mean
  
  #write.csv(rf_per_mean,paste0("./performance/rf_permean_","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  #write.csv(dp_per_mean,paste0("./performance/dp_meanALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(dp_per_mean2,paste0("./performance/dp_meannoloc_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  write.csv(dp_per_mean3,paste0("./performance/dp_meanonlyloc_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  
  # write.csv(svm_per_mean,paste0("./performance/svm_permean_","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # write.csv(knn1_per_mean,paste0("./performance/knn1_per_mean","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # write.csv(knn3_per_mean,paste0("./performance/knn3_per_mean","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
  # write.csv(knn5_per_mean,paste0("./performance/knn5_per_mean","ALL_",base^2,".csv"),row.names = F,fileEncoding = "big5")
}
dp_prediction_list[[1]]$predict

