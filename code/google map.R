library(sp)
library(rgdal)
library(raster)
library(maptools)
library(rjson)
library(RCurl)
library(jsonlite)

setwd("C:/crime predition project")
crs_string <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#----repick Taoyuan map----
County  <- readOGR(dsn="./shp",layer="County",encoding = "utf8")
TAO <- as(County, "SpatialPolygons")
TAO <- TAO[17]
plot(TAO)

cdata <- County@data[17,]
TAO2 <-  SpatialPolygonsDataFrame(TAO,cdata)
#"+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
TAO3 <-  spTransform(TAO2,CRS(crs_string))
plot(TAO3)
#----create raster----
library(raster)
x = y = 20
xy <- matrix(rnorm(x*y),x,y)
rast <- raster(xy)
extent(rast) <- c(120.982, 121.48, 24.58646, 25.12362)#taoyuan x,y boundaries
projection(rast) <- CRS(crs_string) #projection WGS84
rast2 <- rasterToPolygons(rast,dissolve=TRUE) #to spframe
plot(rast2)
ID <- c(1:(x*y))
rast2@data$ID <- c(1:nrow(rast2@data))
#----intersect two sp frame----
inTAO_400 <- intersect(rast2,TAO3)
#----
inTAO_400@data
plot(inTAO_400)
#writeOGR(obj=inTAO_400, dsn="./shp", layer="inTAO_400", driver="ESRI Shapefile") 
#----central----
#radius <= 2000
leng <- length(inTAO_400@polygons)
central <- data.frame(long=0,lat=0)[0,]
for(i in 1:leng){
  central[i,] <- inTAO_400@polygons[[i]]@labpt
}
central <- central[,c("lat","long")]
#----Place API----#
central$location <- paste(central$lat,central$long,sep=",")
radius <- "2000" #less than 50,000

code <- read.csv("type_code.csv",stringsAsFactors = F,fileEncoding = "big5")
code <- code[,1]#array

google_radarsearch <- function(central,radius = 2000,type){
  result <- data.frame(place_id="",geometry.location.lat = 0,geometry.location.lng = 0,central_lat =0, central_lng =0)[0,]
  for(i in 1:nrow(central)){
    nokeyword_url <- paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?location=",central$location[i],"&radius=",radius,"&type=",type,"&language=zh-TW&key=YOURKEY")
    json_data <- fromJSON(nokeyword_url, flatten=TRUE)
    if(json_data$status == "OK"){
      tmp <- json_data$results[c("place_id","geometry.location.lat","geometry.location.lng")]
      tmp$central_lat <- central$lat[i]
      tmp$central_lng <- central$long[i]
      result <- rbind(result,tmp)
    }
  }
  if(nrow(result) > 1){
    result2 <- result[!duplicated(result$place_id),]
    result2$type <- type
    return(result2)
  }else{
    return(NA)
  }
}

for(i in 22 : length(code)){
  result <- google_radarsearch(central,radius,code[i])
  if(!is.na(result)){
    write.csv(result,paste0("./place/",code[i],".csv"),row.names = F,fileEncoding = "big5")
  }
}

#----find place with place_id
#place_url <- paste0("https://maps.googleapis.com/maps/api/place/details/json?placeid=",json_data$results$place_id,"&language=zh-TW&key=YOURKEY")
#place_data <- fromJSON(place_url, flatten=TRUE)

