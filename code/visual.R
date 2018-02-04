#----baseline plot---- 
library(ggplot2)
library(dplyr)
setwd("C:/crime predition project")

files <- list.files("./performance/baseline/")
files <- files[grepl("11m_mean",files)]
pf <- data.frame(PPV = 0,TPR = 0,NPV = 0,TNR = 0,accuracy = 0,F1 = 0)[0,]
for(i in 1 : length(files)){
  pf[i,] <- read.csv(paste0("./performance/baseline/",files[i]),stringsAsFactors = F,fileEncoding = "big5")
  row.names(pf)[i] <- files[i]
}

pf$grid <- row.names(pf)
pf$grid <- gsub("11m_mean","",pf$grid)
pf$grid <- gsub(".csv","",pf$grid)
pf$grid <- as.integer(pf$grid)
pf <- pf[order(pf$grid),]

pf$Algorithm <- "baseline"

df <- pf
df2 <- df2[df2$Algorithm != "DNN-tunning",]
df <- rbind(df ,pf)

write.csv(df,"ALL_loc.csv",row.names = F,fileEncoding = "big5")

#----no NPV & TNR----
g1 <- data.frame(grid = pf$grid,value = pf$PPV,type = "Precision",stringsAsFactors = F)
g2 <- data.frame(grid = pf$grid,value = pf$TPR,type = "Recall",stringsAsFactors = F)
g3 <- data.frame(grid = pf$grid,value = pf$accuracy,type = "Accuracy",stringsAsFactors = F)
g4 <- data.frame(grid = pf$grid,value = pf$F1,type = "F1",stringsAsFactors = F)

g <- rbind(g1,g2,g3,g4)
g$type <- factor(g$type,levels = c("Precision","Recall","Accuracy","F1"))

png(file="dp_location.png", units="in", width=8, height=6, res=600)
ggplot(g, aes(grid,value,group = type,colour = type)) +
  geom_line(size=1) +
  geom_point(aes(shape = type),size = 3) +
  ylim(0,1)+
  theme_classic() +
  theme(axis.text.x=element_text(angle=-30)) +
  labs(x= "Grids ",y = "Performance",colour ="Measure Types",shape = "Measure Types") 
dev.off()



#---
str(crime_sta)
crime_sta$time <- gsub("104","2015",crime_sta$time)
crime_sta$time <- gsub("105","2016",crime_sta$time)
crime_sta$time <- gsub("106","2017",crime_sta$time)
crime_sta$year <- substr(crime_sta$time,1,4)
str(crime_sta)
crime_sta$year <- factor(crime_sta$year)
crime_sta$time <- factor(crime_sta$time)

mean(crime_sta[crime_sta$year == 2017,"count"])


png(file="crime_sta.png", units="in", width=8, height=6, res=600)
ggplot(crime_sta,aes(time,count,label = count,fill = year)) +
  geom_bar(stat="identity",width = 0.7) +
  geom_text(size=3,position = position_stack(vjust = 0.5)) +
  theme_classic()+
  coord_flip()
dev.off()
#---
varimp <- read.csv("./performance/varimp.csv",stringsAsFactors = F,fileEncoding = "big5")
varimp <- varimp[1:13,]
varimp$features <- factor(varimp$features,levels = rev(varimp$features))
varimp[varimp$count == 8,"group"] <- 1
varimp[varimp$count == 5,"group"] <- 2
varimp[varimp$count == 3,"group"] <- 3
varimp[varimp$count == 2,"group"] <- 4
varimp$group <- factor(varimp$group)


png(file="varimp_sta.png", units="in", width=8, height=6, res=600)
ggplot(varimp,aes(features,count,label = count,fill = group)) +
  geom_bar(stat="identity",width = 0.5) +
  geom_text(size=4,position = position_stack(vjust = 0.5)) +
  theme_classic()+
  coord_flip() +
  labs(x= "Feature",y = "Count") +
  theme(legend.position="none")
dev.off()




#----
df2$Algorithm[df2$Algorithm == "MA-11"] <- "Baseline"
df2$Algorithm[df2$Algorithm == "knn-5"] <- "KNN-5"
df2$Algorithm[df2$Algorithm == "svm"] <- "SVM"

png(file="dp_location.png", units="in", width=8, height=6, res=600)
ggplot(df, aes(grid,F1,group = Algorithm,colour = Algorithm)) +
  geom_line(size=1) +
  geom_point(aes(shape = Algorithm),size = 3) +
  labs(x="grid",y="performance",colour="Algorithm") +
  ylim(0.4,1)+
  theme_classic() +
  theme(axis.text.x=element_text(angle=-30)) +
  labs(x= "Grids ",y = "Performance",colour ="Location_stat",shape = "Location_stat") 
dev.off()





#----
g1 <- data.frame(grid = pf$grid,value = pf$PPV,type = "Precision",stringsAsFactors = F)
g2 <- data.frame(grid = pf$grid,value = pf$TPR,type = "Recall",stringsAsFactors = F)
g3 <- data.frame(grid = pf$grid,value = pf$NPV,type = "NPV",stringsAsFactors = F)
g4 <- data.frame(grid = pf$grid,value = pf$TNR,type = "TNR",stringsAsFactors = F)
g5 <- data.frame(grid = pf$grid,value = pf$accuracy,type = "Accuracy",stringsAsFactors = F)
g6 <- data.frame(grid = pf$grid,value = pf$F1,type = "F1",stringsAsFactors = F)

g <- rbind(g1,g2,g3,g4,g5,g6)
g$type <- factor(g$type,levels = c("Precision","Recall","NPV","TNR","Accuracy","F1"))


png(file="dp_permean_ALL_9drop.png", units="in", width=8, height=6, res=600)
ggplot(g, aes(grid,value,group = type,colour = type)) +
  geom_line(size=1) +
  geom_point(aes(shape = type),size = 3) +
  labs(x="grid",y="performance",colour="type") +
  ylim(0,1)+
  #scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) +
  theme_classic() +
  theme(axis.text.x=element_text(angle=-30))
dev.off()

#------
per_dp <- read.csv("./performance/dfp/dfp_dp.csv",stringsAsFactors = F,fileEncoding = "big5")
per_11 <- read.csv("./performance/dfp/dfp_11.csv",stringsAsFactors = F,fileEncoding = "big5")

per_dp$type <- "DNN"
per_dp$month <- 201702:201708
per_11$type <- "11m"
per_11$month <- 201702:201708

per <- rbind(per_dp,per_11)

g1 <- data.frame(month = per_dp$month,value = per_dp$PPV,type = "Precision",Algorithm ="DNN-tuning",stringsAsFactors = F)
g2 <- data.frame(month = per_dp$month,value = per_dp$TPR,type = "Recall",Algorithm ="DNN-tuning",stringsAsFactors = F)
g3 <- data.frame(month = per_dp$month,value = per_dp$accuracy,type = "Accuracy",Algorithm ="DNN-tuning",stringsAsFactors = F)
g4 <- data.frame(month = per_dp$month,value = per_dp$F1,type = "F1",Algorithm ="DNN-tuning",stringsAsFactors = F)

g5 <- data.frame(month = per_11$month,value = per_11$PPV,type = "Precision",Algorithm ="Baseline",stringsAsFactors = F)
g6 <- data.frame(month = per_11$month,value = per_11$TPR,type = "Recall",Algorithm ="Baseline",stringsAsFactors = F)
g7 <- data.frame(month = per_11$month,value = per_11$accuracy,type = "Accuracy",Algorithm ="Baseline",stringsAsFactors = F)
g8 <- data.frame(month = per_11$month,value = per_11$F1,type = "F1",Algorithm ="Baseline",stringsAsFactors = F)


g <- rbind(g1,g2,g3,g4,g5,g6,g7,g8)


#g$type <- factor(g$type,levels = c("Precision","Recall","Accuracy","F1"))

g <- g[g$type == "F1",]

png(file="df_F1.png", units="in", width=8, height=6, res=600)
ggplot(g,aes(month,value,group = Algorithm,colour = Algorithm)) +
  geom_line(size=1) +
  geom_point(aes(shape = Algorithm),size = 3) +
  labs(x="grid",y="performance",colour="type") +
  ylim(0,1)+
  scale_x_continuous(breaks = 201702:201708) +
  theme_classic() +
  labs(x= "Time ",y = "Performance",colour ="Algorithm") 
dev.off()

