library(ggplot2)
library(gridExtra)
library(reshape2)
library(dplyr)

source("src/dataprep.R",echo = F,verbose = F)
train <- getDataset()
test <- getDataset(path = "input/test.csv")
numericCols <- sapply(train,is.numeric)
categoricalCols <- sapply(train,function(x){!is.numeric(x)})
#nonFeatures <- c("Date","WnvPresent","Address","sprayed2011","sprayed2013","closestStation","Street","AddressNumberAndStreet","AddressAccuracy","SnowFall","Depth","Water1")
#featureset <- train[,setdiff(names(train),nonFeatures)]

featureset <- getFeatures(train,withWnvPresent = T)

#interesting <- c("year","NumMosquitos","Tmax","PrecipTotal","DewPoint","ResultSpeed","ResultDir","week","Heat","Species")
interesting <- names(featureset)



## Sampling to get cleaner plots
set.seed(107)
PROB = 0.9

trainNumeric <- featureset[,interesting]
featuresetS <- featureset[sample(1:nrow(featureset),replace = F,size =nrow(featureset)*PROB),]
featuresetS$PrecipTotal <- ifelse(featuresetS$PrecipTotal > 0.4,0.4,featuresetS$PrecipTotal)
featuresetS$LogHeat <- ifelse(featuresetS$Heat == 0,0,log(featuresetS$Heat))
featuresetS$Heat <- NULL

densityPlot <- function(){
  par(mfrow = c(3, 3))
  
  
  
  #ggplot(trainMelt, aes(x = variable, y = value)) + geom_density(alpha = 0.3)
  
  plotsList <- lapply(colnames(featuresetS), function(i) {
    if(is.numeric(featuresetS[,i])){
      ggplot(featuresetS, aes_string(x=i, fill = "WnvPresent")) + 
        #geom_density(alpha = 0.3,adjust = 0.4) +
        geom_histogram(alpha = 0.9,bins = 20) +
        guides(fill=FALSE) +
        scale_y_continuous(name = "",labels = function(x) as.character(round(x,2))) + ggtitle(paste("Distribution of", i))
      #facet_wrap(~ variable)
      #return(g)
    } else{
      ggplot(featuresetS, aes_string(x=i, fill = "WnvPresent")) + 
        geom_bar() +guides(fill=FALSE) +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle(paste("Distribution of", i))
    }
  })
  
  #do.call(grid.arrange, c(plotsList, ncol=3))
  dir.create("output/density")
  lapply(1:length(plotsList),function(i){ggsave(filename = paste0("output/density/density",i,".png"),plot = plotsList[[i]])})
}


savePairsPlot <- function(df,var1,var2){
  if(is.numeric(df[,var1]) | is.numeric(df[,var2])){
    g <- ggplot(df, aes_string(x=var1, y=var2,color = "WnvPresent")) + 
      geom_point(alpha = 0.3)+
      geom_density_2d()+
      geom_smooth(method=lm)
    ggsave(filename = paste0("output/pairs/pair-",var1,"-",var2,".png"),plot = g)
  } else{
    dd <- table(df[,var1],df[,var2])
    png(filename=paste0("output/pairs/pair-",var1,"-",var2,".png"))
    g <- mosaicplot(dd, col=c(2,4))
    dev.off()
  }
  
}

##Saves to file a density plot for each variable given the label
pairsPlot<- function(){
  
  
  interesting <- c("WnvPresent","year","NumMosquitos","Tmax","PrecipTotal","DewPoint","ResultSpeed","ResultDir","week","LogHeat","Species")
  numeric <- sapply(featuresetS,is.numeric)
  nonNumeric <- !numeric
  scaled <- data.frame(sapply(featuresetS[,numeric],function(x){return((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))}))
  dataset <- bind_cols(scaled,featuresetS[,nonNumeric])
  
  dir.create("output/pairs")
  
  for(i in 1:ncol(dataset)){
    for(j in 1:ncol(dataset)){
      if(i != j){
        var1 <- names(dataset)[i]
        var2 <- names(dataset)[j]
        savePairsPlot(dataset,var1,var2)
      }
    }
  }
  
  # new data frame mega
  #mega <- data.frame(pairs$all, WnvPresent=rep(trainNumericS$WnvPresent, length=nrow(pairs$all)))
  
  
  
}

trapsPlot <- function(requestedYear = 2009){
  library(ggmap)
  chi <- get_map("Chicago")
  
  grpByLocation <- train %>% filter(year == requestedYear) %>%
    group_by(year, Latitude, Longitude) %>% 
    summarize(numCollections = n(), `Total number of mosquitoes in trap` = sum(NumMosquitos),`Is WNV present` = as.factor(length(which(WnvPresent=="Yes")) > 0))
  
  
  
  g <- ggmap(chi) + 
    geom_point(aes(x = Longitude, y = Latitude,color = `Is WNV present`, size = `Total number of mosquitoes in trap`),data = grpByLocation) + 
    theme(legend.position="bottom")
  dir.create("output/years")
  ggsave(filename = paste0("output/years/",requestedYear,".png"),plot = g)
  
  g
  
}

convexHullPlot <- function(){
  #Convex hull
  library(leaflet)
  
  wnvPositive <- train[which(train$WnvPresent=="Yes"),]
  wnvNegative <- train[which(train$WnvPresent=="No"),]
  chpositive <- wnvPositive[chull(wnvPositive$Longitude,wnvPositive$Latitude),]
  
  chnegative <- wnvNegative[chull(wnvNegative$Longitude,wnvNegative$Latitude),]
  m2 = leaflet(chnegative) %>% addTiles() %>% addPolygons(
    chpositive$Longitude, chpositive$Latitude,color ="red",opacity = 0.5) %>% addPolygons(
      chnegative$Longitude, chnegative$Latitude,color = "blue",opacity = 0.5) 
  m2
  
}
clusterPlot <- function(reqYear = 2013){
  library(ggmap)
  
  sprays <- read.csv("input/spray.csv",stringsAsFactors = F)
  
  grpByLocation <- train %>% filter(year == reqYear) %>%
    group_by(Latitude, Longitude) %>% 
    summarize(totalNumMosquitos = sum(NumMosquitos),WnvPresent = as.factor(length(which(WnvPresent=="Yes"))>1))
  
  chi <- get_map("Chicago")
  library(fossil)
  lats <- cbind(grpByLocation$Longitude,grpByLocation$Latitude)
  dist <- earth.dist(lats,dist = T)
  
  #dbsc <- dbscan(data.frame(lat = grpByLocation$Latitude,lon = grpByLocation$Longitude),eps = .02,minPts = 1)
  hcl <- hclust(d = dist,method = "complete")
  clusters <- cutree(hcl,h = 6)
  
  grpByLocation$cluster <-clusters
  chullList <- list()
  for(i in 1 :length(levels(as.factor(clusters)))){
    coords <- grpByLocation %>% filter(cluster == i) %>% data.frame()
    chullList[[i]] <- coords[chull(coords),]
  }
  chull <- bind_rows(chullList)
  clusterWmv <- chull %>% group_by(cluster) %>% summarize(WnvPresentInCluster = as.factor(any(as.logical(WnvPresent))))
  
  chull2 <- left_join(chull,clusterWmv,by="cluster")
  
  head(grpByLocation)
  #plot map with clusters
  
  sprays2013 <- sprays %>% filter(year(Date) == 2013)
  spraylats <- cbind(sprays2013$Longitude,sprays2013$Latitude)
  
  spraydist <- dist(spraylats)#earth.dist(spraylats,dist = T)
  sprayshclust <- hclust(d = spraydist,method = "complete")
  sprayclusters <- cutree(sprayshclust,h = 0.5)
  spraysClst <- sprays %>% group_by(Latitude, Longitude) %>% summarize(co <- n())
  
  library(ggmap)
  g <- ggmap(chi) + 
    geom_point(aes(x = Longitude, y = Latitude),data = spraysClst,alpha = 0.5, size = 0.01,color = "yellow") + 
    geom_polygon(aes(x= Longitude, y = Latitude, group = as.factor(cluster),fill = WnvPresentInCluster),data =chull2,alpha = 0.9) + 
    geom_point(aes(x = Longitude, y = Latitude,color = WnvPresent),data = grpByLocation,size = 3,alpha = 0.7)
  g
  
  
}

nuMosquitosPlot <- function(){
  #NumMosquitos
  ggplot(train, aes(x=NumMosquitos)) +
    geom_histogram() + facet_grid(WnvPresent~.) + ggtitle("# of mosquitos in trap with or without WMV existence")
  
}

speciesPlot <- function(){
  
  #species
  library(grid)
  g1 <- ggplot(train, aes(x=SpeciesCULEX.PIPIENS,fill = WnvPresent)) +  geom_bar()
  g2 <- ggplot(train, aes(x=SpeciesCULEX.SALINARIUS,fill = WnvPresent)) +  geom_bar() 
  g3 <- ggplot(train, aes(x=SpeciesCULEX.PIPIENS.RESTUANS,fill = WnvPresent)) +  geom_bar() 
  g4 <- ggplot(train, aes(x=SpeciesCULEX.TARSALIS,fill = WnvPresent)) +  geom_bar() 
  g5 <- ggplot(train, aes(x=SpeciesCULEX.RESTUANS,fill = WnvPresent)) +  geom_bar() 
  g6 <- ggplot(train, aes(x=SpeciesCULEX.TERRITANS,fill = WnvPresent)) +  geom_bar() 
  grid.arrange(g1,g2,g3,g4,g5,g6,ncol = 3)
}

trapsStatisticsPlot <- function(){
  trapSpeciesGrp <- train %>% group_by(Trap, Species,WnvPresent, year) %>% 
    summarize(count = n(), numMosquitosSTD = sd(NumMosquitos)) %>% arrange(desc(numMosquitosSTD))
  trapSpeciesGrp$Trap <- as.factor(trapSpeciesGrp$Trap)
  trapSpeciesGrp$WNV <- ifelse(trapSpeciesGrp$WnvPresent=="No",FALSE,TRUE)
  
  stdPerYear <- trapSpeciesGrp %>% group_by(WNV, year) %>% summarize(meanSTD = mean(numMosquitosSTD,na.rm = T)) %>% arrange(year)
  #knitr::kable(stdPerYear)
  
  
  library(ggplot2)
  library(scales)
  species <- unlist(trapSpeciesGrp[1:50,'Species'])
  trap <- unlist(trapSpeciesGrp[1:50,'Trap'])
  df <- train %>% filter(species %in% as.character(species) & Trap %in% as.character(trap))
  
  dfgrp <- df %>% group_by(Trap,year) %>% summarize(AvgNumMosquitos = sum(NumMosquitos),WnvPresent = any(WnvPresent == "Yes"))
  
  
  ggplot(dfgrp, aes(year, AvgNumMosquitos,colour = Trap)) + geom_line() +
    #scale_x_date(date_breaks = "1 year") + 
    xlab("") + ylab("Num of mosquitos (log)") + scale_y_log10()  + 
    facet_grid(WnvPresent~.) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ggplot2::ggtitle("Top 50 traps with the heighest variance of number of mosquitos")
}




getPredictionStates <- function(classPrediction, probPrediction, Label, fittedModel,withTuning = T){
  
  varImp <- plot(varImp(fittedModel))
  if(withTuning){
    modelPlot <- plot(fittedModel)
  }else{
    modelPlot <- NULL
  }
  roc <- plotROC(probPrediction,Label,fittedModel)
  
  return(list(varImp,modelPlot,roc))
}

plotROC <- function(probPrediction, Label,fittedModel,addToPrev = T){
  library(ROCR)
  pred <- prediction(probPrediction[,2], Label=="Yes")
  perf <- performance(pred, "tpr", "fpr")
  roc <- plot(perf, main = paste0("ROC curve for ",fittedModel$method),add = addToPrev,colorize = T)
}

getConfusionMatrix <- function(classPrediction,Label){
  cm <- confusionMatrix(classPrediction, Label)
  cm
}
