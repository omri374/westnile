library(ggplot2)
library(gridExtra)
library(reshape2)


source("src/dataprep.R",echo = F,verbose = F)
train <- getDataset()

numericCols <- sapply(train,is.numeric)
categoricalCols <- sapply(train,function(x){!is.numeric(x)})
nonFeatures <- c("Date","Label","Address","sprayed2011","sprayed2013","closestStation","Street","AddressNumberAndStreet","AddressAccuracy","SnowFall","Depth","Water1")
featureset <- train[,setdiff(names(train),nonFeatures)]
#interesting <- c("year","NumMosquitos","Tmax","PrecipTotal","DewPoint","ResultSpeed","ResultDir","week","Heat","Species")
interesting <- names(featureset)


trainNumeric <- featureset[,interesting]
featureset$Label <- as.factor(train$Label)
featuresetS <- featureset[sample(1:nrow(featureset),replace = F,size =nrow(featureset)*0.4),]
featuresetS$PrecipTotal <- ifelse(featuresetS$PrecipTotal > 0.4,0.4,featuresetS$PrecipTotal)
featuresetS$LogHeat <- ifelse(featuresetS$Heat == 0,0,log(featuresetS$Heat))
featuresetS$Heat <- NULL

densityPlot <- function(){
  par(mfrow = c(3, 3))
  
  
  
  #ggplot(trainMelt, aes(x = variable, y = value)) + geom_density(alpha = 0.3)
  
  plotsList <- lapply(colnames(featuresetS), function(i) {
    if(is.numeric(featuresetS[,i])){
      ggplot(featuresetS, aes_string(x=i, fill = "Label")) + 
        #geom_density(alpha = 0.3,adjust = 0.4) +
        geom_histogram(alpha = 0.9,bins = 20) +
        guides(fill=FALSE) +
        scale_y_continuous(name = "",labels = function(x) as.character(round(x,2))) + ggtitle(paste("Distribution of", i))
      #facet_wrap(~ variable)
      #return(g)
    } else{
      ggplot(featuresetS, aes_string(x=i, fill = "Label")) + 
        geom_bar() +guides(fill=FALSE) +theme(axis.text.x = element_text(angle = 45, hjust = 1))+ ggtitle(paste("Distribution of", i))
    }
  })
  
  #do.call(grid.arrange, c(plotsList, ncol=3))
  lapply(1:length(plotsList),function(i){ggsave(filename = paste0("output/density/density",i,".png"),plot = plotsList[[i]])})
}


savePairsPlot <- function(df,var1,var2){
  g <- ggplot(df, aes_string(x=var1, y=var2,color = "Label")) + 
    geom_point()+
    geom_smooth(method=lm)
  ggsave(filename = paste0("output/pairs/pair-",var1,"-",var2,".png"),plot = g)
}

pairsPlot<- function(){

  
  
  numeric <- sapply(featuresetS,is.numeric)
  nonNumeric <- !numeric
  scaled <- data.frame(sapply(featuresetS[,numeric],function(x){return((x - min(x,na.rm = T)) / (max(x,na.rm = T) - min(x,na.rm = T)))}))
  dataset <- bind_cols(scaled,featuresetS[,nonNumeric])
  #pairs <- makePairs(dataset)
  
  for(i in 1:ncol(dataset)){
    for(j in 1:ncol(dataset)){
      if(i != j){
        var1 <- names(dataset)[i]
        var2 <- names(dataset)[j]
        savePairPlot(dataset,var1,var2)
      }
    }
  }
  
  # new data frame mega
  #mega <- data.frame(pairs$all, Label=rep(trainNumericS$Label, length=nrow(pairs$all)))
  
  

}

convexHullPlot <- function(){
  #Convex hull
  library(leaflet)
  
  wnvPositive <- train[which(train$Label==1),]
  wnvNegative <- train[which(train$Label==0),]
  chpositive <- wnvPositive[chull(wnvPositive$Longitude,wnvPositive$Latitude),]
  
  chnegative <- wnvNegative[chull(wnvNegative$Longitude,wnvNegative$Latitude),]
  m2 = leaflet(chnegative) %>% addTiles() %>% addPolygons(
    chpositive$Longitude, chpositive$Latitude,color ="red",opacity = 0.5) %>% addPolygons(
      chnegative$Longitude, chnegative$Latitude,color = "blue",opacity = 0.5) 
  m2
  
}
clusterPlot <- function(){
  library(fpc)
  library(ggmap)
  EPS <- 0.15
  MINPTS <- 5
  
  sprays <- read.csv("input/spray.csv",stringsAsFactors = F)
  
  grpByLocation <- train %>% 
    group_by(Latitude, Longitude) %>% 
    summarize(totalNumMosquitos = sum(NumMosquitos),Label = sum(as.numeric(Label)) > 1)
  
  chi <- get_map("Chicago")
  library(fossil)
  lats <- cbind(grpByLocation$Longitude,grpByLocation$Latitude)
  dist <- earth.dist(lats,dist = T)
  
  #dbsc <- dbscan(data.frame(lat = grpByLocation$Latitude,lon = grpByLocation$Longitude),eps = .02,minPts = 1)
  hcl <- hclust(d = dist,method = "complete")
  clusters <- cutree(hcl,h = 4)
  
  grpByLocation$cluster <-clusters
  chullList <- list()
  for(i in 1 :length(levels(as.factor(clusters)))){
    coords <- grpByLocation %>% filter(cluster == i) %>% data.frame()
    chullList[[i]] <- coords[chull(coords),]
  }
  chull <- bind_rows(chullList)
  
  head(grpByLocation)
  #plot map with clusters
  
  sprays2007 <- sprays %>% filter(year(Date) == 2007)
  spraylats <- cbind(sprays$Longitude,sprays$Latitude)
  
  spraydist <- earth.dist(spraylats,dist = T)
  sprayshclust <- hclust(d = spraydist,method = "complete")
  sprayclusters <- cutree(sprayshclust,h = 0.5)
  spraysClst <- sprays %>% group_by(Latitude, Longitude) %>% summarize(co <- n())
  
  library(ggmap)
  g <- ggmap(chi) + 
    geom_point(aes(x = Longitude, y = Latitude),data = spraysGrp,alpha = 0.5, size = 0.01,color = "yellow") + 
    geom_polygon(aes(x= Longitude, y = Latitude, group = as.factor(cluster)),data =chull) + 
    geom_point(aes(x = Longitude, y = Latitude,color = as.factor(Label)),data = grpByLocation) + 
    
    
    theme(legend.position = "none")
  g
  
  
}

nuMosquitosPlot <- function(){
  #NumMosquitos
  ggplot(train, aes(x=NumMosquitos)) +
    geom_histogram() + facet_grid(Label~.)
  
}

speciesPlot <- function(){
  
  #species
  library(grid)
  g1 <- ggplot(train, aes(x=SpeciesCULEX.PIPIENS,fill = Label)) +  geom_bar()
  g2 <- ggplot(train, aes(x=SpeciesCULEX.SALINARIUS,fill = Label)) +  geom_bar() 
  g3 <- ggplot(train, aes(x=SpeciesCULEX.PIPIENS.RESTUANS,fill = Label)) +  geom_bar() 
  g4 <- ggplot(train, aes(x=SpeciesCULEX.TARSALIS,fill = Label)) +  geom_bar() 
  g5 <- ggplot(train, aes(x=SpeciesCULEX.RESTUANS,fill = Label)) +  geom_bar() 
  g6 <- ggplot(train, aes(x=SpeciesCULEX.TERRITANS,fill = Label)) +  geom_bar() 
  grid.arrange(g1,g2,g3,g4,g5,g6,ncol = 3)
}

trapsPlot <- function(){
  trapSpeciesGrp <- train %>% group_by(Trap, Species,Label, year) %>% 
    summarize(count = n(), numMosquitosSTD = sd(NumMosquitos)) %>% arrange(desc(labelSTD))
  trapSpeciesGrp$Trap <- as.factor(trapSpeciesGrp$Trap)
  trapSpeciesGrp$WNV <- ifelse(trapSpeciesGrp$Label==0,FALSE,TRUE)
  
  stdPerYear <- trapSpeciesGrp %>% group_by(WNV) %>% summarize(meanSTD = mean(numMosquitosSTD,na.rm = T)) %>% arrange(year)
  knitr::kable(stdPerYear)
  
  
  library(ggplot2)
  library(scales)
  species <- unlist(trapSpeciesGrp[1:50,'Species'])
  trap <- unlist(trapSpeciesGrp[1:50,'Trap'])
  df <- train %>% filter(year == 2007) %>% filter(species %in% species & Trap %in% trap)
  ggplot(df, aes(as.Date(Date), NumMosquitos,colour = Trap)) + geom_line() +
    scale_x_date(date_breaks = "1 year") + xlab("") + ylab("Num of mosquitos")+
    facet_grid(wnvPositive~.) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggplot2::ggtitle("Top 50 traps with the heighest variance of number of mosquitos")
  
  library(gridExtra)
  grid.arrange(glist[[1]],glist[[2]],glist[[3]],glist[[4]],glist[[5]])
  
}
