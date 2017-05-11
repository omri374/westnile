PLOT <<- FALSE
SPRAY_EFFECTIVE_DISTANCE <<- 100
library(lubridate,quietly = T)
library(dplyr,quietly = T)
getDataset <- function(path = "input/train.csv"){
  #path = "input/train.csv"
  dataset <- read.csv(path,stringsAsFactors = F)
  dataset$Date <- as.Date(dataset$Date)
  
  sprays <- read.csv("input/spray.csv",stringsAsFactors = F)

  dataset$year <- year(dataset$Date)
  dataset$month <- month(dataset$Date)
  dataset$week <- week(dataset$Date)
  if(!is.null(dataset$WnvPresent)){
    dataset$Label <- as.factor(dataset$WnvPresent)
    dataset$WnvPresent <- NULL
  }
  
  ## ADD spraying features
  traps <- unique(dataset[,c("Trap","Latitude","Longitude")]) %>% data.frame()
  uniqueSprays <- unique(sprays)
  uSprays2011 <- uniqueSprays %>% filter(year(Date) == 2011)
  uSprays2013 <- uniqueSprays %>% filter(year(Date) == 2013)
  for(i in 1:nrow(traps)){
    source("src/utils.R")
    dists <- pythagoras(traps[i,"Longitude"],traps[i,"Latitude"],uSprays2011$Longitude,uSprays2011$Latitude)
    closest <- min(dists)
    traps$sprayed2011[[i]] <- closest < SPRAY_EFFECTIVE_DISTANCE
    
    dists <- pythagoras(traps[i,"Longitude"],traps[i,"Latitude"],uSprays2013$Longitude,uSprays2013$Latitude)
    closest <- min(dists)
    traps$sprayed2013[[i]] <- closest < SPRAY_EFFECTIVE_DISTANCE
  }
  
  dataset <- inner_join(dataset,traps,by = c("Trap" = "Trap", "Latitude" = "Latitude", "Longitude" = "Longitude"))
  
  
  ## ADD weather features
  ## Clean / remove missing
  weather <- read.csv("input/weatherImp.csv",stringsAsFactors = F)
  weather$Date <- as.Date(weather$Date)
  
  
  
  #Impute (takes some time)
  #weather <- imputeMisingWeatherFromOtherStation(weather)
  
  station1coord <- list(Latitude = 41.995,Longitude = -87.933)
  station2coord <- list(Latitude = 41.786,Longitude = -87.752)
  
  source("src/utils.R")
  dataset$closestStation <- ifelse(haversine(dataset$Longitude,dataset$Latitude,station1coord$Longitude,station1coord$Latitude) <
                                     haversine(dataset$Longitude,dataset$Latitude,station2coord$Longitude,station2coord$Latitude),1,2)
  
  dataset <- inner_join(dataset,weather,by = c("closestStation" = "Station", "Date"))
  
  
  speciesEncoded <- data.frame(stats::model.matrix(~Species,dataset))
  speciesEncoded$X.Intercept. <- NULL
  #dataset$Species <- NULL
  dataset <- bind_cols(dataset,speciesEncoded)
  
  
  dataset
  
}

train <- getDataset()

runme <- function(train){
  numericCols <- sapply(train,is.numeric)
 # categoricalCols <- sapply(train,function(x){!is.numeric(x)})
  nonFeatures <- c("Date","Label")
  
  
  
  # Univariate
  # 

  
  #-------- PLOTTING ---------#

  trainNumeric <- data.frame(sapply(trainNumeric,function(x){return((x - min(x,na.rm = T))/(max(x,na.rm = T) - min(x,na.rm = T)))}))
 # trainNumeric$Label <- train$Label
  trainNumericSampled <- trainNumeric[sample(1:nrow(trainNumeric),replace = F,size = nrow(trainNumeric)*0.4),]
  
  source("src/makePairs.R")
  # expand iris data frame for pairs plot
  if(PLOT){
    gg1 = makePairs(trainNumericSampled[,-8])
    
    # new data frame mega
    mega = data.frame(gg1$all, Label=rep(trainNumericSampled$Label, length=nrow(gg1$all)))
    library(ggplot2)
    # pairs plot
    ggplot(mega, aes_string(x = "x", y = "y")) + 
      facet_grid(xvar ~ yvar, scales = "free") + 
      geom_point(aes(colour=Label), na.rm = TRUE, alpha=0.8) + 
      stat_density(aes(x = x, y = ..scaled.. * diff(range(x)) + min(x)), 
                   data = gg1$densities, position = "identity", 
                   colour = "grey20", geom = "line")
    
    
    
    
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
  #------ Clustering ------#
  
  library(fpc)
  EPS <- 0.15
  MINPTS <- 5
  
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
  
  
  
  #NumMosquitos
  library(ggplot2)
  ggplot(train, aes(x=NumMosquitos)) +
    geom_histogram() + facet_grid(Label~.)
  
  #species
  library(grid)
  library(gridExtra)
  library(ggplot2)
  g1 <- ggplot(train, aes(x=SpeciesCULEX.PIPIENS,fill = Label)) +  geom_bar()
  g2 <- ggplot(train, aes(x=SpeciesCULEX.SALINARIUS,fill = Label)) +  geom_bar() 
  g3 <- ggplot(train, aes(x=SpeciesCULEX.PIPIENS.RESTUANS,fill = Label)) +  geom_bar() 
  g4 <- ggplot(train, aes(x=SpeciesCULEX.TARSALIS,fill = Label)) +  geom_bar() 
  g5 <- ggplot(train, aes(x=SpeciesCULEX.RESTUANS,fill = Label)) +  geom_bar() 
  g6 <- ggplot(train, aes(x=SpeciesCULEX.TERRITANS,fill = Label)) +  geom_bar() 
  grid.arrange(g1,g2,g3,g4,g5,g6,ncol = 3)
  
  
  
  #### MORE TO COME ####
  # look at the traps with the highest change over time
  # Analyze weather
  # Build model first for one year and then for all the years (with trend per area / cluster / trap)
  # 
  
  
  ###### group by trap ######
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