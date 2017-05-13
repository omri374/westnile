PLOT <<- FALSE
SPRAY_EFFECTIVE_DISTANCE <<- 100
library(lubridate,quietly = T)
library(dplyr,quietly = T)


nonFeatures <- c("Date","Address","sprayed2011","sprayed2013","closestStation","Street","AddressNumberAndStreet","AddressAccuracy","SnowFall","Depth","Water1")

getDataset <- function(path = "input/train.csv", 
                       requestedYears = NULL,
                       onehot = F, removeCols = T){
  #path = "input/train.csv"
  dataset <- read.csv(path,stringsAsFactors = F)
  dataset$Date <- as.Date(dataset$Date)
  

  dataset$year <- year(dataset$Date)
  if(!is.null(requestedYears)){
    dataset <- dataset %>% filter(year %in% requestedYears)
  }
  dataset$month <- month(dataset$Date)
  dataset$week <- week(dataset$Date)
  dataset$isSummer <- ifelse(dataset$month %in% c(7,8,9),1,0)

  
  ## ADD spraying features
  sprays <- read.csv("input/spray.csv",stringsAsFactors = F)
  
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
  weather$X <- NULL
  
  
  
  #Impute (takes some time)
  #weather <- imputeMisingWeatherFromOtherStation(weather)
  
  station1coord <- list(Latitude = 41.995,Longitude = -87.933)
  station2coord <- list(Latitude = 41.786,Longitude = -87.752)
  
  source("src/utils.R")
  dataset$closestStation <- ifelse(haversine(dataset$Longitude,dataset$Latitude,station1coord$Longitude,station1coord$Latitude) <
                                     haversine(dataset$Longitude,dataset$Latitude,station2coord$Longitude,station2coord$Latitude),1,2)
  
  dataset <- inner_join(dataset,weather,by = c("closestStation" = "Station", "Date"))
  
  if(onehot){
    speciesEncoded <- data.frame(stats::model.matrix(~Species,dataset))
    speciesEncoded$X.Intercept. <- NULL
    dataset$Species <- NULL
    dataset <- bind_cols(dataset,speciesEncoded)
  }
  if(removeCols){
    dataset <- dataset %>% select(-one_of(nonFeatures))
  }
  
  #Sanity for missing values
  sapply(dataset,function(x){return(length(which(complete.cases(x)))!=length(x))})
  
  
  if(!is.null(dataset$WnvPresent)){
    dataset$WnvPresent <- as.factor(ifelse(dataset$WnvPresent,"Yes","No"))
  }
  dataset
  
}

getFeatures <- function(dataset=NULL,withWmvPresent = T,...){
  if(is.null(dataset)){
    dataset <- getDataset(...)
  }
  nonFeatures <- c("Date","Trap","WmvPresent","Address","NumMosquitos","CodeSum","sprayed2011","sprayed2013","closestStation","Street","AddressNumberAndStreet","AddressAccuracy","SnowFall","Depth","Water1")
  featureset <- dataset[,setdiff(names(dataset),nonFeatures)]
  if(withWmvPresent){
    featureset$WmvPresent <- dataset$WmvPresent
  }
  featureset
  
}