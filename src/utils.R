dist.between.latlong.points <- function (long1, lat1, long2, lat2)
{
  Radius <- 6371
  dlat <- to.radians(lat2-lat1)
  dlon <- to.radians(long2-long1)
  a <- (sin(dlat/2))^2 + cos(to.radians(lat1)) * cos(to.radians(lat2)) * (sin(dlon/2))^2
  c <- 2 * sin(sqrt(a))
  R <- 6378.145
  d <- Radius * c
  
  return(d[1])
}

to.radians <- function(degree) {
  return(degree * pi / 180)
}

haversine <- Vectorize(dist.between.latlong.points)

pythagoras <-function (lon1, lat1, lon2, lat2) {
  sqr <- function(x) {return (x * x);}
  cosDeg <- function(x) {return(cos(x * pi / 180.0));}
  
  earthCyclePerimeter = 40000000.0 * cosDeg((lat1 + lat2) / 2.0);
  dx = (lon1 - lon2) * earthCyclePerimeter / 360.0;
  dy = 37000000.0 * (lat1 - lat2) / 360.0;
  
  return(sqrt(sqr(dx) + sqr(dy)))
}
euclideanDist <- function(x1,y1,x2,y2){
  return(sqrt((x1-x2)^2 + (y1-y2)^2))
}


imputeMisingWeatherFromOtherStation <- function(weather){
  weather <- weather %>% arrange(as.Date(Date))
  weather[(weather == " ")] <- NA
  weather[(weather == "M")] <- NA
  weather[(weather == "-")] <- NA
  weather[(weather == "  T")] <- NA
  weather$diff <- c(diff(as.Date(weather$Date)),0)
  dates <- weather %>% filter(diff < 2) %>% select(Date) %>% unique() %>% unlist() %>% as.Date()
  imputed <- data.frame()
  for(i in dates){
    x <- imputeWeather(weather,i)
    imputed <- bind_rows(imputed,x)
  }
  weather <- imputed
  
  write.csv(file = "input/weatherImp.csv",x = imputed)
  weather
}

imputeWeather <- function(weather, date, window = 5){
  
  station1 <- weather %>% filter(Station == 1 & as.Date(Date) <= as.Date(date) & as.Date(Date) > as.Date(date)-window)
  station2 <- weather %>% filter(Station == 2 & as.Date(Date) <= as.Date(date) & as.Date(Date) > as.Date(date)-window)
  library(zoo)
  last <- nrow(station2)
  
  for(j in 1:ncol(weather)){
    curCol <- station2[last,j]
    empty <- is.na(curCol)
    
    if(empty){
      
      station2[last,j] <- station1[last,j]
      curCol <- station2[last,j]
      empty <- is.na(curCol)
      if(empty){
        station2[last,j] <- mode(station2[1:(last-1),j],rm.na = T)
      }
    }
    curCol <- station1[last,j]
    empty <- is.na(curCol)
    
    if(empty){
      station1[empty,j] <- station2[empty,j]
      curCol <- station1[last,j]
      empty <- is.na(curCol)
      if(empty){
        station1[last,j] <- mode(station1[1:(last-1),j],rm.na = T)
      }
    }
  }
  
  return(bind_rows(station1[last,],station2[last,]))
  
}

mode <- function(x,rm.na = F) {
  
  if(rm.na){
    ux <- na.omit(unique(x) )
  } else{
    ux <- unique(x)
  }
  ux[which.max(tabulate(match(x, ux)))]
}
