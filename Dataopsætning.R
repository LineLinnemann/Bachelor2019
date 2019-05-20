#Data opsætning

PRICES_list <- list.files("PRICES", full.names = 1)
HYDRO_list <- list.files("HYDRO", full.names = 1)
CONSUMPTION_list <- list.files("CONSUMPTION", full.names = 1)
WEATHER_list <- list.files("WEATHER", full.names = 1)

PRICES <- read.csv2(PRICES_list[1], header = TRUE)[,c(10:15)]
HYDRO <- read.csv2(HYDRO_list[1], header = TRUE)[,c(2:3)]
CONSUMPTION <- read.csv2(CONSUMPTION_list[1], header = TRUE)[,c(2:7)]
WEATHER <- read.csv2(WEATHER_list[1], header = TRUE,skip = 1)[1:2191,c(3,6)]

WEATHER$Precipitation <- as.numeric(gsub(",", ".", WEATHER$Precipitation,ignore.case = "."))
for (i in 1:length(WEATHER$Precipitation)) {
  if(is.na(WEATHER$Precipitation[i])==TRUE){
    WEATHER$Precipitation[i] <- 0
  }else{
    
  }
  
  for (i in 2:6) {
    PRICES <- rbind(PRICES,read.csv2(PRICES_list[i], header = TRUE)[,c(10:15)])
    HYDRO <- rbind(HYDRO,read.csv2(HYDRO_list[i], header = TRUE)[,c(2:3)])
    CONSUMPTION <- rbind(CONSUMPTION,read.csv2(CONSUMPTION_list[i], header = TRUE)[,c(2:7)])
  }
  hydrolang <- rep(HYDRO$NO,each=7)
  pris <- cbind(dato,PRICES)
  hydro2 <- cbind(dato2,HYDRO)
  
  dagligpris <- as.data.frame(mutate(dplyr::select(pris,dato,Oslo),weekday = wday(dato)))
  dagligpris[,2] <-log(dagligpris[,2])
  
  
### Hverdags/weekend dummies
  hverdagspris <- filter(dagligpris,weekday==2|weekday==3|weekday==4|weekday==5|weekday==6)
  weekendpris <- filter(dagligpris,weekday==1|weekday==7)
  
  
  helligdage <- c("2013-01-01 12:00:00 GMT","2013-03-28 12:00:00 GMT","2013-03-29 12:00:00 GMT","2013-04-01 12:00:00 GMT","2013-05-01 12:00:00 GMT","2013-05-09 12:00:00 GMT","2013-05-17 12:00:00 GMT","2013-05-20 12:00:00 GMT","2013-12-25 12:00:00 GMT","2013-12-26 12:00:00 GMT","2014-01-01 12:00:00 GMT","2014-04-17 12:00:00 GMT","2014-04-18 12:00:00 GMT","2014-04-21 12:00:00 GMT","2014-05-01 12:00:00 GMT","2014-05-29 12:00:00 GMT","2014-06-09 12:00:00 GMT","2014-12-25 12:00:00 GMT","2014-12-26 12:00:00 GMT","2015-01-01 12:00:00 GMT","2015-04-02 12:00:00 GMT","2015-04-03 12:00:00 GMT","2015-04-06 12:00:00 GMT","2015-05-01 12:00:00 GMT","2015-05-14 12:00:00 GMT","2015-05-25 12:00:00 GMT","2015-12-25 12:00:00 GMT","2015-12-26 12:00:00 GMT","2016-01-01 12:00:00 GMT","2016-03-24 12:00:00 GMT","2016-03-25 12:00:00 GMT","2016-03-28 12:00:00 GMT","2016-05-05 12:00:00 GMT","2016-05-16 12:00:00 GMT","2016-05-17 12:00:00 GMT","2016-12-25 12:00:00 GMT","2016-12-26 12:00:00 GMT","2017-04-13 12:00:00 GMT","2017-04-14 12:00:00 GMT","2017-04-17 12:00:00 GMT","2017-05-01 12:00:00 GMT","2017-05-17 12:00:00 GMT","2017-05-25 12:00:00 GMT","2017-12-25 12:00:00 GMT","2017-12-26 12:00:00 GMT","2018-01-01 12:00:00 GMT","2018-03-29 12:00:00 GMT","2018-03-30 12:00:00 GMT","2018-04-02 12:00:00 GMT","2018-05-01 12:00:00 GMT","2018-05-10 12:00:00 GMT","2018-05-17 12:00:00 GMT","2018-05-21 12:00:00 GMT","2018-12-25 12:00:00 GMT","2018-12-26 12:00:00 GMT")
  
  helligdage = strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")
  
  
  helligedage2019 <- c("2019-01-01 12:00:00 GMT",
                       "2019-04-18 12:00:00 GMT",
                       "2019-04-19 12:00:00 GMT",
                       "2019-04-22 12:00:00 GMT",
                       "2019-05-01 12:00:00 GMT",
                       "2019-05-17 12:00:00 GMT",
                       "2019-05-30 12:00:00 GMT",
                       "2019-06-10 12:00:00 GMT",
                       "2019-12-25 12:00:00 GMT",
                       "2019-12-26 12:00:00 GMT")
  
  helligedage2019 <-  strptime(helligedage2019, format = "%Y-%m-%d %H:%M:%S", "GMT")
  helligdage <-  strptime(helligdage, format = "%Y-%m-%d %H:%M:%S", "GMT")
  
  dato3 <- strptime(dato, format = "%Y-%m-%d %H:%M:%S", "GMT")
  
  match(helligdage,dato3)
  
  dummyhelligdage <- numeric(length = length(dato3))
  dummyhelligdage[match(helligdage,dato3)] <- 1
  
  weekend <- strptime(weekendpris[,1],format = "%Y-%m-%d %H:%M:%S", "GMT")
  
  dummyweekend <- numeric(length = length(dato3))
  
  
  dummyweekend[match(weekend,dato3)] <- 1
  
  ###sæson cleaning
  dummyhelligweekend <- dummyhelligdage+dummyweekend
  
  for (i in 1:length(dummyhelligweekend)) {
    if (dummyhelligweekend[i]==2){
      dummyhelligweekend[i]=1
    }
  }
  ## dummy spiks
  model1 <- glm(dagligpris[,2]~
                  time(dagligpris[,1])+
                  I(time(dagligpris[,1])^2)+
                  cos((2*pi/365)*I(time(dagligpris[,1])))+
                  sin((2*pi/365)*I(time(dagligpris[,1])))+
                  cos((4*pi/365)*I(time(dagligpris[,1])))+
                  sin((4*pi/365)*I(time(dagligpris[,1])))+
                  dummyhelligweekend)
  summary(model1)
  
  #vores nye tidsrække før spikes
  X_t <- ts(model1$residuals)