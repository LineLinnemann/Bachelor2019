### Prewhitening ###
### #################### Mean.temp ###################
temp <- ts(WEATHER[,1])
model2 <- glm(temp~I(time(dagligpris[,1]))+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))
fittemp <- auto.arima(model2$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)#AR(3)


########################## Consumption##################
consump <-ts(CONSUMPTION[,1])

##Vi laver 2 matricer en for hverdags  og en for weekend priser
hverdag_consump <- filter(consump_daligi,weekday==2|weekday==3|weekday==4|weekday==5|weekday==6)
weekend_consump <- filter(consump_daligi,weekday==1|weekday==7)


### splitter data om og tager gennemsnit af hverdage og weekender
k <-split(consump_daligi,1:7)
consump_split <- cbind(k$`1`[,1],k$`2`[,1],k$`3`[,1],k$`4`[,1],k$`5`[,1],k$`6`[,1],k$`7`[,1])
gennemsnit <- c()
for (i in 1:7) {
  gennemsnit <- rbind(gennemsnit,mean(consump_split[,i]))  
  if (i==7) {
    gennemsnit <- as.data.frame(gennemsnit)
    names(gennemsnit) #<- rbind("Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday","Monday")
    gennemsnit <- cbind(1:7,gennemsnit)
    print(gennemsnit)
  }
}

gennemsnit <- c(gennemsnit)
##E Deterministisk trend og sæson corrigering
model3 <- glm(consump~
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1])))+dummyhelligweekend)

#Den fittede model på consumption
fitcon <- auto.arima(model3$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F)

############ Hydro #####
#fjerner trend og sæson
model5 <- glm(hydrodayli~
                time(dagligpris[,1])+
                I(time(dagligpris[,1])^2)+
                cos((2*pi/365)*I(time(dagligpris[,1])))+
                sin((2*pi/365)*I(time(dagligpris[,1])))+
                cos((4*pi/365)*I(time(dagligpris[,1])))+
                sin((4*pi/365)*I(time(dagligpris[,1]))))

#estimation af model
fithy <- auto.arima(model5$residuals,stepwise=FALSE, approximation=FALSE,allowmean = F) #AR(2)