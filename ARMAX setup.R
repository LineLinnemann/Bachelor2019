##Opsætning af lags
# lag temp #####
#ccftemplist <- seq(from=-30, to =30, along.with = tempccf[[1]])
#meantemp <- cbind(ccftemplist,tempccf[[1]])[1:31,1:2]
#lagtemp <- meantemp[which(abs(meantemp[,2])>0.042)]
lagtemp <- c(-10,-1,0)
xregtemp <- c()
for (i in 1:length(lagtemp)) {
  xregtemp <- cbind(xregtemp,stats::lag(as.ts(data_NO1[,4]),k=(lagtemp[i])))
}

colnames(xregtemp) <- c("lag 10","lag 1","lag 0")
# model =ARIMA(3,0,0) with zero mean

# lag con #####
#ccfconlist <- seq(from=-30, to =30, length.out = 61)
#meancon <- cbind(ccfconlist,conccf[[1]])[1:31,1:2]
#lagcon <- meancon[which(abs(meancon[,2])>0.042)]
lagcon <- c(-30 ,-23, -22 ,-17 ,-16 ,-11 ,-10 , -9 , -4 , -2 ,  0)
xregcon <- c()
for (i in 1:length(lagcon)) {
  xregcon <- cbind(xregcon,stats::lag(as.ts(data_NO1[,3]),k=(lagcon[i])))
}


colnames(xregcon) <- c("lag 30","lag 23","lag 22","lag 17","lag 16","lag 11","lag 10","lag 9","lag 4","lag 2","lag 0")


# lag hydro #####
# ccfhydrolist <- seq(from=-30, to =30, length.out = 61)
# meanhydro <- cbind(ccfhydrolist,hydroccf[[1]])[1:31,1:2]
# laghydro <- meanhydro[which(abs(meanhydro[,2])>0.042)]
# 
# xreghydro <- c()
# for (i in 1:length(laghydro)) {
#   xreghydro <- cbind(xreghydro,stats::lag(as.ts(data_NO1[,2]),k=(laghydro[i])))
# }
xreghydro <- cbind(stats::lag(as.ts(data_NO1[,2]),k=(-20)),stats::lag(as.ts(data_NO1[,2]),k=(-19)),stats::lag(as.ts(data_NO1[,2]),k=(-16)))
colnames(xreghydro) <- c("lag 20","lag 19","lag 16")

# lag rain #####
xregrain <- as.data.frame(stats::lag(as.ts(WEATHER$Precipitation),k=-1))
# samlede xreg ####
xvaribale <- cbind((xregcon)[1:2191,],xreghydro[1:2191,],xregtemp[1:2191,],xregrain[1:2191,])

# model =ARIMA(2,0,0) with zero mean 




# ARFIMAX model (3,0.19,4)####
arfimax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg = xvaribale, include.mean = F)

# arfimax(3,019,4) con sumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- conxreg, include.mean = F)

# arfimax(3,019,4)  temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4), seasonal = list(order = c(0, 0, 0)),xreg <- tempxreg, include.mean = F)
# arfimax(3,019,4) hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- (xreghydro)[1:2191,], seasonal = list(order = c(0, 0, 0), include.mean = F))

# arfimax(3,019,4) rain xreg #####
rainxreg <-stats::lag(as.ts(WEATHER$Precipitation),k=-1)
rain_armax3_019_4 <- TSA::arima(frakdiff(X_t,0.19),order=c(3,0,4),xreg <- rainxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

# arfimax(3_019_4) beste model ud fra  aic  ####

startAIClag <- c()
for (i in 1:18) {
  startAIClag[i] <- AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=xvaribale[,i], include.mean = F))
}
names(startAIClag) <- colnames(xvaribale)

modellagarfimax <- as.numeric(which.min(startAIClag))# temp lag 0
best_aic_3_019_4 <- min(startAIClag)

parameterantal <- c(1:18)
diff_X_t <- frakdiff(X_t,0.19)
minaiclist <- c()
minaic <- 1
for(j in 1:10){
  minaiclist <- c()
  for  (i in parameterantal[-modellagarfimax]) {
    minaiclist <- c(minaiclist,AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=xvaribale[,c(modellagarfimax,i)], include.mean = F)))
  }
  if(min(minaiclist)<best_aic_3_019_4){
    modellagarfimax <- c(modellagarfimax,parameterantal[-modellagarfimax][as.numeric(which.min(minaiclist))])
    minaic <- AIC(TSA::arima(frakdiff(X_t,0.19), order = c(3, 0, 4),xreg=xvaribale[,modellagarfimax], include.mean = F))
    best_aic_3_019_4 <- minaic
  }
}


modellagarfimax <- c(17,18,11,16)
colnames(xvaribale)[modellagarfimax ]
ARFIMAXBESTMOD <- TSA::arima(diff_X_t , order = c(3, 0, 4),xreg=xvaribale[,modellagarfimax], include.mean = F)
ARFIMAXrmse <- rmse(diff_X_t[31:2191],as.data.frame(fitted.values(ARFIMAXBESTMOD ))[31:2191,1])
stats::AIC(ARFIMAXBESTMOD)#-4040.281
stats::BIC(ARFIMAXBESTMOD)#-3971.981
ARFIMAXrmse# 0.09578728

myrmsefi(ARFIMAXBESTMOD)

# ARFIMAX kitchensink#########

ARFIMAXKS <- TSA::arima(diff_X_t , order = c(3, 0, 4),xreg=xvaribale, include.mean = F)


# ARMA #####
arma1_2 <- TSA::arima(X_t,order=c(1,0,2), include.mean = F)

# ARMAX model (1,0,2)####

testxreg <- xvaribale

armax1_0_2 <- TSA::arima(X_t,order=c(1,0,2),xreg = xvaribale, include.mean = F)



# armax(1,0,2) consumption xreg  #######
conxreg <-as.data.frame(cbind(xregcon)[1:2191,] )

con_armax1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- conxreg, include.mean = F)

# armax(1,0,2)   temp xreg ######### 
tempxreg <-as.data.frame(cbind(xregtemp)[1:2191,] )
temp_armax1_2 <- TSA::arima(X_t,order=c(1,0,2), seasonal = list(order = c(0, 0, 0)),xreg <- tempxreg, include.mean = F)

# armax(1,0,2)  hydro xreg #####
hydroxreg <-as.data.frame(cbind(xreghydro)[1:2191,] )
hydro_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- hydroxreg, seasonal = list(order = c(0, 0, 0), include.mean = F))

# armax(1,0,2)  rain xreg #####
rain_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- xvaribale[,18], seasonal = list(order = c(0, 0, 0), include.mean = F))

# armax(1,0,2)  kitchen snik #####
kcar_armax1_2 <- TSA::arima(X_t,order=c(1,0,2),xreg <- xvaribale, seasonal = list(order = c(0, 0, 0), include.mean = F))



# auto arima med x reg ####
  aarmax <- auto.arima(X_t,xreg = as.matrix(testxreg))
  
# aic values forloop ARMAX model (1,0,2) #####
  xvaribale <-xvaribale
  
  #for (i in 1:dim(xvaribale)[1]) {
  for (j in 1:dim(xvaribale)[2]) {
    if(is.na(xvaribale[i,j])==TRUE){
      xvaribale[i,j] <- 0
    }
    
  }
  
  
  AIClagmatrix <- matrix(data=0,nrow = 18,ncol = 20)
  #rownavne <- c("c30","c23","con lag 22","con lag 17","con lag 16","con lag 11" ,"con lag 10","con lag 9","con lag 4","con lag 2","con lag 0","hydro lag 20" ,"hydro lag 19","hydro lag 16","temp lag 10","temp lag 1","temp lag 0","Rain lag 1")
  row.names(AIClagmatrix) <- colnames(xvaribale)
  
  for (i in 1:18) {
    AIClagmatrix[i,19] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,1:i]))
    AIClagmatrix[i,1:i] <- "x" 
  }
  
  
  AIClagmatrix[2:18,20] <- format(round(diff(as.numeric(AIClagmatrix[,19])), 2), nsmall = 2)
  AIClagmatrix[1:18,19] <- format(round(as.numeric(AIClagmatrix[1:18,19]), 2), nsmall = 2)
  nytestxreg <- xvaribale[,c(1,which(as.numeric(AIClagmatrix[,20])<0))]
  nyAIClagmatrix <- matrix(data=0,nrow = dim(nytestxreg)[2],ncol = (dim(nytestxreg)[2]+2))
  #rownavne <- c("c30","c23","con lag 22","con lag 17","con lag 16","con lag 11" ,"con lag 10","con lag 9","con lag 4","con lag 2","con lag 0","hydro lag 20" ,"hydro lag 19","hydro lag 16","temp lag 10","temp lag 1","temp lag 0","Rain lag 1")
  row.names(nyAIClagmatrix) <- colnames(nytestxreg)
  for (i in 1:7) {
    nyAIClagmatrix[i,8] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=nytestxreg[,1:i]))
    nyAIClagmatrix[i,1:i] <- "x" 
  }
  nyAIClagmatrix[2:7,9] <- format(round(diff(as.numeric(nyAIClagmatrix[,8])), 2), nsmall = 2)
  nyAIClagmatrix[1:7,8] <- format(round(as.numeric(nyAIClagmatrix[1:7,8]), 2), nsmall = 2)
  
  for (i in 1:50) {
    set.seed(i)
    rand <- sample(ncol(xvaribale))
    
  }
  
  
  xvaribale <-as.data.frame(cbind(xregcon[,11:1],xreghydro[,3:1],xregtemp[,3:1],as.ts(WEATHER$Precipitation))[1:2191,] )
  
  for (j in 1:dim(xvaribale)[2]) {
    if(is.na(xvaribale[i,j])==TRUE){
      xvaribale[i,j] <- 0
    }
    
  }
  
# Best AIC model for ARMAX(1,0,2) #######
  #bestem start parameter
  startAIClag <- c()

#stokastisk optimering #######
  modelsforarmax1_0_2 <- c()
  modelsforarmax1_0_2 <-c("15",-4015.749)
  
  names(startAIClag) <- colnames(xvaribale)
  for (i in 1:18) {
    startAIClag[i] <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,i], include.mean = F))
  }
  
  for (j in 1:20) {
    rand <- sample(ncol(xvaribale))
    modellag <- as.numeric(which.min(startAIClag))# temp lag 0 
    best_aic_1_0_2 <- min(startAIClag)
    for (i in rand[rand != modellag]) {
      nyaic <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,c(modellag,i)]))
      if(nyaic<best_aic_1_0_2){
        best_aic_1_0_2 <- nyaic
        modellag <- c(modellag,i)
      }else{
        next
      }
    }
    modelsforarmax1_0_2 <- rbind(modelsforarmax1_0_2,cbind(toString(modellag),best_aic_1_0_2))
  }
  AIC(arma1_2)
  
  
# en af gangen optimering ######
  modellag <- as.numeric(which.min(startAIClag))# temp lag 0 
  best_aic_1_0_2 <- min(startAIClag)
  
  parameterantal <- c(1:18)
  k <- 1
  repeat{
    for (i in parameterantal[-modellag]) {
      nyaic <- AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,c(modellag,i)], include.mean = F))
      print(i)
      if(nyaic<best_aic_1_0_2){
        best_aic_1_0_2 <- nyaic
        modellag <- c(modellag,i)
      }else{
        next
      }
    }
    k <- k+1
    if(k==5){
      break
    }
  }
# Den bedste model for en ARMAX (1_0_2) "xregtemp[, 3:1].lag 0""xregtemp[, 3:1].lag 1""as.ts(WEATHER$Precipitation)" AIC)-4039.285
  
  
  
  
  
# test best model algoritmer
# armax(1,2) beste aic  ####

armastartvalues <- c()
startAIClag <- c()
biclagarmax <- c()
rmsearmax <- c()
for (i in 1:18) {
  midmodarmax <- TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,i], include.mean = F)
  startAIClag[i] <- stats::AIC(midmodarmax)
  biclagarmax[i] <- stats::BIC(midmodarmax)
  rmsearmax[i]   <- rmse(X_t[31:2191],as.data.frame(fitted.values(midmodarmax )[31:2191])[,1])

}
armastartvalues <- cbind(startAIClag,biclagarmax,rmsearmax)
row.names(armastartvalues) <- colnames(xvaribale)

modellagarmax <- as.numeric(which.min(startAIClag))# temp lag 0
best_aic_1_0_2 <- min(startAIClag)

parameterantal <- c(1:18)
minaiclistar <- c()
minaicar <- 1
for(j in 1:6){
  minaiclistar <- c()
  for  (i in parameterantal[-modellagarmax]) {
    minaiclistar <- c(minaiclistar,stats::AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,c(modellagarmax,i)], include.mean = F)))
  }
  if(min(minaiclistar)<best_aic_1_0_2){
    modellagarmax <- c(modellagarmax,parameterantal[-modellagarmax][as.numeric(which.min(minaiclistar))])
    minaicar <- stats::AIC(TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,modellagarmax], include.mean = F))
    best_aic_1_0_2 <- minaicar
  }
}
modellagarmax <- c(17,16,18)
colnames(xvaribale)[modellagarmax]

bestarmax <- TSA::arima(X_t, order = c(1, 0, 2),xreg=xvaribale[,c(modellagarmax)], include.mean = F)


# armax models #####
armaxmodels <- c(arma1_2,armax1_0_2,con_armax1_2,hydro_armax1_2,temp_armax1_2,rain_armax1_2,kcar_armax1_2,bestarmax)

