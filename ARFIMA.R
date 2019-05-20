##ARFIMA
### Estimering af d-parameter med AIC og BIC
AIC_BIC_matrix_d <- c()
AIC_BIC_Resultat <- c()
for (i in 0:5) {
  for (j in 0:5) {
    AIC_value <-  AIC(fracdiff(X_t, nar = i, nma = j))
    BIC_value <- AIC(fracdiff(X_t, nar = i, nma = j),k = log(length(X_t)) )#Ved K=log(length(X_t)), gives BIC.
    
    BIC_AIC_values <- cbind(AIC_value,BIC_value,i,j,fracdiff(X_t,nar=i,nma=j)$d)
    AIC_BIC_Resultat <- rbind(AIC_BIC_Resultat,BIC_AIC_values)
    names <- c("AIC","BIC","p","q","d")
    colnames(AIC_BIC_Resultat) <- names
  }
  if (i==5 &&j==5) {
    print(AIC_BIC_Resultat)
  }
}

### AIC og BIC vælger model ###
which(AIC_BIC_Resultat[,1] == min(AIC_BIC_Resultat[,1]), arr.ind = TRUE) 
which(AIC_BIC_Resultat[,2] == min(AIC_BIC_Resultat[,2]), arr.ind = TRUE) 

###Estimering af d-parameter
AIC_p <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,1] == min(AIC_BIC_Resultat[,1]), arr.ind = TRUE),3]
AIC_q <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,1] == min(AIC_BIC_Resultat[,1]), arr.ind = TRUE),4]

BIC_p <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,2] == min(AIC_BIC_Resultat[,2]), arr.ind = TRUE),3]
BIC_q <- AIC_BIC_Resultat[which(AIC_BIC_Resultat[,2] == min(AIC_BIC_Resultat[,2]), arr.ind = TRUE),4]


d_hat_AIC <- fracdiff(X_t,nar=AIC_p,nma=AIC_q)$d;d_hat_AIC
d_hat_BIC <- fracdiff(X_t,nar=BIC_p,nma=BIC_q)$d;d_hat_BIC

#Den fraktionelle diffede tidsserie
frak_X_t <- ts(frakdiff(X_t,d_hat_BIC))

#Loop der finder den bedste model til Diffy12 ved både AIC og BIC
BIC_AIC_fmod <- c()
result_diffy12 <- c()
for (i in 0:5) {
  for (j in 0:5) {
    f_model <- sarima(frak_X_t,i,0,j,details = F,no.constant = T)
    
    BIC_AIC_fmod <- cbind(f_model$AIC,f_model$BIC,f_model$AICc,i,j)
    
    result <- rbind(result,  BIC_AIC_fmod)
    
    names <- c("AIC","BIC","AICc","p","q")
    
    colnames(result) <- c("AIC","BIC","AICc","p","q")
  }
  if (i==5&&j==5) {
    print(result)
  }
}


###################### Estimation a d-parameter med fdGPH ############################
d_hat_fdGPH <- fdGPH(X_t)$d;d_hat_fdGPH #d=0.2943173

