janelas_crescentes <- function(N,n0,serie,passo,lambda){
  y <- subset(serie, start = n0+1)
  yhat <- y
  for (t in n0:(N-passo)){
    if ((t-n0)%%n0 == 0){
      cat(sprintf("%d\n ...",t))
    }
    #separa a janela
    df <- subset(serie, start = 1, end=t)
    #estima modelo
    mod <- ets(df,"AAA",lambda = lambda) # su poderia selecionar o modelo tambem
    #preve 1 obs a frente
    yy <- forecast(mod,passo)$mean[passo]
    #guarda resultado
    yhat[t-n0+1] <- yy
  }
  
  yhat <- zoo(yhat)
  index(yhat) <- index(y)
  
  # RMSE
  RMSE = sqrt(mean((as.vector(yhat)-as.vector(y))^2))
  MAE = mean(abs(as.vector(yhat)-as.vector(y)))
  
  return(c(RMSE, MAE))
  
}
janelas_moveis <- function(N,n0,serie,passo,lambda){
  y <- subset(serie, start = n0+passo)
  yhat <- y
  for (t in n0:(N-passo)){
    if ((t-n0)%%n0 == 0){
      cat(sprintf("%d\n ...",t))
    }
    #separa a janela
    df <- subset(serie, start = t-n0+1, end=t)
    #estima modelo
    mod <- ets(df,"AAA",lambda = lambda) # su poderia selecionar o modelo tambem
    #preve 1 obs a frente
    yy <- forecast(mod,passo)$mean[passo]
    #guarda resultado
    yhat[t-n0+1] <- yy
  }
  
  yhat <- zoo(yhat)
  index(yhat) <- index(y)
  
  # RMSE
  RMSE = sqrt(mean((as.vector(yhat)-as.vector(y))^2))
  MAE = mean(abs(as.vector(yhat)-as.vector(y)))
  
  return(c(RMSE, MAE))
  
}
janela_fixa <- function(N,n0,serie,lambda){
  
  y <- subset(serie, start = n0+1)
  
  df <- subset(serie, start = 1, end=n0)
  #estima modelo
  mod <- ets(df,"AAA",lambda = lambda) # su poderia selecionar o modelo tambem
  yhat <- forecast(mod,N-n0)$mean
  
  yhat <- zoo(yhat)
  index(yhat) <- index(y)
  
  # RMSE
  RMSE = sqrt(mean((as.vector(yhat)-as.vector(y))^2))
  MAE = mean(abs(as.vector(yhat)-as.vector(y)))
  
  return(c(RMSE, MAE))
  
}



janelas_crescentes_arima <- function(N,n0,serie,passo,lambda){
  y <- subset(serie, start = n0+1)
  yhat <- y
  for (t in n0:(N-passo)){
    if ((t-n0)%%n0 == 0){
      cat(sprintf("%d\n ...",t))
    }
    #separa a janela
    df <- subset(serie, start = 1, end=t) 
    #estima modelo
    mod <- auto.arima(df,lambda = lambda) # su poderia selecionar o modelo tambem
    #preve 1 obs a frente
    yy <- forecast(mod,h=passo)$mean[passo]
    #guarda resultado
    yhat[t-n0+1] <- yy
  }
  
  yhat <- zoo(yhat)
  index(yhat) <- index(y)
  
  # RMSE
  RMSE = sqrt(mean((as.vector(yhat)-as.vector(y))^2))
  MAE = mean(abs(as.vector(yhat)-as.vector(y)))
  
  return(c(RMSE, MAE))
  
}
janelas_moveis_arima <- function(N,n0,serie,passo,lambda){
  y <- subset(serie, start = n0+1)
  yhat <- y
  for (t in n0:(N-passo)){
    if ((t-n0)%%n0 == 0){
      cat(sprintf("%d\n ...",t))
    }
    #separa a janela
    df <- subset(serie, start = t-n0+1, end=t)
    #estima modelo
    mod <- auto.arima(df,lambda = lambda) # su poderia selecionar o modelo tambem
    #preve 1 obs a frente
    yy <- forecast(mod,h=passo)$mean[passo]
    #guarda resultado
    yhat[t-n0+1] <- yy
  }
  
  yhat <- zoo(yhat)
  index(yhat) <- index(y)
  
  # RMSE
  RMSE = sqrt(mean((as.vector(yhat)-as.vector(y))^2))
  MAE = mean(abs(as.vector(yhat)-as.vector(y)))
  
  return(c(RMSE, MAE))
  
}
janela_fixa_arima <- function(N,n0,serie,lambda){
  
  y <- subset(serie, start = n0+1)
  
  df <- subset(serie, start = 1, end=n0)
  #estima modelo
  mod <- auto.arima(df,lambda = lambda) # su poderia selecionar o modelo tambem
  yhat <- forecast(mod,h=N-n0)$mean
  
  yhat <- zoo(yhat)
  index(yhat) <- index(y)
  
  # RMSE
  RMSE = sqrt(mean((as.vector(yhat)-as.vector(y))^2))
  MAE = mean(abs(as.vector(yhat)-as.vector(y)))
  
  return(c(RMSE, MAE))
  
}

