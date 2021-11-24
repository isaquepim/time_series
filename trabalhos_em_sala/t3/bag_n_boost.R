library(HDeconometrics)
library(forecast)
library(ggplot2)
library(zoo)
library(dplyr)
#Serie escolhida: Residencial

autoplot(res)

temperatura <- read.csv("/home/isaque/Documents/fgv/TimeSeries/trabalhos_em_sala/t2/dados_fillna.csv")
temperatura
#Utilizando 12 lags passados para predicao

embed.res <- embed(res,13)
embed.res
y <- embed.res[,1]
X <- embed.res[,c(2:13)]
ts.y <- ts(y, frequency = 12, start = c(2005,1))




##### Incorporar informacao adicional #####

# No script de coleta fornecido para a A1, troquei os valores
# Para coletar a serie residencial dos meus colegas, respectivamente
# BA, MG e SP. Vou usar 3 lags passados delas
# Alem disso vou utilizar dados de temperatura na capital e nas
# duas regioes mais populosas disponiveis no INMET
# Os dados de temperatura comecam em 2006,
# a serie devera ser cortada

res.ajusted <- window(x = res, start = c(2006,11), end = c(2021,06),
                      frequency = 12)

ba.ajusted <- window(x = residencial.bh, start = c(2006,11), end = c(2021,06),
                     frequency = 12)

mg.ajusted <- window(x = residencial.mg, start = c(2006,11), end = c(2021,06),
                     frequency = 12)

sp.ajusted <- window(x = residencial.sp, start = c(2006,11), end = c(2021,06),
                     frequency = 12)

temp.ajusted <- temperatura[c(1:length(res.ajusted)),c(3:5)]

X.go <- cbind(embed(res.ajusted,13)[,c(2:13)],
              embed(ba.ajusted,13)[,c(2:4)],
              embed(mg.ajusted,13)[,c(2:4)],
              embed(sp.ajusted,13)[,c(2:4)],
              embed(temp.ajusted[,1],13)[,c(2:4)],
              embed(temp.ajusted[,2],13)[,c(2:4)],
              embed(temp.ajusted[,3],13)[,c(2:4)])
X.go
y.go <- embed(res.ajusted,13)[,1]
ts.y.go <- ts(y.go, frequency = 12, start = c(2007,11))


###### Janelas Moveis Bagging ######


janelas_moveis <- function(X,y,n0,passo){
  N <- length(y)
  yhat <- y[(n0+passo):N]
  
  for (t in n0:(N-passo)){
    #separa a janela
    X.janela <- X[(t-n0+1):(t-passo+1),]
    y.janela <- y[(t-n0+1+passo-1):t]
    
    X.out <- X[(t+1),]
    #estima modelo
    mod <- bagging(X.janela,y.janela, pre.testing = "group-joint")
    #preve 1 obs a frente
    yy <- predict(mod,X.out)
    #guarda resultado
    yhat[(t-n0+1)] <- yy
  }
  # RMSE e MAE
  RMSE = sqrt(mean((yhat-y[(n0+passo):N])^2))
  MAE = mean(abs(yhat-y[(n0+passo):N]))
  return(c(RMSE, MAE))
  
}

result1 <- janelas_moveis(X.go,y.go,60,1)
result6 <- janelas_moveis(X.go,y.go,60,6)
result12 <- janelas_moveis(X.go,y.go,60,12)

###### Janelas Moveis Boosting ######

janelas_moveis_boost <- function(X,y,n0,passo){
  N <- length(y)
  yhat <- y[(n0+passo):N]
  
  for (t in n0:(N-passo)){
    #separa a janela
    X.janela <- X[(t-n0+1):(t-passo+1),]
    y.janela <- y[(t-n0+1+passo-1):t]
    
    X.out <- X[(t+1),]
    #estima modelo
    mod <- boosting(X.janela, y.janela)
    #preve obs a frente
    yy <- predict(mod,X.out)
    #guarda resultado
    yhat[(t-n0+1)] <- yy
  }
  # RMSE e MAE
  RMSE = sqrt(mean((yhat-y[(n0+passo):N])^2))
  MAE = mean(abs(yhat-y[(n0+passo):N]))
  return(c(RMSE, MAE))
  
}

result1_boost <- janelas_moveis_boost(X.go,y.go,60,1)
result6_boost <- janelas_moveis_boost(X.go,y.go,60,6)
result12_boost <- janelas_moveis_boost(X.go,y.go,60,12)







