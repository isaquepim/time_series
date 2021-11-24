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
##### Modelagem usando apenas lags passados #####
### Ridge ###
ridge = ic.glmnet(X,y, crit = "bic", alpha = 0)
coef(ridge)
checkresiduals(ridge$residuals)

df.ridge <- data.frame(actual = ts.y, predicted = ridge$fitted.values)
p.ridge <- ggplot(data = df.ridge, aes(x = zoo::index(ts.y)))+ 
  geom_line(aes(y=predicted, color='predicted')) + 
  geom_point(aes(y=actual, color='actual')) + 
  labs(title = "Fitted model Ridge", x = "Ano", y = "Consumo MWh")

p.ridge
### LASSO ###
lasso = ic.glmnet(X,y, crit = "aic")
coef(lasso)
checkresiduals(lasso$residuals)


df.lasso <- data.frame(actual = ts.y, predicted = lasso$fitted.values)
p.lasso <- ggplot(data = df.lasso, aes(x = zoo::index(ts.y)))+ 
  geom_line(aes(y=predicted, color='predicted')) + 
  geom_point(aes(y=actual, color='actual')) + 
  labs(title = "Fitted model LASSO", x = "Ano", y = "Consumo MWh")

p.lasso

### Comparative plot ###

df.comp <- data.frame(actual = ts.y,
                      predicted.lasso = lasso$fitted.values,
                      predicted.ridge = ridge$fitted.values)

p.comp <- ggplot(data = df.comp, aes(x = zoo::index(ts.y)))+ 
  geom_line(aes(y=predicted.ridge, color='ridge')) + 
  geom_line(aes(y=predicted.lasso, color='lasso')) + 
  labs(title = "Fitted model LASSO", x = "Ano", y = "Consumo MWh")
p.comp


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

###### Fit completo ######

ridge.go = ic.glmnet(X.go,y.go, crit = "aic", alpha = 0)
coef(ridge.go)
checkresiduals(ridge.go$residuals)

df.ridge.go <- data.frame(actual = ts.y.go, predicted = ridge.go$fitted.values)
p.ridge.go <- ggplot(data = df.ridge.go, aes(x = zoo::index(ts.y.go)))+ 
  geom_line(aes(y=predicted, color='predicted')) + 
  geom_point(aes(y=actual, color='actual')) + 
  labs(title = "Fitted model Ridge", x = "Ano", y = "Consumo MWh")

p.ridge.go
### LASSO ###
lasso.go = ic.glmnet(X.go,y.go, crit = "aic")
coef(lasso.go)
checkresiduals(lasso.go$residuals)


df.lasso.go <- data.frame(actual = ts.y.go, predicted = lasso.go$fitted.values)
p.lasso.go <- ggplot(data = df.lasso.go, aes(x = zoo::index(ts.y.go)))+ 
  geom_line(aes(y=predicted, color='predicted')) + 
  geom_point(aes(y=actual, color='actual')) + 
  labs(title = "Fitted model LASSO", x = "Ano", y = "Consumo MWh")

p.lasso.go

### Comparative plot ###

df.comp.go <- data.frame(actual = ts.y.go,
                      predicted.lasso = lasso.go$fitted.values,
                      predicted.ridge = ridge.go$fitted.values)

p.comp.go <- ggplot(data = df.comp.go, aes(x = zoo::index(ts.y.go)))+ 
  geom_line(aes(y=predicted.ridge, color='ridge')) + 
  geom_line(aes(y=predicted.lasso, color='lasso')) + 
  labs(title = "Fitted model LASSO", x = "Ano", y = "Consumo MWh")
p.comp
###### CSR #####

csr.go <- csr(X.go, y.go, K = min(20, ncol(X.go)), k = 6)
plot(y.go,type="l")
lines(fitted(csr.go),col=2)


###### Janelas Moveis LASSO ######


janelas_moveis <- function(X,y,n0,passo){
  N <- length(y)
  yhat <- y[(n0+passo):N]

  for (t in n0:(N-passo)){
    #separa a janela
    X.janela <- X[(t-n0+1):(t-passo+1),]
    y.janela <- y[(t-n0+1+passo-1):t]
    
    X.out <- X[(t+1),]
    #estima modelo
    mod <- ic.glmnet(X.janela,y.janela,crit = "aic")
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

###### Janelas Moveis CSR ######

janelas_moveis_CSR <- function(X,y,n0,passo){
  N <- length(y)
  yhat <- y[(n0+passo):N]
  
  for (t in n0:(N-passo)){
    #separa a janela
    X.janela <- X[(t-n0+1):(t-passo+1),]
    y.janela <- y[(t-n0+1+passo-1):t]
    
    X.out <- X[(t+1),]
    #estima modelo
    mod <- csr(X.janela, y.janela, K = 15, k = 5)
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

result1_CSR <- janelas_moveis_CSR(X.go,y.go,60,1)
result6_CSR <- janelas_moveis_CSR(X.go,y.go,60,6)
result12_CSR <- janelas_moveis_CSR(X.go,y.go,60,12)





