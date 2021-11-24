library(forecast)
library(fpp2)
library(ggplot2)
library(zoo)

# Obter dados ES do arquivo extract.R

###### análise inicial ######
plot(ES$Total)
lambda <- BoxCox.lambda(ES$Total)
plot(BoxCox(ES$Total, lambda = lambda))


total <- ES$Total
N <- length(total)

mod1 <- ets(total, 'AAA')

summary(mod1)
checkresiduals(mod1)

mod2 <- ets(total, 'ZZZ')

summary(mod2)
checkresiduals(mod2)


data.frame(y = total, yhat = fitted(mod1)) %>% 
  ggplot(aes(x=zoo::index(total))) + 
  geom_line(aes(y=y.1, color='predicted')) + 
  geom_point(aes(y=y, color='actual')) +
  labs(title = "Predicted vs forecasted - dentro da amostra", 
       x = "Ano", 
       y = "Consumo Eletrico")

data.frame(y = total, yhat = fitted(mod1)) %>% 
  ggplot(aes(x=y.1, y=y))+
  geom_point(color='red')+ 
  geom_abline(slope = 1, intercept = 0)


data.frame(y = diff(total), yhat = diff(fitted(mod1))) %>% 
  ggplot(aes(x=y.1, y=y))+
  geom_point(color='red')+ 
  geom_abline(slope = 1, intercept = 0)

###### calculando erros para ets ######
erros <- array(dim=c(6,7,2))
i <- 1
for(serie in ES){
  
  lambda <- BoxCox.lambda(serie)
  erros[i,1,1:2] <- janela_fixa(N=length(serie),n0 = 60, serie = serie, lambda = lambda)
  
  erros[i,2,1:2] <- janelas_crescentes(N=length(serie),n0=60,serie = serie,
                       passo = 1, lambda = lambda)
  erros[i,3,1:2] <- janelas_crescentes(N=length(serie),n0=60,serie = serie,
                                  passo = 6, lambda = lambda)
  erros[i,4,1:2] <- janelas_crescentes(N=length(serie),n0=60,serie = serie,
                                  passo = 12, lambda = lambda)
  erros[i,5,1:2] <- janelas_moveis(N=length(serie),n0=60,serie = serie,
                   passo = 1, lambda = lambda)
  erros[i,6,1:2] <- janelas_moveis(N=length(serie),n0=60,serie = serie,
                           passo = 6, lambda = lambda)
  erros[i,7,1:2] <- janelas_moveis(N=length(serie),n0=60,serie = serie,
                           passo = 12, lambda = lambda)
  
  i <- i+1
}
###### erros para sarima ######
erros_sarima <- array(dim=c(6,7,2))
i <- 1
for(serie in ES){
  
  lambda <- 0
  erros_sarima[i,1,1:2] <- janela_fixa_arima(N=length(serie),n0 = 60, serie = serie,
                                      lambda = lambda)
  
  erros_sarima[i,2,1:2] <- janelas_crescentes_arima(N=length(serie),n0=60,serie = serie,
                                       passo = 1, lambda = lambda)
  erros_sarima[i,3,1:2] <- janelas_crescentes_arima(N=length(serie),n0=60,serie = serie,
                                       passo = 6, lambda = lambda)
  erros_sarima[i,4,1:2] <- janelas_crescentes_arima(N=length(serie),n0=60,serie = serie,
                                       passo = 12, lambda = lambda)
  erros_sarima[i,5,1:2] <- janelas_moveis_arima(N=length(serie),n0=60,serie = serie,
                                   passo = 1, lambda = lambda)
  erros_sarima[i,6,1:2] <- janelas_moveis_arima(N=length(serie),n0=60,serie = serie,
                                   passo = 6, lambda = lambda)
  erros_sarima[i,7,1:2] <- janelas_moveis_arima(N=length(serie),n0=60,serie = serie,
                                   passo = 12, lambda = lambda)
  
  i <- i+1
}



