#SARIMA model
#Precisamos da intervenção em 2009
#------------total------------
total <- ES$Total
treino <- ts(log(total[1:180]), frequency = 12)
teste <- ts(log(total[181:210]), frequency = 12)
xreg <- numeric(180)
xreg[59:66] <- 1
xreg_in <- ts(xreg, frequency = 12)
xreg_out <- ts(numeric(length(teste)), frequency = 12)


par(mfrow=c(1,2))
acf(treino)
pacf(treino)

par(mfrow=c(1,2))
acf(diff(treino))
pacf(diff(treino))


model1 <- auto.arima(treino, stepwise = FALSE, trace = TRUE, xreg = xreg_in)
summary(model1)
checkresiduals(model1)
jarque.bera.test(model1$residuals)
pacf(model1$residuals, lag.max = 24)
acf(model1$residuals, lag.max = 24)



f <- forecast::forecast(model1, xreg = xreg_out)
autoplot(f)
autoplot(model1$fitted)

#------------Sem o buraco------------

nohole <- ts(log(total[67:180]), frequency = 12)
nohole.t <- ts(log(total[181:210]), frequency = 12)
autoplot(nohole)

par(mfrow=c(1,2))
acf(nohole)
pacf(nohole)

par(mfrow=c(1,2))
acf(diff(nohole), lag.max = 24)
pacf(diff(nohole), lag.max = 24)


model2 <- Arima(nohole, order = c(0,1,1), seasonal = c(0,1,1))
summary(model2)
checkresiduals(model2)
jarque.bera.test(model2$residuals)
pacf(model2$residuals, lag.max = 24)
acf(model2$residuals, lag.max = 24)

f2 <- forecast::forecast(model2, h = 30)
autoplot(f2)
autoplot(model2$fitted)


model3 <- Arima(nohole, order = c(0,1,1), seasonal = c(0,1,2))
summary(model3)
f3 <- forecast::forecast(model3, h = 30)
autoplot(f3)

#---------MSE---------
mean((model1$fitted - teste)^2)
mean((model2$fitted - teste)^2)
mean((model3$fitted - teste)^2)
#----------ploting--------

t = c(1:length(nohole))
t_pos = c(115: 144)

df_pre = data.frame(t = t, treino = nohole)
df_pos = data.frame(t = t_pos, teste = teste, fit = f2$mean)

p <- ggplot() + geom_line(data = df_pre, aes(x = t, y = treino))
p <- p + geom_line(data = df_pos, aes(x = t, y = teste))
p <- p + geom_line(data = df_pos, aes(x = t, y = fit), colour = 'red')
p






