total <- log(ES$Total)
treino <- ts(total[1:180], frequency = 12)
teste <- ts(total[181:210], frequency = 12)


#--------FITTING--------

plot(treino, type= 'l')
plot(decompose(treino))
dummies <- factor(cycle(treino))


inter1 <- numeric(length(treino))
inter2 <- numeric(length(treino))
inter3 <- numeric(length(treino))

inter1[59:66] <- 1
inter2[133:180] <- 1
inter3[160:180] <- 1

t <- c(1:length(treino))

df_treino <- data.frame(treino, t, dummies, inter1, inter2, inter3)

df_treino$t2 <- df_treino$t -133
df_treino$t3 <- df_treino$t -160

df_treino$X2 <- df_treino$t2 * df_treino$inter2
df_treino$X3 <- df_treino$t3 * df_treino$inter3


fit.reg <- lm(formula = treino ~ t + X2 + X3 + inter1, data = df_treino)
summary(fit.reg)

ggplot(df_treino, aes(x = t, y = treino)) + 
  geom_line(alpha=0.7) +                           # observed data
  geom_line(aes(x = t, y = fit.reg$fitted.values),  # fitted data
            color='red', alpha=0.5)


detrend <- treino - fit.reg$fitted.values
plot(detrend, type = 'l')

par(mfrow=c(1,2))
acf(detrend, lag.max = 25)
pacf(detrend, lag.max = 25)

fit.model <- lm(formula = treino ~ t + X2 + X3 + inter1 + dummies, data = df_treino)
summary(fit.model)

ggplot(df_treino, aes(x = t, y = treino)) + 
  geom_line(alpha=0.7) +                           # observed data
  geom_line(aes(x = t, y = fit.model$fitted.values),  # fitted data
            color='red', alpha=0.5)

par(mfrow=c(1,2))
acf(fit.model$residuals, lag.max = 25)
pacf(fit.model$residuals, lag.max = 25)


fit.res <- Arima(fit.model$residuals, order = c(1,0,2), include.mean = FALSE)
summary(fit.res)
checkresiduals(fit.res)
jarque.bera.test(fit.res$residuals)
pacf(fit.res$residuals, lag.max = 25)



#----------PREDICTION-------------
set.seed(2000)

new_t <- c(181:210)
new_inter1 <- numeric(length(new_t))

new_X2 <- new_t - 133
new_X3 <- new_t - 160

new_dummies <- dummies[1:length(new_t)]

newdata <- data.frame(t = new_t, inter1 = new_inter1, X2 = new_X2,
                      X3 = new_X3, dummies = new_dummies)

err <- arima.sim(list(ar=fit.res$coef["ar1"],ma=c(fit.res$coef["ma1"],fit.res$coef["ma2"])),
                 sd=fit.res$sigma,length(new_t))

pred <- ts(predict(fit.model, newdata =  newdata) + err, frequency = 12)
plot(pred, type = 'l')
conf <- predict(fit.model, newdata = newdata, interval = 'confidence')
mse <- mean((pred - teste)^2)



df_teste <- data.frame(t = new_t, fit = pred, teste = teste)
df_teste2 <-  data.frame(t = new_t, fit = conf[,1], lwr = conf[,2], upr = conf[,3] , teste = teste)
p <- ggplot() + geom_line(data = df_treino, aes(x = t, y = treino))
p <- p + geom_line(data = df_teste, aes(x = t, y = teste))
p <- p + geom_line(data = df_teste, aes(x = t, y = fit), colour = 'red')
p <- p + geom_line(data = df_teste2, aes(x = t, y = lwr),colour = 'blue')
p <- p + geom_line(data = df_teste2, aes(x = t, y = upr),colour = 'blue')
p








