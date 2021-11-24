
#--------residencial---------
casa <- ES$Residencial
lcasa <- log(casa)
dcasa <- diff(lcasa)
ddcasa <- diff(dcasa, lag = 12)
plot(lcasa)
plot(dcasa)

par(mfrow=c(1,2))

acf(dcasa)
pacf(dcasa)

par(mfrow=c(1,2))

acf(ddcasa, lag.max = 25)
pacf(ddcasa, lag.max = 25)

pandemia <- numeric(210)
pandemia[195:210] <- 1

modelo <- Arima(lcasa, order = c(2,1,1), seasonal = c(1,1,1), xreg = pandemia)
summary(modelo)
checkresiduals(modelo)

modelo2 <- auto.arima(lcasa, stepwise = FALSE, trace = TRUE, xreg = pandemia)
summary(modelo2)
checkresiduals(modelo2)
pacf(modelo2$residuals, lag.max = 25)

modelo3 <- Arima(lcasa, order = c(0,1,1), seasonal = c(1,1,1), xreg = pandemia)
summary(modelo3)
checkresiduals(modelo3)
pacf(modelo3$residuals, lag.max = 25)


modelo$coef[4]
modelo2$coef[3]
modelo3$coef[4]




#-------comercial-------

comercial <- ES$Comercial
b.comercial <- BoxCox(comercial,0)

db.comercial <- diff(b.comercial)
plot(db.comercial)
ddb.comercial <- diff(db.comercial, lag = 12)


par(mfrow=c(1,2))
acf(ddb.comercial,lag.max = 24)
pacf(ddb.comercial,lag.max = 24)

model.c1 <- Arima(b.comercial, order = c(1,1,1), seasonal = c(1,1,1),
                  xreg = pandemia)

model.c2 <- auto.arima(b.comercial,xreg = pandemia)
summary(model.c2)
par(mfrow=c(1,1))

#---------cativ0-----------
cativo <- ts(ES$Cativo[73:210], frequency = 12)
lcativo <- log(cativo)
plot(lcativo)

model.ca1 <- auto.arima(lcativo, xreg = pandemia[73:210])
summary(model.ca1)
#----------outros----------


outros <- log(ES$Outros)

model.o <- auto.arima(outros, xreg = pandemia)
summary(model.o)

#------------industrial--------


industrial <- ts(log(ES$Industrial[97:210]),frequency = 12)
crash <- numeric(length(industrial))
crash[47] <- 1
xreg <- matrix(ncol = 2,nrow = length(industrial))
xreg[,1] <- crash
xreg[,2] <- pandemia[97:210]

par(mfrow=c(1,2))
acf(diff(industrial),lag.max = 24)
pacf(diff(industrial),lag.max = 24)



model.i <- Arima(industrial, order = c(0,1,1), seasonal = c(1,0,1) , xreg = xreg)
summary(model.i)

model.i2 <- auto.arima(industrial, xreg = xreg)
summary(model.i2)

