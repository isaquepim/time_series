#Começando o modelo do item A
#Fazer uma análise geral das estatísticas básicas da série e propor modelo
#Tenha a variável ES em mãos do arquivo extract.R



total <- log(ES$Total)
teste <- ts(total[1:180], frequency = 12)
treino <- ts(total[181:210], frequency = 12)
plot(teste, type= 'l')
plot(decompose(teste))
dummies <- factor(cycle(teste))

inter1 <- numeric(length(teste))
inter2 <- numeric(length(teste))

inter1[59:66] <- 1
inter2[133:164] <- 1

t <- c(1:length(teste))

df.teste <- data.frame(teste = teste,t,dummies,inter1,inter2)

fit.trend <- lm(teste ~ t + dummies + inter1 + t*inter2, data = df.teste)
summary(fit.trend)
plot(fit.trend$residuals, type = 'l')

ggplot(df.teste, aes(x = t, y = teste)) + 
  geom_line(alpha=0.7) +                           # observed data
  geom_line(aes(x = t, y = fit.trend$fitted.values),  # predicted data
             color='red', alpha=0.5)



#Criando intervenções

df.now <- data.frame(t,dummies, teste)
plot(df.now$teste)

df.now[['crise1']] <- numeric(180)
df.now[['crise2']] <- numeric(180)
df.now[['hoje']] <- numeric(180)
df.now[['past']] <- numeric(180)

df.now[59:66,'crise1']   <- 1
df.now[133:160,'crise2'] <- 1
df.now[161:180,'hoje']   <- 1
df.now[1:58, 'past']     <- 1

df.now$diff <- df.now$t - 160
df.now$diff2 <- df.now$t - 132

df.now$X0 <- df.now$t * df.now$past
df.now$X <- df.now$diff * df.now$hoje
df.now$X2 <- df.now$diff2 * df.now$crise2


fit.model <- lm(teste ~t + X0 + X2 + crise1, data = df.now)
summary(fit.model)

ggplot(df.now, aes(x = t, y = teste)) + 
  geom_line(alpha=0.7) +                           # observed data
  geom_line(aes(x = t, y = fit.model$fitted.values),  # predicted data
            color='red', alpha=0.5)


plot(fit.model$residuals, type = 'l')
checkresiduals(fit.model$residuals)
jarque.bera.test(fit.model$residuals)
Box.test(fit.model$residuals)


