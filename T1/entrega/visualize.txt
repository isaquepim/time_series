#Tenha a variavel 'ES' em mãos do script extract.R


#Primeiro plot: visualização conjunta das séries para entender possiveis
#correlacoes visualmente

#Passando a ts para df para melhor sincronia com o ggplot
dfES <- data.frame(ES)
dfES[['date']] = seq(as.Date("2004-01-01"), as.Date("2021-06-01"), "1 month")
summary(dfES)
colunas <- c("Industrial","Comercial","Cativo","Residencial", "Outros","Total")
dfES$date = as.yearmon(dfES$date)


meltedES <- melt(dfES, id = 'date')
p <- ggplot(meltedES, aes(x = date, y = value, colour = variable)) + geom_line()
p <- p + labs(x = 'Ano/mês', y = 'Consumo MWh', colour = 'Categoria')
p

log.dfES <- dfES
log.dfES[colunas] <- log(log.dfES[colunas])

meltedES.log <- melt(log.dfES, id = 'date')
p.log <- ggplot(meltedES.log, aes(x = date, y = value, colour = variable)) + geom_line()
p.log <- p.log + labs(x = 'Ano/mês', y = 'Consumo log MWh', colour = 'Categoria')
p.log

#ITEM A , APENAS O CONSUMO TOTAL.

total.log <- log(ES$Total)
dfTOTAL <- data.frame(total.log)
dfTOTAL[['date']] = seq(as.Date("2004-01-01"), as.Date("2021-06-01"), "1 month")
dfTOTAL$date = as.yearmon(dfTOTAL$date)
meltedTOTAL <- melt(dfTOTAL, id = 'date')

p.total <- ggplot(meltedTOTAL, aes(x = date, y = value)) + geom_line()
p.total <- p.total + labs(x = 'Ano/mês', y = 'Consumo log MWh')
p.total <- p.total + geom_vline(aes(xintercept=as.yearmon(as.Date('2008-11-01')), col="red"), show.legend = F)
p.total <- p.total + geom_vline(aes(xintercept=as.yearmon(as.Date('2009-07-01')), col="red"), show.legend = F)
p.total <- p.total + geom_vline(aes(xintercept=as.yearmon(as.Date('2015-01-01')), col="red"), show.legend = F)
p.total <- p.total + geom_vline(aes(xintercept=as.yearmon(as.Date('2017-01-01')), col="red"), show.legend = F)
p.total



#Separar pré-crise de 2009, pós-crise treino e pós-crise teste

dfTOTAL[['status']] <- numeric(210)
dfTOTAL[67:180,'status'] <- 'teste'
dfTOTAL[181:210,'status'] <- 'treino'
dfTOTAL[1:66,'status'] <- 'rejeitado'

meltedStatus <- melt(dfTOTAL, id = 'date')
p.status <- ggplot(dfTOTAL, aes(x = date, y = total.log, colour = status)) + geom_line()
p.status <- p.status + labs(x = 'Ano/mês', y = 'Consumo log MWh')
p.status






























