##-- instalação / instanciação de libraries


library(readr)
library(ggplot2)
library(GGally)
library(PerformanceAnalytics)



##-- Seta o diretorio de trabalho
setwd('E:/Mauro/CVs/testes/semantix/workarea')

##-- limpa o ambiente
rm(list=ls())

##-- carrega dados de treino e teste

ds_bank_full <- read_delim("bank-additional-full.csv", ";", escape_double = FALSE, trim_ws = TRUE)

##-- análise exploratória dos dados


##-- avaliação geral
str(ds_bank_full)
summary(ds_bank_full, na.rm=FALSE)

##-- avaliação de cada variável (distribuição, missing values, etc)

cbind(colnames(ds_bank_full))

##------ avaliacao {age}  ------------------------------------------

cbind(table(ds_bank_full$age))

hist(ds_bank_full$age, , xlab = 'Age', main = 'Distribuição da variável AGE')

boxplot(ds_bank_full$age, main = 'Boxplot AGE')


##------ avaliacao {job}  ------------------------------------------

cbind(table(ds_bank_full$job))


##------ avaliacao {marital}  ---------------------------------------


cbind(table(ds_bank_full$marital))


##------ avaliacao {education}  -------------------------------------


cbind(table(ds_bank_full$education))

##------ avaliacao {default}  ---------------------------------------

cbind(table(ds_bank_full$default))

##------ avaliacao {housing}  ---------------------------------------

cbind(table(ds_bank_full$housing))


##------ avaliacao {loan}  ------------------------------------------

cbind(table(ds_bank_full$loan))


##------ avaliacao {contact}  ---------------------------------------

cbind(table(ds_bank_full$contact))


##------ avaliacao {month}  -----------------------------------------


cbind(table(ds_bank_full$month))

##------ avaliacao {day_of_week}  -----------------------------------

cbind(table(ds_bank_full$day_of_week))


##------ avaliacao {duration}  --------------------------------------

summary(ds_bank_full$duration)

hist(ds_bank_full$duration)

hist(log(ds_bank_full$duration))

boxplot(ds_bank_full$duration)

boxplot(log(ds_bank_full$duration))


##------ avaliacao {campaign}  --------------------------------------

summary(ds_bank_full$campaign)

par(mfrow=c(1,2))

hist(ds_bank_full$campaign)

hist(log(ds_bank_full$campaign))

boxplot(ds_bank_full$campaign)

boxplot(log(ds_bank_full$campaign))

par(mfrow=c(1,1))


##------ avaliacao {pdays}  -----------------------------------------


summary(ds_bank_full$pdays)

hist(ds_bank_full$pdays)

boxplot(ds_bank_full$pdays)

cbind(table(ds_bank_full$pdays))

##------ avaliacao {previous}  --------------------------------------

summary(ds_bank_full$previous)

par(mfrow=c(2,1))

hist(ds_bank_full$previous)

par(mfrow=c(1,1))

boxplot(ds_bank_full$previous)

cbind(table(ds_bank_full$previous))


##------ avaliacao {poutcome}  --------------------------------------

cbind(table(ds_bank_full$poutcome))


##------ avaliacao {emp.var.rate}  ----------------------------------

summary(ds_bank_full$emp.var.rate)

hist(ds_bank_full$emp.var.rate)

boxplot(ds_bank_full$emp.var.rate)

cbind(table(ds_bank_full$emp.var.rate))


##------ avaliacao {cons.price.idx}  --------------------------------

summary(ds_bank_full$cons.price.idx)

hist(ds_bank_full$cons.price.idx)

boxplot(ds_bank_full$cons.price.idx)

cbind(table(ds_bank_full$cons.price.idx))


##------ avaliacao {cons.conf.idx}  ---------------------------------

summary(ds_bank_full$cons.conf.idx)

hist(ds_bank_full$cons.conf.idx)

boxplot(ds_bank_full$cons.conf.idx)

cbind(table(ds_bank_full$cons.conf.idx))


##------ avaliacao {euribor3m}  -------------------------------------

summary(ds_bank_full$euribor3m)

hist(ds_bank_full$euribor3m)

boxplot(ds_bank_full$euribor3m)

cbind(table(ds_bank_full$euribor3m))


##------ avaliacao {nr.employed}  -----------------------------------

summary(ds_bank_full$nr.employed)

hist(ds_bank_full$nr.employed)

boxplot(ds_bank_full$nr.employed)

cbind(table(ds_bank_full$nr.employed))


##----- matrix de correlação das variáveis numericas


ds_num_bank <- ds_bank_full[c(1, 11, 12, 13, 14, 16, 17, 18, 19, 20)]

ggpairs(ds_num_bank)

chart.Correlation(ds_num_bank, histogram=TRUE, pch=17)

##----- decision trees para remover os campos unknow




#################################################################################################################



