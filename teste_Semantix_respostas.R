library(dplyr)
library(e1071)
library(plyr)

### -- Quest�o 1

##-- Extrai um subset do data frame tratado
subset_bank <- ds_bank_full_acuracidade[c(3,7,8)]

##-- gera o modelo a partir do algoritmo Nayve Bayes
naive_bayes_loan <- naiveBayes(job ~., data=subset_bank)

### -- Quest�o 2

##-- Extrai um subset do data frame tratado
subset_campaign <- ds_bank_full_acuracidade[c(15,16)]

##-- gera o modelo a partir do algoritmo Nayve Bayes
naive_bayes_cmp <- naiveBayes(previous ~., data=subset_campaign)

##-- Converte o modelo dm data frame
ds_naive_bayes_cmp <- data.frame(naive_bayes_cmp$tables)

##-- subset que mant�m dados para a an�lise
ds_naive_bayes_cmp <- with(ds_naive_bayes_cmp, subset(ds_naive_bayes_cmp, ds_naive_bayes_cmp$poutcome.poutcome == 'success'))

##-- extrai os valores relevantes a serem comparados e converte para num�rico
ds_naive_bayes_cmp$poutcome.Y <- as.numeric(ds_naive_bayes_cmp$poutcome.Y)
ds_naive_bayes_cmp$poutcome.Freq <- as.numeric(ds_naive_bayes_cmp$poutcome.Freq)

##-- extrai a correla��o das duas vari�veis
cor(ds_naive_bayes_cmp$poutcome.Y, ds_naive_bayes_cmp$poutcome.Freq)

##-- determina o coeficente de crescimento atrav�s de uma regress�o linear simples
taxa_crescimento <- lm(ds_naive_bayes_cmp$poutcome.Freq ~ ds_naive_bayes_cmp$poutcome.Y, data=ds_naive_bayes_cmp)

##-- exibe os detalhes da regress�o
summary(taxa_crscimento)

##-- exibe graficamente os resultados da regress�o
plot(taxa_crscimento)


### -- Quest�o 3

##-- avalia�o detalhada das medidas de centralidade e dispers�o b�sicas
mean(ds_naive_bayes_cmp$poutcome.Freq)
sd(ds_naive_bayes_cmp$poutcome.Freq)

##-- exibe o gr�fico prim�rio da rela��o entre as vari�veis
plot(ds_naive_bayes_cmp$poutcome.Y, ds_naive_bayes_cmp$poutcome.Freq, type='l')

### -- Quest�o 4

##-- Extrai um subset do data frame tratado
ds_bank_result_campaign <- ds_bank_full_acuracidade[c(16,22)]

##-- Separa os dados da campanha anterior em 2 datasets distintos
no_result <- with(ds_bank_result_campaign, subset(ds_bank_result_campaign, ds_bank_result_campaign$y == 'no'))
yes_result <- with(ds_bank_result_campaign, subset(ds_bank_result_campaign, ds_bank_result_campaign$y == 'yes'))

##-- conta as frequ�ncias
freq_no <- count(no_result, "poutcome")
freq_yes <- count(yes_result, "poutcome")

##-- realiza a correla��o entre frequ�ncias
cor(freq_no$freq, freq_yes$freq)


### -- Quest�o 6

##-- exibe a contagem de cada ocorr�ncia
cbind(table(ds_bank_full_acuracidade$housing))

##-- subset dos dados relevantes � an�lise
client_housing <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$housing == 'yes'))

##-- caracter�sticas proeminentes de quem tem empr�stimo por hipoteca
mean(client_housing$age)

cbind(table(client_housing$loan))
cbind(table(client_housing$default))
cbind(table(client_housing$marital))
cbind(table(client_housing$education))
cbind(table(client_housing$housing))
