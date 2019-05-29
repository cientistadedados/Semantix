library(dplyr)
library(e1071)
library(plyr)

### -- Questão 1

##-- Extrai um subset do data frame tratado
subset_bank <- ds_bank_full_acuracidade[c(3,7,8)]

##-- gera o modelo a partir do algoritmo Nayve Bayes
naive_bayes_loan <- naiveBayes(job ~., data=subset_bank)

### -- Questão 2

##-- Extrai um subset do data frame tratado
subset_campaign <- ds_bank_full_acuracidade[c(15,16)]

##-- gera o modelo a partir do algoritmo Nayve Bayes
naive_bayes_cmp <- naiveBayes(previous ~., data=subset_campaign)

##-- Converte o modelo dm data frame
ds_naive_bayes_cmp <- data.frame(naive_bayes_cmp$tables)

##-- subset que mantém dados para a análise
ds_naive_bayes_cmp <- with(ds_naive_bayes_cmp, subset(ds_naive_bayes_cmp, ds_naive_bayes_cmp$poutcome.poutcome == 'success'))

##-- extrai os valores relevantes a serem comparados e converte para numérico
ds_naive_bayes_cmp$poutcome.Y <- as.numeric(ds_naive_bayes_cmp$poutcome.Y)
ds_naive_bayes_cmp$poutcome.Freq <- as.numeric(ds_naive_bayes_cmp$poutcome.Freq)

##-- extrai a correlação das duas variáveis
cor(ds_naive_bayes_cmp$poutcome.Y, ds_naive_bayes_cmp$poutcome.Freq)

##-- determina o coeficente de crescimento através de uma regressão linear simples
taxa_crescimento <- lm(ds_naive_bayes_cmp$poutcome.Freq ~ ds_naive_bayes_cmp$poutcome.Y, data=ds_naive_bayes_cmp)

##-- exibe os detalhes da regressão
summary(taxa_crscimento)

##-- exibe graficamente os resultados da regressão
plot(taxa_crscimento)


### -- Questão 3

##-- avaliaão detalhada das medidas de centralidade e dispersão básicas
mean(ds_naive_bayes_cmp$poutcome.Freq)
sd(ds_naive_bayes_cmp$poutcome.Freq)

##-- exibe o gráfico primário da relação entre as variáveis
plot(ds_naive_bayes_cmp$poutcome.Y, ds_naive_bayes_cmp$poutcome.Freq, type='l')

### -- Questão 4

##-- Extrai um subset do data frame tratado
ds_bank_result_campaign <- ds_bank_full_acuracidade[c(16,22)]

##-- Separa os dados da campanha anterior em 2 datasets distintos
no_result <- with(ds_bank_result_campaign, subset(ds_bank_result_campaign, ds_bank_result_campaign$y == 'no'))
yes_result <- with(ds_bank_result_campaign, subset(ds_bank_result_campaign, ds_bank_result_campaign$y == 'yes'))

##-- conta as frequências
freq_no <- count(no_result, "poutcome")
freq_yes <- count(yes_result, "poutcome")

##-- realiza a correlação entre frequências
cor(freq_no$freq, freq_yes$freq)


### -- Questão 6

##-- exibe a contagem de cada ocorrência
cbind(table(ds_bank_full_acuracidade$housing))

##-- subset dos dados relevantes à análise
client_housing <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$housing == 'yes'))

##-- características proeminentes de quem tem empréstimo por hipoteca
mean(client_housing$age)

cbind(table(client_housing$loan))
cbind(table(client_housing$default))
cbind(table(client_housing$marital))
cbind(table(client_housing$education))
cbind(table(client_housing$housing))
