library(rpart)
library(rpart.plot)
library(dplyr)


##-- novo dataset para se trabalhar
ds_bank_full_acuracidade <- ds_bank_full

##-- coloca row ids
ds_bank_full_acuracidade$ID <- seq.int(nrow(ds_bank_full_acuracidade))

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[c(22,1:21)]

###-- View(ds_bank_full_acuracidade)

cbind(colnames(ds_bank_full_acuracidade))





## predizendo Estado Civil unknow

cbind(table(ds_bank_full_acuracidade$marital))

ss_marital_train <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$marital != 'unknown'))
ss_marital_test  <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$marital == 'unknown'))

modelo_rpart_marital <- rpart(marital ~ age + job + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed + y, data = ss_marital_train)

rpart.plot(modelo_rpart_marital)

summary(modelo_rpart_marital)

pred_marital <- predict(modelo_rpart_marital, ss_marital_test[c(2,3,5:22)], type="class")

summary(pred_marital)

ss_marital_test$marital <- as.character(pred_marital)

ds_bank_full_acuracidade <- union(ss_marital_train, ss_marital_test)

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[order(ds_bank_full_acuracidade$ID),]

nrow(ds_bank_full_acuracidade)
unique(ds_bank_full_acuracidade$marital)








## predizendo Housing unknow

cbind(table(ds_bank_full_acuracidade$housing))

ss_housing_train <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$housing != 'unknown'))
ss_housing_test  <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$housing == 'unknown'))

##-- o dataset de teste não tinha todos os fatores
ss_housing_test$loan <- factor(ss_housing_test$loan, levels = c("no", "yes" , "unknow"))

cbind(table(ss_housing_test$loan))

modelo_rpart_housing <- rpart(housing ~ age + job + marital + education + default + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed + y, data = ss_housing_train)

rpart.plot(modelo_rpart_housing)


summary(modelo_rpart_housing)



pred_housing <- predict(modelo_rpart_housing, ss_housing_test[c(2:6,8:22)], type="class")

summary(pred_housing)

ss_housing_test$housing <- as.character(pred_housing)

ds_bank_full_acuracidade <- union(ss_housing_train, ss_housing_test)

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[order(ds_bank_full_acuracidade$ID),]

nrow(ds_bank_full_acuracidade)
unique(ds_bank_full_acuracidade$housing)

##-- neste processo os loan = unknow viraram NA - recolocando os valores originais
ds_bank_full_acuracidade$loan[is.na(ds_bank_full_acuracidade$loan)] <- "unknown"
                                                                       











## predizendo loan unknown

cbind(table(ds_bank_full_acuracidade$loan))

ss_loan_train <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$loan != 'unknown'))
ss_loan_test  <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$loan == 'unknown'))




##-- o dataset de teste não tinha todos os fatores
##ss_loan_test$loan <- factor(ss_loan_test$loan, levels = c("no", "yes" , "unknow"))

cbind(table(ss_loan_test$loan))

modelo_rpart_loan <- rpart(loan ~ age + job + marital + education + default + housing + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed + y, data = ss_loan_train)

rpart.plot(modelo_rpart_loan)


summary(modelo_rpart_loan)



pred_loan <- predict(modelo_rpart_loan, ss_loan_test[c(2:7,9:22)], type="class")

summary(pred_loan)

ss_loan_test$loan <- as.character(pred_loan)

ds_bank_full_acuracidade <- union(ss_loan_train, ss_loan_test)

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[order(ds_bank_full_acuracidade$ID),]

nrow(ds_bank_full_acuracidade)
unique(ds_bank_full_acuracidade$loan)





























## predizendo JOB unknow

cbind(table(ds_bank_full_acuracidade$job))

cbind(colnames(ss_job_train))

ss_job_train <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$job != 'unknown'))
ss_job_test  <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$job == 'unknown'))

modelo_rpart_job <- rpart(job ~ age + marital + education + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed + y, data = ss_job_train)

rpart.plot(modelo_rpart_job)


summary(modelo_rpart_job)


pred_job <- predict(modelo_rpart_job, ss_job_test[c(2,4:22)], type="class")

summary(pred_job)

ss_job_test$job <- as.character(pred_job)

ds_bank_full_acuracidade <- union(ss_job_train, ss_job_test)

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[order(ds_bank_full_acuracidade$ID),]

nrow(ds_bank_full_acuracidade)
unique(ds_bank_full_acuracidade$job)




## predizendo education unknow

cbind(table(ds_bank_full_acuracidade$education))


ss_education_train <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$education != 'unknown'))
ss_education_test  <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$education == 'unknown'))

modelo_rpart_education <- rpart(education ~ age + job + marital + default + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed + y, data = ss_education_train)

rpart.plot(modelo_rpart_education)

summary(modelo_rpart_education)

nrow(ss_education_test)

pred_education <- predict(modelo_rpart_education, ss_education_test[c(2:4, 6:22)], type="class")


ss_education_test$education <- as.character(pred_education)


ds_bank_full_acuracidade <- union(ss_education_train, ss_education_test)

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[order(ds_bank_full_acuracidade$ID),]

nrow(ds_bank_full_acuracidade)
unique(ds_bank_full_acuracidade$education)

cbind(table(ds_bank_full_acuracidade$education))




## predizendo default unknow

cbind(table(ds_bank_full_acuracidade$default))


ss_default_train <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$default != 'unknown'))
ss_default_test  <- with(ds_bank_full_acuracidade, subset(ds_bank_full_acuracidade, ds_bank_full_acuracidade$default == 'unknown'))

modelo_rpart_default <- rpart(default ~ age + job + marital + education + housing + loan + contact + month + day_of_week + duration + campaign + pdays + previous + poutcome + emp.var.rate + cons.price.idx + cons.conf.idx + euribor3m + nr.employed + y, data = ss_default_train)

rpart.plot(modelo_rpart_default)

summary(modelo_rpart_default)

nrow(ss_default_test)

pred_default <- predict(modelo_rpart_default, ss_default_test[c(2:5, 7:22)], type="class")


ss_default_test$default <- as.character(pred_default)


ds_bank_full_acuracidade <- union(ss_default_train, ss_default_test)

ds_bank_full_acuracidade <- ds_bank_full_acuracidade[order(ds_bank_full_acuracidade$ID),]

nrow(ds_bank_full_acuracidade)

unique(ds_bank_full_acuracidade$default)

cbind(table(ds_bank_full_acuracidade$default))


##------------------------------------------------------------------------

cbind(table(ds_bank_full_acuracidade$marital))
cbind(table(ds_bank_full_acuracidade$housing))
cbind(table(ds_bank_full_acuracidade$loan))
cbind(table(ds_bank_full_acuracidade$job))
cbind(table(ds_bank_full_acuracidade$education))
cbind(table(ds_bank_full_acuracidade$default))


save(ds_bank_full_acuracidade, file = "ds_bank_full_acuracidade.RData")


load("ds_bank_full_acuracidade.RData")
