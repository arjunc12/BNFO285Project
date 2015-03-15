library(data.table)
library(party)
library(ada)
library(kernlab)

data0 <- read.csv("trainingData-release.csv")
data <- data0
data$Patient_id <- NULL
na.omit(data)

indices <- 1:nrow(data)
trainIndex <- sample(indices, 2*trunc(length(indices)/3))
train <- data[trainIndex,]
train1 <- train[!is.na(train['Remission_Duration']),]
train2 <- train[!is.na(train['Relapse']),]

test <- data[-trainIndex,]
test1 <- test[!is.na(test['Remission_Duration']),]
test2 <- test[!is.na(test['Relapse']),]

survival = train[,!(colnames(train) %in% c("resp.simple", 
                                           "Remission_Duration",
                                           "vital.status",
                                           "Relapse")),
                drop=F]

response = train[,!(colnames(train) %in% c("Overall_Survival", 
                                           "Remission_Duration",
                                        "vital.status",
                                        "Relapse")),
               drop=F]

remission = train1[,!(colnames(train) %in% c("resp.simple", 
                                       "Overall_Survival",
                                       "vital.status",
                                       "Relapse")),
              drop=F]

vitals = train[,!(colnames(train) %in% c("Overall_Survival", 
                                         "Remission_Duration",
                                         "resp.simple",
                                         "Relapse")),
               drop=F]

relapse = train2[,!(colnames(train) %in% c("resp.simple", 
                                           "Remission_Duration",
                                           "vital.status",
                                           "Overall_Survival")),
                 drop=F]

r_square <- function(y, yhat)
{
  ybar = mean(y)
  sstot = sum((y - ybar) ^ 2)
  ssreg = sum((yhat - ybar) ^ 2)
  return(ssreg / sstot)
}

error_rate <- function(y, yhat)
{
  n = length(y)
  errors = sum(y != yhat)
  return(errors / n)
}

bac <- function(y, yhat, pos=1)
{
  p = sum(y == pos)
  n = sum(y != pos)
  tp = sum(yhat == pos & yhat == y)
  tn = sum(yhat != pos & yhat == y)
  return(0.5*(tp/p + tn/n))
}

alpha = 0.75
survival_fit <- ctree(Overall_Survival ~ ., survival, 
                      controls = ctree_control(mincriterion=alpha))
response_fit <- ctree(resp.simple ~ ., response,
                      controls = ctree_control(mincriterion=alpha))
remission_fit <- ctree(Remission_Duration ~ ., remission,
                       controls = ctree_control(mincriterion=alpha))
vitals_fit <- ctree(vital.status ~ ., vitals,
                    controls = ctree_control(mincriterion=alpha))
relapse_fit <- ctree(Relapse ~ ., relapse,
                     controls = ctree_control(mincriterion=alpha))

n = 500
m = 16
survival_fit2 <- cforest(Overall_Survival ~ ., survival, 
                        controls = cforest_control(mtry=m,
                                                   ntree=n,
                                                   mincriterion=alpha))
response_fit2 <- cforest(resp.simple ~ ., response,
                        controls = cforest_control(mtry=m,
                                                   ntree=n,
                                                   mincriterion=alpha))
remission_fit2 <- cforest(Remission_Duration ~ ., remission,
                         controls = cforest_control(mtry=m,
                                                    ntree=n,
                                                    mincriterion=alpha))
vitals_fit2 <- cforest(vital.status ~ ., vitals,
                      controls = cforest_control(mtry=m,
                                                 ntree=n,
                                                 mincriterion=alpha))
relapse_fit2 <- cforest(Relapse ~ ., relapse,
                       controls = cforest_control(mtry=m,
                                                  ntree=n,
                                                  mincriterion=alpha))
response_fit3 <- ada(resp.simple ~ ., response)
vitals_fit3 <- ada(vital.status ~ ., vitals)
relapse_fit3 <- ada(Relapse ~ ., relapse)

survival_fit4 <- ksvm(Overall_Survival ~ ., survival)
remission_fit4 <- ksvm(Remission_Duration ~ ., remission)
response_fit4 <- ksvm(resp.simple ~ ., response)
vitals_fit4 <- ksvm(vital.status ~ ., vitals)
relapse_fit4 <- ksvm(Relapse ~ ., relapse)

survival_hat = predict(survival_fit, survival, OOB=T)
response_hat = predict(response_fit, response, OOB=T)
remission_hat = predict(remission_fit, remission, OOB=T)
vitals_hat = predict(vitals_fit, vitals, OOB=T)
relapse_hat = predict(relapse_fit, relapse, OOB=T)

survival_hat2 = predict(survival_fit2, survival, OOB=T)
response_hat2 = predict(response_fit2, response, OOB=T)
remission_hat2 = predict(remission_fit2, remission, OOB=T)
vitals_hat2 = predict(vitals_fit2, vitals, OOB=T)
relapse_hat2 = predict(relapse_fit2, relapse, OOB=T)

response_hat3 = predict(response_fit3, response)
vitals_hat3 = predict(vitals_fit3, vitals)
relapse_hat3 = predict(relapse_fit3, relapse)

survival_hat4 = predict(survival_fit4, survival)
remission_hat4 = predict(remission_fit4, remission)
response_hat4 = predict(response_fit4, response)
vitals_hat4 = predict(vitals_fit4, vitals)
relapse_hat4 = predict(relapse_fit4, relapse)

survival1_r2 = r_square(survival$Overall_Survival, survival_hat)
response1_bac = bac(response$resp.simple, response_hat, "CR")
remission1_r2 = r_square(remission$Remission_Duration, remission_hat)
vitals1_bac = bac(vitals$vital.status, vitals_hat, 'A')
relapse1_bac = bac(relapse$Relapse, relapse_hat, "Yes")

survival2_r2 = r_square(survival$Overall_Survival, survival_hat2)
response2_bac = bac(response$resp.simple, response_hat2, "CR")
remission2_r2 = r_square(remission$Remission_Duration, remission_hat2)
vitals2_bac = bac(vitals$vital.status, vitals_hat2, 'A')
relapse2_bac = bac(relapse$Relapse, relapse_hat2, "Yes")

response3_bac = bac(response$resp.simple, response_hat3, "CR")
vitals3_bac = bac(vitals$vital.status, vitals_hat3, 'A')
relapse3_bac = bac(relapse$Relapse, relapse_hat3, "Yes")

survival4_r2 = r_square(survival$Overall_Survival, survival_hat4)
response4_bac = bac(response$resp.simple, response_hat4, "CR")
remission4_r2 = r_square(remission$Remission_Duration, remission_hat4)
vitals4_bac = bac(vitals$vital.status, vitals_hat4, 'A')
relapse4_bac = bac(relapse$Relapse, relapse_hat4, "Yes")

survival_test1 = predict(survival_fit, test, OOB=T)
response_test1 = predict(response_fit, test, OOB=T)
remission_test1 = predict(remission_fit, test1, OOB=T)
vitals_test1 = predict(vitals_fit, test, OOB=T)
relapse_test1 = predict(relapse_fit, test2, OOB=T)

survival_test2 = predict(survival_fit2, test, OOB=T)
response_test2 = predict(response_fit2, test, OOB=T)
remission_test2 = predict(remission_fit2, test1, OOB=T)
vitals_test2 = predict(vitals_fit2, test, OOB=T)
relapse_test2 = predict(relapse_fit2, test2, OOB=T)

response_test3 = predict(response_fit3, test)
vitals_test3 = predict(vitals_fit3, test)
relapse_test3 = predict(relapse_fit3, test2)

survival_test1_r2 = r_square(test$Overall_Survival, survival_test1)
response_test1_bac = bac(test$resp.simple, response_test1, "CR")
remission_test1_r2 = r_square(test1$Remission_Duration, remission_test1)
vitals_test1_bac = bac(test$vital.status, vitals_test1, "A")
relapse_test1_bac = bac(test2$Relapse, relapse_test1, "Yes")

survival_test2_r2 = r_square(test$Overall_Survival, survival_test2)
response_test2_bac = bac(test$resp.simple, response_test2, "CR")
remission_test2_r2 = r_square(test1$Remission_Duration, remission_test2)
vitals_test2_bac = bac(test$vital.status, vitals_test2, "A")
relapse_test2_bac = bac(test2$Relapse, relapse_test2, "Yes")

response_test3_bac = bac(test$resp.simple, response_test3, "CR")
vitals_test3_bac = bac(test$vital.status, vitals_test3, "A")
relapse_test3_bac = bac(test2$Relapse, relapse_test3, "Yes")

print("survival train r2")
print(c(survival1_r2, survival2_r2))

print("remission train r2")
print(c(remission1_r2, remission2_r2))

print("relapse train bac")
print(c(relapse1_bac, relapse2_bac, relapse3_bac))

print("response train bac")
print(c(response1_bac, response2_bac, response3_bac))

print("vitals train bac")
print(c(vitals1_bac, vitals2_bac, vitals3_bac))

print("survival test r2")
print(c(survival_test1_r2, survival_test2_r2))

print("remission test r2")
print(c(remission_test1_r2, remission_test2_r2))

print("relapse test bac")
print(c(relapse_test1_bac, relapse_test2_bac, relapse_test3_bac))

print("response test bac")
print(c(response_test1_bac, response_test2_bac, relapse_test3_bac))

print("vitals test bac")
print(c(vitals_test1_bac, vitals_test2_bac, relapse_test3_bac))

plot(survival_fit)
plot(remission_fit)
plot(response_fit3)
plot(relapse_fit3)
plot(vitals_fit3)