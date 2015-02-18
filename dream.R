library(data.table)
library(party)

data <- read.csv("trainingData-release.csv")
data <- na.omit(data)

survival = data[,!(colnames(data) %in% c("Relapse", "vital.status")),
                drop=F]
relapse = data[,!(colnames(data) %in% c("Overall_Survival"
                                        , "vital.status")),
               drop=F]
#relapse <- na.omit(relapse)
vitals = data[,!(colnames(data) %in% c("Relapse", 
                                       "Overall_Survival")),
              drop=F]

survival_fit <- ctree(Overall_Survival ~ ., survival)
relapse_fit <- ctree(Relapse ~ ., relapse)
vitals_fit <- ctree(vital.status ~ ., vitals)

survival_hat = predict(survival_fit, survival)
relapse_hat = predict(relapse_fit, relapse)
vitals_hat = predict(vitals_fit, vitals)

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

survival_r2 = r_square(survival$Overall_Survival, survival_hat)
vitals_ein = error_rate(vitals$vital.status, vitals_hat)
relapse_ein = error_rate(relapse$Relapse, relapse_hat)
