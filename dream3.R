library(data.table)
library(party)

data0 <- read.csv("trainingData-release.csv")
#na.omit(data)
data <- data0
data$Patient_id <- NULL

indices <- 1:nrow(data)
trainIndex <- sample(indices, trunc(length(indices)/2))
train <- data[trainIndex,]
train1 <- train[!is.na(train['Remission_Duration']),]
test <- data[-trainIndex,]
test1 <- test[!is.na(test['Remission_Duration']),]

survival = train[,!(colnames(train) %in% c("resp.simple", "Remission_Duration")),
                drop=F]
response = train[,!(colnames(train) %in% c("Overall_Survival"
                                        , "Remission_Duration")),
               drop=F]
remission = train1[,!(colnames(train) %in% c("resp.simple", 
                                       "Overall_Survival")),
              drop=F]

# survival_fit <- ctree(Overall_Survival ~ ., survival,
#                       controls = ctree_control(mincriterion=0.9))
# response_fit <- ctree(resp.simple ~ ., response,
#                       controls = ctree_control(mincriterion=0.9))
# remission_fit <- ctree(Remission_Duration ~ ., remission,
#                        controls = ctree_control(mincriterion=0.9))
# 
# survival_hat = predict(survival_fit, survival, OOB=T)
# response_hat = predict(response_fit, response, OOB=T)
# remission_hat = predict(remission_fit, remission, OOB=T)

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

# survival1_r2 = r_square(survival$Overall_Survival, survival_hat)
# response1_ein = error_rate(response$resp.simple, response_hat)
# remission1_r2 = r_square(remission$Remission_Duration, remission_hat)
# 
# survival_test1 = predict(survival_fit, test)
# response_test1 = predict(response_fit, test)
# remission_test1 = predict(remission_fit, test1)

survival_alpha <- function(alpha)
{
  survival_fit <- ctree(Overall_Survival ~ ., survival,
                        controls = ctree_control(mincriterion=alpha))
  
  survival_hat = predict(survival_fit, survival, OOB=T)
  
  survival1_r2 = r_square(survival$Overall_Survival, survival_hat)
  
  return(survival1_r2)
}

remission_alpha <- function(alpha)
{
  remission_fit <- ctree(Remission_Duration ~ ., remission,
                         controls = ctree_control(mincriterion=alpha))
  
  remission_hat = predict(remission_fit, remission, OOB=T)
  
  remission1_r2 = r_square(remission$Remission_Duration, remission_hat)
  
  return(remission1_r2)
}

response_alpha <- function(alpha)
{
  response_fit <- ctree(resp.simple ~ ., response,
                        controls = ctree_control(mincriterion=alpha))
  
  response_hat = predict(response_fit, response, OOB=T)
  
  response1_ein = error_rate(response$resp.simple, response_hat)
  
  return(response1_ein)
}

al <- seq(0.5, 0.99, 0.01)
survival_r2 <- sapply(al, survival_alpha)
remission_r2 <- sapply(al, remission_alpha)
response_ein <- sapply(al, response_alpha)

plot(al, survival_r2)
plot(al, remission_r2)
plot(al, response_ein)