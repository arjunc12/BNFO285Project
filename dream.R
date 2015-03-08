library(data.table)
library(party)

data0 <- read.csv("trainingData-release.csv")
na.omit(data)
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

survival_fit <- ctree(Overall_Survival ~ ., survival)
response_fit <- ctree(resp.simple ~ ., response)
remission_fit <- ctree(Remission_Duration ~ ., remission)

survival_fit2 <- cforest(Overall_Survival ~ ., survival)
response_fit2 <- cforest(resp.simple ~ ., response)
remission_fit2 <- cforest(Remission_Duration ~ ., remission)

# survival_fit3 <- lm(Overall_Survival ~ ., survival, na.action=na.omit)
# response_fit3 <- glm(resp.simple ~ ., response, family="binomial")
# remission_fit3 <- lm(Remission_Duration ~ ., remission)

survival_hat = predict(survival_fit, survival, OOB=T)
response_hat = predict(response_fit, response, OOB=T)
remission_hat = predict(remission_fit, remission, OOB=T)

survival_hat2 = predict(survival_fit2, survival,OOB=T)
response_hat2 = predict(response_fit2, response,OOB=T)
remission_hat2 = predict(remission_fit2, remission,OOB=T)

# survival_hat3 = predict(survival_fit3, survival)
# response_hat3 = predict(response_fit3, response)
# remission_hat3 = predict(remission_fit3, remission)

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

survival1_r2 = r_square(survival$Overall_Survival, survival_hat)
response1_ein = error_rate(response$resp.simple, response_hat)
remission1_r2 = r_square(remission$Remission_Duration, remission_hat)

survival2_r2 = r_square(survival$Overall_Survival, survival_hat2)
response2_ein = error_rate(response$resp.simple, response_hat2)
remission2_r2 = r_square(remission$Remission_Duration, remission_hat2)

# survival3_r2 = r_square(survival$Overall_Survival, survival_hat3)
# response3_ein = error_rate(response$resp.simple, response_hat3)
# remission3_r2 = r_square(remission$Remission_Duration, remission_hat3)

survival_test1 = predict(survival_fit, test)
response_test1 = predict(response_fit, test)
remission_test1 = predict(remission_fit, test1)

# survival_test2 = predict(survival_fit2, test)
# response_test2 = predict(response_fit2, test)
# remission_test2 = predict(remission_fit2, test)
# 
# survival_test3 = predict(survival_fit3, test)
# response_test3 = predict(response_fit3, test)
# remission_test3 = predict(remission_fit3, test)

survival_test1_r2 = r_square(test$Overall_Survival, survival_test1)
response_test1_ein = error_rate(test$resp.simple, response_test1)
remission_test1_r2 = r_square(test1$Remission_Duration, remission_test1)
