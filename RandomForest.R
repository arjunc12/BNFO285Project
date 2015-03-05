#Set working directory.
setwd("/Users/sjroth/Documents/BNFO285/Final/")

#Read data.
data <- read.csv("trainingData-release.csv")
data <- na.omit(data)

#Get survival, relapse, and vitals data.
survival = data[,!(colnames(data) %in% c("Relapse", "vital.status")),
                drop=F]
relapse = data[,!(colnames(data) %in% c("Overall_Survival",
                                        "vital.status")),drop=F]
vitals = data[,!(colnames(data) %in% c("Relapse", 
                                       "Overall_Survival")),drop=F]

#Fit the data using random forest.
library(party)
survival_fit <- cforest(Overall_Survival ~ ., survival)
relapse_fit <- cforest(Relapse ~ ., relapse)
vitals_fit <- cforest(vital.status ~ ., vitals)

survival_hat = predict(survival_fit, survival,OOB=T)
relapse_hat = predict(relapse_fit, relapse,OOB=T)
vitals_hat = predict(vitals_fit, vitals,OOB=T)
