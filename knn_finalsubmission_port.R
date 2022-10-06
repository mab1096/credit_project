rm(list=ls())
library(tidyverse)
library(modelr)
library(skimr)
library(gmodels)
library(class)
library(caret)
library(pROC)
library(mlbench)
credit_test<-read_csv("credit_test_me.csv")
credit_train<-read_csv("credit_train_me.csv")
set.seed(17)


credit_train$default[credit_train$default == 0] <- 'No'
credit_train$default[credit_train$default == 1] <- 'Yes'
credit_train$default <- factor(credit_train$default)



m2<-glm(default~., data = credit_train, family = binomial)
logit_pred<- as.numeric(predict(m2, credit_train)>0.5)
CrossTable(x = credit_train$default, y = logit_pred,
           prop.chisq = F)
logit_test <- as.numeric(predict(m2, credit_test)>0.5)
credit_test<- cbind(credit_test, logit_test)
credit_test <-rename(credit_test, default = logit_test)
credit_test$default[credit_test$default == 0] <- 'No'
credit_test$default[credit_test$default == 1] <- 'Yes'





tc <- trainControl(method = "repeatedcv",
                   number = 10,
                   repeats =3)

fit <- train(default ~. - job - employment_duration - other_credit - purpose - age,
             data = credit_train,
             method = 'knn',
             tuneLength = 20,
             trControl = tc,
             preProc = c("center", "scale"))

summary(fit)
fit
plot(fit)


defaulted <- predict(fit, newdata = credit_test)
id <- credit_test$id
pred_df <- as.data.frame(id)
pred_df$defaulted <- defaulted
write.csv(pred_df, file = "credit_submission.csv", row.names = FALSE)
credit_submission<-read_csv("credit_submission.csv")
view(credit_submission)


