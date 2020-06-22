

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(data.table)
library(tibble)
library(pillar)
library(readr)

temp<-tempfile()
url<-"https://raw.githubusercontent.com/mpalme/tree_shadows/master/Shadows.csv"
download.file(url,temp)
dat <-read_csv(temp)
data<-data.frame(dat)


temp_1<-tempfile()
url_1<-"https://raw.githubusercontent.com/mpalme/tree_shadows/master/z.csv"
download.file(url_1,temp_1)
v<-read.csv(temp_1,header=FALSE)


temp_2<-tempfile()
url_2<-"https://raw.githubusercontent.com/mpalme/tree_shadows/master/rep.csv"
download.file(url_2,temp_2)
rep<-read.csv(temp_2,header=FALSE)


set.seed(3, sample.kind = "Rounding")
test_index<-createDataPartition(data$saving,times=1,p=0.25,list=FALSE)
test_set <- data[test_index, ]
train_set <- data[-test_index, ]

test_v<-v[test_index, ]
test_rep<-rep[test_index, ]

train_knn<-train(saving~.,method="knn",data=train_set)
y_hat_knn<-predict(train_knn,test_set,type="raw")
y_hat_1<-ifelse(y_hat_knn>15,"Y","N")
y_hat_1<-as.factor(y_hat_1)
confusionMatrix(y_hat_1,test_v)
accuracy_1<-confusionMatrix(y_hat_1,test_v)$overall["Accuracy"]


train_rf<-train(saving~.,method="rf",data=train_set)
y_hat_rf<-predict(train_rf,test_set,type="raw")
y_hat_2<-ifelse(y_hat_rf>15,"Y","N")
y_hat_2<-as.factor(y_hat_2)
confusionMatrix(y_hat_2,test_v)
varImp(train_rf)
accuracy_2<-confusionMatrix(y_hat_2,test_v)$overall["Accuracy"]

train_gamLoess<-train(saving~.,method="gamLoess",data=train_set)
y_hat_gamLoess<-predict(train_gamLoess,test_set,type="raw")
y_hat_3<-ifelse(y_hat_gamLoess>15,"Y","N")
y_hat_3<-as.factor(y_hat_3)
confusionMatrix(y_hat_3,test_v)
accuracy_3<-confusionMatrix(y_hat_3,test_v)$overall["Accuracy"]

train_glm<-train(saving~.,method="glm",data=train_set)
y_hat_glm<-predict(train_glm,test_set,type="raw")
y_hat_4<-ifelse(y_hat_glm>15,"Y","N")
y_hat_4<-as.factor(y_hat_4)
confusionMatrix(y_hat_4,test_v)
accuracy_4<-confusionMatrix(y_hat_4,test_v)$overall["Accuracy"]

y_hat_resp_ensamble<-(y_hat_gamLoess+y_hat_glm+y_hat_knn+y_hat_rf)/4


y_hat_ensamble<-ifelse(y_hat_resp_ensamble>15,"Y","N")
y_hat_ensamble<-as.factor(y_hat_ensamble)
confusionMatrix(y_hat_ensamble,test_v)
accuracy_5<-confusionMatrix(y_hat_ensamble,test_v)$overall["Accuracy"]





y_hat_resp_4<-ifelse(y_hat_gamLoess<5,"very low",ifelse(y_hat_gamLoess<15,"low",ifelse(y_hat_gamLoess<25,"medium",ifelse(y_hat_gamLoess<35,"high","very high"))))
y_hat_resp_4<-as.factor(y_hat_resp_4)
confusionMatrix(y_hat_resp_4,test_rep)
accuracy_resp_4<-confusionMatrix(y_hat_resp_4,test_rep)$overall["Accuracy"]

y_hat_resp_3<-ifelse(y_hat_glm<5,"very low",ifelse(y_hat_glm<15,"low",ifelse(y_hat_glm<25,"medium",ifelse(y_hat_glm<35,"high","very high"))))
y_hat_resp_3<-as.factor(y_hat_resp_3)
confusionMatrix(y_hat_resp_3,test_rep)
accuracy_resp_3<-confusionMatrix(y_hat_resp_3,test_rep)$overall["Accuracy"]

y_hat_resp_2<-ifelse(y_hat_rf<5,"very low",ifelse(y_hat_rf<15,"low",ifelse(y_hat_rf<25,"medium",ifelse(y_hat_rf<35,"high","very high"))))
y_hat_resp_2<-as.factor(y_hat_resp_2)
confusionMatrix(y_hat_resp_2,test_rep)
accuracy_resp_2<-confusionMatrix(y_hat_resp_2,test_rep)$overall["Accuracy"]

y_hat_resp_1<-ifelse(y_hat_knn<5,"very low",ifelse(y_hat_knn<15,"low",ifelse(y_hat_knn<25,"medium",ifelse(y_hat_knn<35,"high","very high"))))
y_hat_resp_1<-as.factor(y_hat_resp_1)
confusionMatrix(y_hat_resp_1,test_rep)
accuracy_resp_1<-confusionMatrix(y_hat_resp_1,test_rep)$overall["Accuracy"]



y_hat_resp_5<-ifelse(y_hat_resp_ensamble<5,"very low",ifelse(y_hat_resp_ensamble<15,"low",ifelse(y_hat_resp_ensamble<25,"medium",ifelse(y_hat_resp_ensamble<35,"high","very high"))))
y_hat_resp_5<-as.factor(y_hat_resp_5)
confusionMatrix(y_hat_resp_5,test_rep)
accuracy_resp_5<-confusionMatrix(y_hat_resp_5,test_rep)$overall["Accuracy"]

RMSE_5<-sqrt(mean((y_hat_resp_ensamble-test_set$saving)^2))
RMSE_2<-sqrt(mean((y_hat_rf-test_set$saving)^2))
RMSE_1<-sqrt(mean((y_hat_knn-test_set$saving)^2))
RMSE_3<-sqrt(mean((y_hat_glm-test_set$saving)^2))
RMSE_4<-sqrt(mean((y_hat_gamLoess-test_set$saving)^2))
mean(test_set$saving)


options(pillar.sigfig=3)

rmse_results <- tibble(method = c("Knn","Rf","Glm","Loess","Ensamble"), RMSE = c(RMSE_1,RMSE_2,RMSE_3,RMSE_4,RMSE_5))

rmse_results

accuracy_results<-tibble(method = c("Knn","Rf","Glm","Loess","Ensamble"), Accuracy = c(accuracy_1,accuracy_2,accuracy_3,accuracy_4,accuracy_5))

accuracy_results

accuracy_resp_results<-tibble(method = c("Knn","Rf","Glm","Loess","Ensamble"), Accuracy_cat = c(accuracy_resp_1,accuracy_resp_2,accuracy_resp_3,accuracy_resp_4,accuracy_resp_5))

accuracy_resp_results