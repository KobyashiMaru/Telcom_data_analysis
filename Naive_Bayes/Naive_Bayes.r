library(readr)
library(e1071)

WA_Fn_UseC_Telco_Customer_Churn <- read_csv("C:/Users/bronc/Desktop/WA_Fn-UseC_-Telco-Customer-Churn.csv")


WA_Fn_UseC_Telco_Customer_Churn["index"] = seq(1:7043)

WA_Fn_UseC_Telco_Customer_Churn$TotalCharges[is.na(WA_Fn_UseC_Telco_Customer_Churn$TotalCharges)]=0
#Contract = WA_Fn_UseC_Telco_Customer_Churn$Contract

#factor

WA_Fn_UseC_Telco_Customer_Churn$gender = as.factor(WA_Fn_UseC_Telco_Customer_Churn$gender)
WA_Fn_UseC_Telco_Customer_Churn$Partner = as.factor(WA_Fn_UseC_Telco_Customer_Churn$Partner)
WA_Fn_UseC_Telco_Customer_Churn$Dependents = as.factor(WA_Fn_UseC_Telco_Customer_Churn$Dependents)
WA_Fn_UseC_Telco_Customer_Churn$PhoneService = as.factor(WA_Fn_UseC_Telco_Customer_Churn$PhoneService)
WA_Fn_UseC_Telco_Customer_Churn$MultipleLines = as.factor(WA_Fn_UseC_Telco_Customer_Churn$MultipleLines)
WA_Fn_UseC_Telco_Customer_Churn$InternetService = as.factor(WA_Fn_UseC_Telco_Customer_Churn$InternetService)
WA_Fn_UseC_Telco_Customer_Churn$OnlineSecurity = as.factor(WA_Fn_UseC_Telco_Customer_Churn$OnlineSecurity)
WA_Fn_UseC_Telco_Customer_Churn$OnlineBackup = as.factor(WA_Fn_UseC_Telco_Customer_Churn$OnlineBackup)
WA_Fn_UseC_Telco_Customer_Churn$DeviceProtection = as.factor(WA_Fn_UseC_Telco_Customer_Churn$DeviceProtection)
WA_Fn_UseC_Telco_Customer_Churn$TechSupport = as.factor(WA_Fn_UseC_Telco_Customer_Churn$TechSupport)
WA_Fn_UseC_Telco_Customer_Churn$StreamingTV = as.factor(WA_Fn_UseC_Telco_Customer_Churn$StreamingTV)
WA_Fn_UseC_Telco_Customer_Churn$StreamingMovies = as.factor(WA_Fn_UseC_Telco_Customer_Churn$StreamingMovies)
WA_Fn_UseC_Telco_Customer_Churn$Contract = as.factor(WA_Fn_UseC_Telco_Customer_Churn$Contract)
WA_Fn_UseC_Telco_Customer_Churn$PaperlessBilling = as.factor(WA_Fn_UseC_Telco_Customer_Churn$PaperlessBilling)
WA_Fn_UseC_Telco_Customer_Churn$PaymentMethod = as.factor(WA_Fn_UseC_Telco_Customer_Churn$PaymentMethod)




#factor



layer1<-WA_Fn_UseC_Telco_Customer_Churn[which(WA_Fn_UseC_Telco_Customer_Churn$Contract=='Month-to-month'),]
n1=2700
train_data_index = sample(layer1$index, size = 2700)

layer2<-WA_Fn_UseC_Telco_Customer_Churn[which(WA_Fn_UseC_Telco_Customer_Churn$Contract=='One year'),]
n2=1030
train_data_index2 = sample(layer2$index, size = 1030)

layer3<-WA_Fn_UseC_Telco_Customer_Churn[which(WA_Fn_UseC_Telco_Customer_Churn$Contract=='Two year'),]
n3=1180
train_data_index3 = sample(layer3$index, size = 1180)

training_set = WA_Fn_UseC_Telco_Customer_Churn[c(train_data_index, train_data_index2, train_data_index3), ]
training_set = training_set[-20]


training_set_index = sort(c(train_data_index, train_data_index2, train_data_index3))

testing_set_index = c()
i = 1
counter = 1

for(i in seq(1:7043)){
  if (sum(training_set_index == i) == 0){
    testing_set_index[counter] = i
    counter = counter + 1
  }
  
}




testing_set = WA_Fn_UseC_Telco_Customer_Churn[testing_set_index, ]
testing_set = testing_set[-20]
#=====================Naive Bayes====================


Telcom_data_NB = naiveBayes(Contract ~., 
                            data = training_set
                            )




#=====================Naive Bayes====================

#testing

Telcom_data_NB_pred = predict(Telcom_data_NB, training_set)
table(training_set$Contract, Telcom_data_NB_pred)


#===================CV==========================
Telcom_data_NB_CV = naiveBayes(Contract ~., 
                               data = training_set, 
                               trControl = trainControl(method = 'cv',number = 10)
)


#testing
Telcom_data_NB_CV_pred = predict(Telcom_data_NB_CV, training_set)
table(training_set$Contract, Telcom_data_NB_CV_pred)



#===================CV==========================