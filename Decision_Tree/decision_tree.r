library(readr)
library(rpart)
library(rpart.plot)

WA_Fn_UseC_Telco_Customer_Churn <- read_csv("C:/Users/bronc/Desktop/WA_Fn-UseC_-Telco-Customer-Churn.csv")


WA_Fn_UseC_Telco_Customer_Churn["index"] = seq(1:7043)

WA_Fn_UseC_Telco_Customer_Churn$TotalCharges[is.na(WA_Fn_UseC_Telco_Customer_Churn$TotalCharges)]=0
#Contract = WA_Fn_UseC_Telco_Customer_Churn$Contract


#factor
WA_Fn_UseC_Telco_Customer_Churn$SeniorCitizen = as.factor(WA_Fn_UseC_Telco_Customer_Churn$SeniorCitizen)
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


#=======================decision tree================================
telcom_data.control = rpart.control(minsplit = 20, minibucket = 6, xval = 0)

telcom_data.treeorig = rpart(Contract ~. , 
                             data = training_set, 
                             method = "class", 
                             control = telcom_data.control)



printcp(telcom_data.treeorig)


prp(telcom_data.treeorig,    # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)


#=======================decision tree================================

#=======================prune=================================

telcom_data.prune = prune.rpart(telcom_data.treeorig, cp = 0.03)

prp(telcom_data.prune,    # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)






#=======================prune=================================

#===============================testing============================

testing_set.pred_matrix = predict(telcom_data.treeorig, testing_set)

print(testing_set.pred_matrix)

testing_set.pred = c()

i = 1


for (i in seq(1:dim(testing_set.pred_matrix)[1])){
  col_index = which(testing_set.pred_matrix[i, ] == max(testing_set.pred_matrix[i, ]))
  if (col_index == 1){
    testing_set.pred[i] = "Month-to-month"
  } else if (col_index == 2){
    testing_set.pred[i] = "One year"
  } else {
    testing_set.pred[i] = "Two year"
  }
}


table(testing_set$Contract, testing_set.pred)



#===============================testing============================

#===========================CV================================

telcom_data.control = rpart.control(minsplit = 20, minibucket = 6, xval = 10)

telcom_data.treecv = rpart(Contract ~. , 
                           data = training_set, 
                           method = "class", 
                           control = telcom_data.control, 
                           parms = list(split = "information"))

prp(telcom_data.treecv,    # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)
printcp(telcom_data.treecv)

plotcp(telcom_data.treecv)

#===========================CV================================

#===============================testingCV============================

testing_set.pred_matrix_CV = predict(telcom_data.treecv, testing_set)

print(testing_set.pred_matrix_CV)

testing_set_CV.pred = c()

i = 1


for (i in seq(1:dim(testing_set.pred_matrix_CV)[1])){
  col_index = which(testing_set.pred_matrix_CV[i, ] == max(testing_set.pred_matrix_CV[i, ]))
  if (col_index == 1){
    testing_set_CV.pred[i] = "Month-to-month"
  } else if (col_index == 2){
    testing_set_CV.pred[i] = "One year"
  } else {
    testing_set_CV.pred[i] = "Two year"
  }
}


table(testing_set$Contract, testing_set_CV.pred)



#===============================testingCV============================
