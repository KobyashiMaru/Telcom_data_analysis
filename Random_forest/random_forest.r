library(readr)

WA_Fn_UseC_Telco_Customer_Churn <- read_csv("C:/Users/bronc/Desktop/WA_Fn-UseC_-Telco-Customer-Churn.csv")


WA_Fn_UseC_Telco_Customer_Churn["index"] = seq(1:7043)

WA_Fn_UseC_Telco_Customer_Churn$TotalCharges[is.na(WA_Fn_UseC_Telco_Customer_Churn$TotalCharges)]=0
#Contract = WA_Fn_UseC_Telco_Customer_Churn$Contract



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

#==========================random froest======================


library(caret)
library(rattle) 
set.seed(520)
tree_control<-trainControl(method = 'cv',
                           search = 'grid',
                           number = 10)
training_set = training_set[-20]

rf<-train(Contract ~ ., 
          data = training_set, 
          method = 'rf',
          tuneGrid = expand.grid(mtry = c(1:18)),
          trControl = tree_control,ntree = 500,cp = 0.01,nodesize = 3  
)




#==========================random froest======================

plot(rf)

#testing


training_set = training_set[-20]
predictor = predict(rf, training_set[, c(1:14, 16:19)])

table(training_set$Contract, predictor)



#=======================feature selection==================

importance <- varImp(rf, scale=FALSE)
print(importance)
plot(importance)


#=======================feature selection==================
