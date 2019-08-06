# Telcom_data_analysis
This is a team project we did in June 2019
Crew members: 洪浚皓 蕭立承 江冠駒

## Dataset name: Telco Costomer Churn
## Dataset URL: https://www.kaggle.com/blastchar/telco-customer-churn

## Introduction of dataset
This Telco Costomer Churn dataset is from an anonymous telcom with many variables, including basic info. of clients, and the type of services subscribed by clients.

This data set contains over 20 variables with categoracal and numerical variables.

## The Purpose of Research
We have two main purposes, classifying and clustering.

The purpose of classfying being using machine learning techniques to predict what kind of contracts will custermers choose to be their next contract to sign.

The purpose of clustering being making selling strategies by finding the common among custermers.

## Results
### Classfying
First, we make a testing set by using stratified sampling.

We use several statistical learning techniques to find out which is better on 10-fold CV, and we use that model to be our final model.

And we find out random forest will perform a lot better than others, also the performance on testing has nearly 90% accuracy.

### Clustering
First, we choose the varible "manually", since the structure of the variable has some collinearity which we think it's bad for our clustering result.

For example, if you didn't applied the phone service, you will not apply multiple lines since you don't have phone service.

Then we can make a MDS matrix, then use it to clustering by K-means method. We can see the result is promising.

Therefore, the right strategy will be focus on those not-married clients. We can observe that those clients have high monthly charges, and the contracts they sign mostly are "month-to-month" contract, so they have high sensitivity on Telcom market, that is, we should make a good strategy to keep them.




