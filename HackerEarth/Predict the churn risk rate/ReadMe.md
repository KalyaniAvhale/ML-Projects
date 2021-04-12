Competition Page link : https://www.hackerearth.com/challenges/competitive/hackerearth-machine-learning-challenge-predict-customer-churn/

**Problem statement**

No business can thrive without it’s customers. On the flip side, customers leaving the business is a nightmare that every business owner dreads!

In fact, one of the key metrics to measure a business’ success is by measuring its customer churn rate - the lower the churn, the more loved the company is. 

Typically, every user of a product or a service is assigned a prediction value that estimates their state of churn at any given time. This value may be based on multiple factors such as the user’s demographic, their browsing behavior and historical purchase data, among other details.

This value factors in unique and proprietary predictions of how long a user will remain a customer and is updated every day for all users who have purchased at least one of the products/services. The values assigned are between 1 and 5.

**Task**

An up-and-coming startup is keen on reducing its customer churn and has hired you as a Machine Learning engineer for this task. As an expert, you are required to build a sophisticated Machine Learning model that predicts the churn score for a website based on multiple features.

**Data description**
The dataset folder contains the following files:
_
train.csv: 36992 x 25
test.csv: 19919 x 24
sample_submission.csv: 5 x 2_

The columns provided in the dataset are as follows:


![DD](https://user-images.githubusercontent.com/47108982/114455306-3a2b2480-9bf9-11eb-906d-e2db53747d78.PNG)




**Evaluation metric**
_score = 100 x metrics.f1_score(actual, predicted, average="macro")_




