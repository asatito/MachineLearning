---
title: "Machine Learning Assignment"
author: "ASati"
date: "Sunday, September 27, 2015"
output: html_document
---

## Introduction

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 
 Training and Test data sets were provided and the goal of the project was to predict the manner in which they did the exercise.


## Data Clean Up

Clean up of data sets was done in a three step process:

1. Removed all coulmns which had only NAs
2. Removed the first seven column which were basically related
   to time/user/window during which the activity was performed
   and would have had no bearing on the actual ourcome of the 
   classe variable.
3. Only numeric data was kept to allow for random forest algorithm
   to be applied.


```r
trnData = read.csv("pml-training.csv")        
tstData =read.csv("pml-testing.csv")

clean_trnData=trnData[, colSums(is.na(trnData)) == 0] 
clean_trnData_2 = clean_trnData[,-(1:7)]
clean_trnData_3= clean_trnData_2[, sapply(clean_trnData_2, is.numeric)]
clean_trnData_3$classe <- clean_trnData_2$classe

clean_tstData=tstData[, colSums(is.na(tstData)) == 0] 
clean_tstData_2 = clean_tstData[,-(1:7)]
clean_tstData_3= clean_tstData_2[, sapply(clean_tstData_2, is.numeric)]
```

## Data Modelling

For the data modelling random forest algorithm was chosen mainly for the below reasons:

1. It is one of the most accurate learning algorithms available. For many data sets, it produces a highly accurate classifier.
2. It runs efficiently on large datasets.
3. It is less sensitive to outliers and parameter choices.


```r
set.seed(125) # For reproducibile purpose
inTrain <- createDataPartition(clean_trnData_3$classe, p=0.70, list=F)
trainData <- clean_trnData_3[inTrain, ]
testData <- clean_trnData_3[-inTrain, ]


rf_cntl <- trainControl(method="cv", 5)
rf_model <- train(classe ~ ., data=trainData, method="rf", trControl=rf_cntl, ntree=250)
```

## Out Of Sample Error and Accuracy

The model was then used to predict the outcome of the test data.
From the below we can see the accuracy was 99.20% and the out
of sample error was 0.8%


```r
rf_model
```

```
## Random Forest 
## 
## 13737 samples
##    52 predictor
##     5 classes: 'A', 'B', 'C', 'D', 'E' 
## 
## No pre-processing
## Resampling: Cross-Validated (5 fold) 
## Summary of sample sizes: 10989, 10990, 10990, 10990, 10989 
## Resampling results across tuning parameters:
## 
##   mtry  Accuracy   Kappa      Accuracy SD  Kappa SD   
##    2    0.9909731  0.9885806  0.001596261  0.002019037
##   27    0.9918467  0.9896859  0.001303602  0.001648777
##   52    0.9850765  0.9811217  0.003444690  0.004356449
## 
## Accuracy was used to select the optimal model using  the largest value.
## The final value used for the model was mtry = 27.
```

```r
rf_predict <- predict(rf_model, testData)
confusionMatrix(testData$classe, rf_predict)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1673    0    1    0    0
##          B    7 1129    3    0    0
##          C    0    9 1010    7    0
##          D    0    0   12  952    0
##          E    0    1    6    1 1074
## 
## Overall Statistics
##                                           
##                Accuracy : 0.992           
##                  95% CI : (0.9894, 0.9941)
##     No Information Rate : 0.2855          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : 0.9899          
##  Mcnemar's Test P-Value : NA              
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity            0.9958   0.9912   0.9787   0.9917   1.0000
## Specificity            0.9998   0.9979   0.9967   0.9976   0.9983
## Pos Pred Value         0.9994   0.9912   0.9844   0.9876   0.9926
## Neg Pred Value         0.9983   0.9979   0.9955   0.9984   1.0000
## Prevalence             0.2855   0.1935   0.1754   0.1631   0.1825
## Detection Rate         0.2843   0.1918   0.1716   0.1618   0.1825
## Detection Prevalence   0.2845   0.1935   0.1743   0.1638   0.1839
## Balanced Accuracy      0.9978   0.9946   0.9877   0.9946   0.9992
```

```r
Accuracy <- postResample(rf_predict, testData$classe)
Accuracy
```

```
##  Accuracy     Kappa 
## 0.9920136 0.9898969
```

```r
out_of_sample_error <- 1 - as.numeric(confusionMatrix(testData$classe, rf_predict)$overall[1])
out_of_sample_error
```

```
## [1] 0.007986406
```

## Predicting the data and submitting.

Below script was used to predict the outcome of test data and to submit
the assignment answers based on the 20 files generated by the pml_write_files function.


```r
answers <- predict(rf_model, clean_tstData_3[, -53])
answers
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)
```


