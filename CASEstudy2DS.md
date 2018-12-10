---
title: "CASEstudy2DS"
author: "Solange GarciadeAlford"
date: "December 1, 2018"
output: 
  html_document:
    keep_md: true
---



# Data Science - Case Study 2

##Executive Summary

##Talent management is at the forefront of CDF’s business strategy.  One of the most valuable assets at CDF are the Employees that form the company.  

##CDF’s decision to analyze the company’s attrition is to ensure CDF continues to maintain its place as the leader in the industry.  Each one of CDF’s Employees is key to the success of the enterprise and to gain a competitive edge over its competition.

##DDSAnalytics was engaged by CDF to provide business intelligence to help retain its employees.  

##This analysis covers:
##   Attrition Trends
##   Top factors that contribute to CDF’s employee turnover
##   Attrition Prediction Model



```r
options(repos="https://cran.rstudio.com" )
install.packages("caret", dependencies = TRUE)
```

```
## Installing package into 'C:/Users/solan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
```

```
## package 'caret' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\solan\AppData\Local\Temp\RtmpU3qdwg\downloaded_packages
```

```r
library(caret)
```

```
## Loading required package: lattice
```

```
## Loading required package: ggplot2
```

```r
install.packages("class::knn")
```

```
## Installing package into 'C:/Users/solan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
```

```
## Warning: package 'class::knn' is not available (for R version 3.5.1)
```

```r
library(mlr)
```

```
## Loading required package: ParamHelpers
```

```
## 
## Attaching package: 'mlr'
```

```
## The following object is masked from 'package:caret':
## 
##     train
```

```r
library(class)
require(class)
install.packages("fpp")
```

```
## Installing package into 'C:/Users/solan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
```

```
## package 'fpp' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\solan\AppData\Local\Temp\RtmpU3qdwg\downloaded_packages
```

```r
library(fpp2)
```

```
## Loading required package: forecast
```

```
## Loading required package: fma
```

```
## Loading required package: expsmooth
```

```r
install.packages("dygraphs")
```

```
## Installing package into 'C:/Users/solan/Documents/R/win-library/3.5'
## (as 'lib' is unspecified)
```

```
## package 'dygraphs' successfully unpacked and MD5 sums checked
## 
## The downloaded binary packages are in
## 	C:\Users\solan\AppData\Local\Temp\RtmpU3qdwg\downloaded_packages
```

```r
library(dygraphs)
library(xts)
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(RColorBrewer)
##display.brewer.all()
cols<-brewer.pal(n=4,name="Set1")

## IMPORT DATA
## training data set 
trainingdata <- read.csv("C:/Users/solan/CaseStudy2-data.csv")
## Validation set 
testdata <- read.csv("C:/Users/solan/CaseStudy2Validation.csv")


## Pull Attrition data into new data frame
atty <- data.frame(trainingdata[1:1170,c(1,2,3,25,26,27,29,30,31,32,33,34,35,36)])
attyfinal <- atty[!(atty$Attrition == "No"),]




## Pull of Current employee data into new data frame
attn <- data.frame(trainingdata[1:1170,c(1,2,3,25,26,27,29,30,31,32,33,34,35,36)])
attnfinal <- attn[!(attn$Attrition == "Yes"),]



## Encoding of response variable since the response variable is a categorical variable
trainingdata = within(trainingdata, {
    Attrition1 = ifelse(Attrition == 'Yes', 1, 0)   
})
testdata = within(testdata, {
    Attrition1 = ifelse(Attrition == 'No', 1, 0)
})


### Encoding of explanatory variables

## Encoding Gender
trainingdata = within(trainingdata, {
    gender1 = ifelse(Gender == 'Female', 1, 0)   
})
testdata = within(testdata, {
    gender1 = ifelse(Gender == 'Female', 1, 0)
})
## Encoding Department
trainingdata$Department1[trainingdata$Department == 'Human Resources'] <- 1
trainingdata$Department1[trainingdata$Department == 'Research & Development']<- 2
trainingdata$Department1[trainingdata$Department == 'Sales'] <- 3

testdata$Department1[testdata$Department == 'Human Resources'] <- 1
testdata$Department1[testdata$Department == 'Research & Development']<- 2
testdata$Department1[testdata$Department == 'Sales'] <- 3


## Trends - Scatter Plots

plot(attnfinal$ID, attnfinal$YearsAtCompany, main="  Current Employees - Years at the Company", 
  	xlab="Employee", ylab="Employee's Years At Company", pch=20, col="Green")
```

![](CASEstudy2DS_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
plot(attnfinal$ID, attnfinal$Age, main="  Current Employees by Age", 
  	xlab="Employee by ID ", ylab="Curren Employees by Age", pch=20, col="orange")
```

![](CASEstudy2DS_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
plot(attyfinal$TotalWorkingYears, main="  Attrition - Total Working Years", 
  	xlab="Employees", ylab="Employee's Years At Company", pch=20, col="blue")
```

![](CASEstudy2DS_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
## Fitting model on the trainingset
Model1_fit = lm(Attrition1 ~ Age + YearsAtCompany + YearsSinceLastPromotion + MonthlyIncome + Department1, data=trainingdata)
summary(Model1_fit)
```

```
## 
## Call:
## lm(formula = Attrition1 ~ Age + YearsAtCompany + YearsSinceLastPromotion + 
##     MonthlyIncome + Department1, data = trainingdata)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.32079 -0.19809 -0.14950 -0.05443  1.11973 
## 
## Coefficients:
##                           Estimate Std. Error t value Pr(>|t|)   
## (Intercept)              2.078e-01  6.574e-02   3.162  0.00161 **
## Age                     -3.118e-03  1.331e-03  -2.343  0.01927 * 
## YearsAtCompany          -6.820e-03  2.338e-03  -2.917  0.00360 **
## YearsSinceLastPromotion  1.154e-02  4.138e-03   2.787  0.00540 **
## MonthlyIncome           -6.565e-06  2.864e-06  -2.292  0.02208 * 
## Department1              5.899e-02  2.032e-02   2.902  0.00377 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.3603 on 1164 degrees of freedom
## Multiple R-squared:  0.04221,	Adjusted R-squared:  0.03809 
## F-statistic: 10.26 on 5 and 1164 DF,  p-value: 1.224e-09
```

```r
Model1_res = resid(Model1_fit)

plot(trainingdata$Age, Model1_res, ylab="Residuals", xlab="Age", main="Age Residuals Plot", col="blue") 
abline(0, 0)                  # the horizontal line
```

![](CASEstudy2DS_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
## Predicting Attrition - CREATE THE TARGET OBJECTS
dfTrainData <- data.frame(trainingdata[1:1170,c(1,2,7,15,20,27,29,30,32,33,35,36,39,38)])
train_target <- as.factor(trainingdata[1:1170, 3])     ## target must be saved as a factor for knn function
length(train_target)
```

```
## [1] 1170
```

```r
dfTestData <- data.frame(testdata[1:300,c(1,2,7,15,20,27,29,30,32,33,35,36,39,38)])
test_target <- as.factor(testdata[1:300, 3])           ## target must be saved as a factor for knn function
length(test_target)
```

```
## [1] 300
```

```r
## build classification Models and confusion tables
dfTrainM1 <- knn(train=dfTrainData, test=dfTestData, cl=train_target, k=11)
## Comparisons models only
dfTrainM2 <- knn(train=dfTrainData, test=dfTestData, cl=train_target, k=5)
dfTrainM3 <- knn(train=dfTrainData, test=dfTestData, cl=train_target, k=8)
dfTrainM4 <- knn(train=dfTrainData, test=dfTestData, cl=train_target, k=9)
dfTrainM5 <- knn(train=dfTrainData, test=dfTestData, cl=train_target, k=10)
## Confusion Tables
table(test_target, dfTrainM1)
```

```
##            dfTrainM1
## test_target  No Yes
##         No  251   0
##         Yes  47   2
```

```r
confusionMatrix(dfTrainM1, test_target)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  No Yes
##        No  251  47
##        Yes   0   2
##                                           
##                Accuracy : 0.8433          
##                  95% CI : (0.7972, 0.8826)
##     No Information Rate : 0.8367          
##     P-Value [Acc > NIR] : 0.4139          
##                                           
##                   Kappa : 0.0665          
##  Mcnemar's Test P-Value : 1.949e-11       
##                                           
##             Sensitivity : 1.00000         
##             Specificity : 0.04082         
##          Pos Pred Value : 0.84228         
##          Neg Pred Value : 1.00000         
##              Prevalence : 0.83667         
##          Detection Rate : 0.83667         
##    Detection Prevalence : 0.99333         
##       Balanced Accuracy : 0.52041         
##                                           
##        'Positive' Class : No              
## 
```

```r
table(test_target, dfTrainM2)
```

```
##            dfTrainM2
## test_target  No Yes
##         No  232  19
##         Yes  41   8
```

```r
confusionMatrix(dfTrainM2, test_target)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  No Yes
##        No  232  41
##        Yes  19   8
##                                           
##                Accuracy : 0.8             
##                  95% CI : (0.7502, 0.8438)
##     No Information Rate : 0.8367          
##     P-Value [Acc > NIR] : 0.960924        
##                                           
##                   Kappa : 0.1069          
##  Mcnemar's Test P-Value : 0.006706        
##                                           
##             Sensitivity : 0.9243          
##             Specificity : 0.1633          
##          Pos Pred Value : 0.8498          
##          Neg Pred Value : 0.2963          
##              Prevalence : 0.8367          
##          Detection Rate : 0.7733          
##    Detection Prevalence : 0.9100          
##       Balanced Accuracy : 0.5438          
##                                           
##        'Positive' Class : No              
## 
```

```r
## comparison purpose only
table(test_target, dfTrainM2)
```

```
##            dfTrainM2
## test_target  No Yes
##         No  232  19
##         Yes  41   8
```

```r
table(test_target, dfTrainM3)
```

```
##            dfTrainM3
## test_target  No Yes
##         No  243   8
##         Yes  44   5
```

```r
table(test_target, dfTrainM4)
```

```
##            dfTrainM4
## test_target  No Yes
##         No  249   2
##         Yes  47   2
```

```r
table(test_target, dfTrainM5)
```

```
##            dfTrainM5
## test_target  No Yes
##         No  250   1
##         Yes  48   1
```

```r
## creation of dfTrain and dfVal data frame
###IMPORTANT NOTE ----I was not able to get rid of the NA so I had to use a reduced file####
dfTrain <- data.frame(trainingdata[1:1170,c(1,2,7,15,20,27,29,30,32,33,35,36,39,38)])
train_target <- as.factor(trainingdata[1:1170, 3])     ## target must be saved as a factor for knn function
length(train_target)
```

```
## [1] 1170
```

```r
dfVal <- data.frame(testdata[1:300,c(1,2,7,15,20,27,29,30,32,33,35,36,39,38)])
test_target <- as.factor(testdata[1:300, 3])           ## target must be saved as a factor for knn function
length(test_target)
```

```
## [1] 300
```

```r
dfPreds1 <- knn(train=dfTrain, test=dfVal, cl=train_target, k=11)
dfPreds1
```

```
##   [1] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
##  [18] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
##  [35] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
##  [52] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
##  [69] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
##  [86] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [103] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [120] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [137] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [154] No  No  No  No  No  No  Yes No  No  No  No  No  No  No  No  No  No 
## [171] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [188] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [205] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [222] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [239] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [256] No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No  No 
## [273] No  No  No  No  No  No  No  No  Yes No  No  No  No  No  No  No  No 
## [290] No  No  No  No  No  No  No  No  No  No  No 
## Levels: No Yes
```

```r
confusionMatrix(dfPreds1, test_target)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  No Yes
##        No  251  47
##        Yes   0   2
##                                           
##                Accuracy : 0.8433          
##                  95% CI : (0.7972, 0.8826)
##     No Information Rate : 0.8367          
##     P-Value [Acc > NIR] : 0.4139          
##                                           
##                   Kappa : 0.0665          
##  Mcnemar's Test P-Value : 1.949e-11       
##                                           
##             Sensitivity : 1.00000         
##             Specificity : 0.04082         
##          Pos Pred Value : 0.84228         
##          Neg Pred Value : 1.00000         
##              Prevalence : 0.83667         
##          Detection Rate : 0.83667         
##    Detection Prevalence : 0.99333         
##       Balanced Accuracy : 0.52041         
##                                           
##        'Positive' Class : No              
## 
```

```r
dfPreds <- data.frame(testdata$ID,dfPreds1)

write.csv(dfPreds, file="Case2PredictionsGarciadeAlford.csv")
```




