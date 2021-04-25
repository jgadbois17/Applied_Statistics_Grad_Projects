
# Predicting the Outcome of UFC Fights

## Multivariate Statistical Analysis 

This was a group project inspired by [this article](https://medium.com/@yuan_tian/predict-ufc-fights-with-deep-learning-ii-data-collection-and-implementation-in-pytorch-ff7a95062554) I found on the Medium. The data were scraped from [UFC FightMetric](http://ufcstats.com/statistics/events/completed) by the author of the post, Yuan Tian. Since he used neural networks, we completely recreated the project using multivariate statistical techniques. 

**My Contributions to the Project:** 

* Found the article and dataset 
* Helped come up with the idea on how we preprocessed the data 
* All of the SAS programming 
* Wrote the majority of the final report 


# Methodology and SAS Procedures Implemented

## Multivariate Normal Distribution 

* proc corr 
* proc sgscatter
* proc iml
* proc sort 
* proc gplot 


## Multivariate Analysis of Variance 

* proc freq
* proc corr 
* proc glm 


## Principal Component Analysis 

* proc princomp 
* proc gplot 
* proc corr


## Linear Discriminant Analysis 

* proc sql 
* proc discrim 


# Project Procedures 

## Preliminary Analysis 

1. Explore Summary Statistics 
2. Assessing the Assumption of Multivariate Normality 
3. Multivariate Analysis of Variance 


## Analysis 

1. Principal Component Analysis 
2. Linear Discriminant Analysis 
   * Fisher's Linear Discriminant Function


# Results 

The data originally had 12 independent variables. After performing principal component analysis we significantly reduced the dimensionality of the data. The first three principal components were used as inputs to linear discriminant analysis. We then carried out Fisher's Linear Discriminant Analysis since the test on covariance matrices resulted in equality. We trained the classifier on fights from 2008 to 2017 and predicted fights from 2018. The model correctly predicted the outcome of fights with about 73.07% accuracy. 

