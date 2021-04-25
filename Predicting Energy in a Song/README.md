# Modeling the Overall Energy in a Track: 
## Linear Regression Approach

# Data

The data were scraped from [Spotify's web API](https://developer.spotify.com/documentation/web-api/reference/tracks/get-audio-features/) (Application Programming Interface) using a cool feature where you can obtain the audio features from a track. 

# Research Questions

1. How can linear regression be implemented in order to understand what determines the overall energy in a track? 
2. What variable had the largest effect on the overall energy in a track?
3. Disregarding simplicity, are there any interactions in the data that contribute to understanding the overall energy in a track? If so, are they intuitive? 


# Methodology

* Linear regression 
* Variable Selection: Best Subsets Regression 
  * *Criteria considered:* 
  * adjusted coefficient of determination
  * Bayesian Information Criterion
  * Mean Squared Error 
  * Mallows' Cp Statistics 
* Influential point detections 
  * Leverages 
  * Studentized Residuals 
* Residual Analysis 
  * Residuals vs. Fit Plot 
  * Normal QQ Plot 
  * Shapiro-Wilk Test 
* ANOVA 
* Partial F-test 
* t-test 
* Likelihood Ratio Test 
* Confidence Intervals 

## R Implementation 

* Packages: 
  * tidyverse 
  * GGally 
  * ggpubr 
  * broom 
  * leap

# Results 

* Second-Order linear regression model with 8 interaction effects and 6 first-order predictors 
* Adjusted R2 = 0.7121
* Potential future work: 
  * higher order interactions 
  * preclassify genres and use separate linear regression models for different genres 
  * explore hierarchical relationships with more categorical variables 

