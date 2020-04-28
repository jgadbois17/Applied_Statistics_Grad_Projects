#--------------------------------------------------------------------------------

# STAT 510 FINAL PROJECT 

#--------------------------------------------------------------------------------

# starter functions 
load_libraries = function(){
  # loads necessary libraries 
  library(tidyverse)
  library(GGally)
  library(ggpubr)
  library(broom)
  library(leaps)
}
load_and_process_data = function(path){
  # given a path, loads data and preprocesses variables 
  # this is specific to this dataset 
  data = read_csv(path)
  data = data %>% select(genre:duration)
  df = data %>% mutate(y = energy, 
                       c1 = factor(genre), c2 = factor(mode), 
                       x1 = tempo, x2 = danceability, x3 = loudness, 
                       x4 = speechiness, x5 = acousticness, 
                       x6 = liveness, x7 = instrumentalness, 
                       x8 = valence, x9 = duration/1000) 
  df = df %>% select(y:x9) 
  df = df %>% mutate(c1 = if_else(c1=='Alternative/Reggae', '1', 
                                  if_else(c1=='Country', '2', 
                                          if_else(c1=='Electronic Dance', '3',
                                                  if_else(c1=='Hip Hop', '4', '5')))))
  df = df %>% mutate(c1 = factor(c1)) %>% print()
  return(df)
}
var_map = function(){
  # reference map for what each variable is 
  cat('Response:  Y  = Energy\n\nCategorical Predictors:',
      '\n\nc1 = Genre   (1: Alt/Reg, 2: Country, 3: Electronic, 4: Hip Hop, 5: Pop)',
      '\nc2 = Mode\n\nNumeric Predictors:',
      '\n\nx1 = Tempo (BPMs)\nx2 = Danceability\nx3 = Loudness',
      '\nx4 = Speechiness\nx5 = Acousticness\nx6 = Liveness', 
      '\nx7 = Instrumentalness\nx8 = Valence\nx9 = Duration (s)')
}

# variable selection functions 
var_select_bestsubsets_comps = function(X, y, df){
  # performs best subsets regression 
  # creates dataset to easily compare models 
  # based on the values extracted 
  model_subsets = regsubsets(X, y, nvmax = ncol(df))
  mods = summary(model_subsets)
  rs = mods$rss 
  n = nrow(df)
  nsubsets = length(rs)
  mses = c()
  for (i in 1:nsubsets){
    mse = rs[i]/(n-i-1)
    mses = append(mses, mse)
  }
  mod_comps = tibble(nvars = 1:nsubsets, R2 = mods$rsq, Ajd_R2 = mods$adjr2,
                     RSS = rs, MSE = mses, BIC = mods$bic, Cp = mods$cp)
  mod_comps %>% print()
}
var_select_bestsubsets_predictors = function(X, y, df){
  # shows which variables are in model subsets 
  nv = ncol(df)
  model_subsets = regsubsets(X, y, nvmax = nv)
  mods = summary(model_subsets)
  mods$which
}

# assessment functions 
assess_res_fit = function(model){
  # Residuals vs. Fit Plot 
  augment(model) %>% ggplot(., aes(x=.fitted, y=.resid)) +
    geom_point(color = 'cyan') + geom_hline(yintercept = 0, color = 'red') +
    labs(title = 'Residuals vs. Fit Plot', x = 'Fitted Values', y = 'Residuals') +
    theme_dark() + theme(plot.title = element_text(hjust = 0.5))
}
assess_norm_qq = function(model){
  # Normal QQ Plot
  augment(model) %>% ggplot(., aes(sample = .resid)) +
    stat_qq(color = 'cyan') + stat_qq_line(color = 'red') + ggtitle('Normal QQ Plot') +
    theme_dark() + theme(plot.title = element_text(hjust = 0.5))
}
assess_outliers_leverage = function(model, df){
  # finds leverages and outliers 
  # returns dataframe with values to determine both 
  df$hv = hatvalues(model)
  df$rs = rstandard(model)
  cat('Leverage > 3*mean :', length(which(df$hv > (3*sum(df$hv)/nrow(df)))),
      '\nLeverage > 2*mean :', length(which(df$hv > (2*sum(df$hv)/nrow(df)))),
      '\nOutliers          :', length(which(abs(df$rs) > 3)), 
      '\n')
  return(df)
}

# testing and confidence intervals 
test_LRT = function(alpha, full, reduced){
  # conducts likelihood ratio test to determine better fitting model 
  k = length(full$coefficients) - length(reduced$coefficients)
  lr= -2*(logLik(reduced) - logLik(full))
  p = pchisq(lr, df = k, lower.tail = F)
  cat('\nLikelihood Ratio Test:',
      '\n\nNull: Smaller model has a better fit',
      '\nAlt : Larger model has a better fit',
      '\n\nTest Statistic:', lr, '\nDegrees of Freedom:', k)
  if (p < alpha){
    cat('\nThe p-value =', p, '<', alpha, '= alpha',
        '\n\nReject Null, choose larger model\n')
  }else{
    cat('\nThe p-value =', p, '>', alpha, '= alpha',
        '\n\nAccept Null, choose smaller model\n')
  }
}
test_shapiro = function(model){
  # Shapiro-Wilk Test for Normality 
  cat('\nShapiro-Wilk Test:\n\nNull: The variable is normally distributed',
      '\nAlt : The variable is not normally distributed\n')
  shapiro.test(augment(model)$.resid)
}
inf_confints = function(model, level = 0.95){
  coeffs = as.data.frame(model$coefficients)
  ci = tibble(Pred = rownames(coeffs), 
              Low = confint(model, level = level)[,1], 
              Est = model$coefficients,
              Up = confint(model, level = level)[,2])
  cat('\n\nConfidence Interval for Beta Coefficients:\n\n')
  ci %>% print(n = Inf)
}

#--------------------------------------------------------------------------------

# FIRST STEPS 

#--------------------------------------------------------------------------------

# load libraries and data
load_libraries()
data = load_and_process_data('audio_features.csv')
var_map()

# scatterplot correlation matrix
data %>% 
  select(y, x1:x9) %>%
  ggpairs(title = 'Scatterplot Correlation Matrix', progress = F) + 
  theme_classic()

# genre means 
data %>% 
  group_by(c1) %>%
  summarise(mY = mean(y), 
            mX1 = mean(x1), mX2 = mean(x2), mX3 = mean(x3),
            mX4 = mean(x4), mX5 = mean(x5), mX6 = mean(x6), 
            mX7 = mean(x7), mX8 = mean(x8), mX9 = mean(x9))

#--------------------------------------------------------------------------------

# BUILDING INITIAL MODEL 

#--------------------------------------------------------------------------------

# best subsets regression
attach(data)
X = cbind(c1, c2, x1, x2, x3, x4, x5, x6, x7, x8, x9)
var_select_bestsubsets_comps(X, y, data)
var_select_bestsubsets_predictors(X, y, data)
detach(data)

# initial model selected from best subsets 
fit = lm(y ~ c1 + x1 + x2 + x3 + x5 + x6 + x7 + x8, data = data)
summary(fit)

# inflential point detection | leverages and studentized residuals 
df = assess_outliers_leverage(fit, data) 
df = df %>% filter(hv < 2*sum(hv)/nrow(df), abs(rs) < 3) 
df = df %>% select(y, c1, x1:x8) %>% print() 

# repeat best subsets 
attach(df)
X = cbind(c1, x1, x2, x3, x4, x5, x6, x7, x8)
var_select_bestsubsets_comps(X, y, df)
var_select_bestsubsets_predictors(X, y, df)
detach(df)

# initial model selected from repeated best subsets 
fit = lm(y ~ c1 + x1 + x2 + x3 + x4 + x5 + x6 + x8, data = df)
summary(fit)

# residual analysis 
assess_res_fit(fit)
assess_norm_qq(fit)
test_shapiro(fit)
assess_outliers_leverage(fit, df) %>% filter(hv > 3*sum(hv)/nrow(df))

# scatterplot of residuals vs. x7 
df %>% ggplot(aes(x = x7, y = augment(fit)$.resid)) +
  geom_point(color = 'blue') + geom_hline(yintercept = 0, color = 'red') +
  labs(title='Residuals vs. x7', x='x7', y='Residuals') + theme_classic()

# confidence interval 
inf_confints(fit, 0.95)

# reduce model 
rfit = lm(y ~ c1 + x1 + x2 + x3 + x5 + x8, data = df)
summary(rfit)
anova(rfit)
test_LRT(0.05, fit, rfit)
anova(rfit, fit)

assess_res_fit(rfit)
assess_norm_qq(rfit)
test_shapiro(rfit)

#--------------------------------------------------------------------------------

# FIRST-ORDER MODEL 

#--------------------------------------------------------------------------------

# model 
model = lm(y ~ c1 + x1 + x2 + x3 + x5 + x8, data = df)
summary(model)
anova(model)

# residual analysis 
p1 = assess_res_fit(model)
p2 = assess_norm_qq(model)
ggarrange(p1, p2, nrow = 1, ncol = 2)
test_shapiro(model)

# confidence intervals 
inf_confints(model, 0.95)

# explore which data points have the largest errors and why
augment(model)
augment(model) %>% filter(.resid > 0.15) 
augment(model) %>%
  group_by(c1) %>% 
  summarise(n = n(), mY = mean(y), mPr = mean(.fitted),
            mX1 = mean(x1), mX2 = mean(x2), mX3 = mean(x3), 
            mX4 = mean(x4), mX5 = mean(x5), mX8 = mean(x8))
augment(model) %>%  
  filter(.resid > 0.1) %>% 
  group_by(c1) %>% 
  summarise(n = n(), mY = mean(y), mPred = mean(.fitted),
            mX1 = mean(x1), mX2 = mean(x2), mX3 = mean(x3), 
            mX4 = mean(x4), mX5 = mean(x5), mX8 = mean(x8))
augment(model) %>% 
  filter(.resid > 0.15) %>% 
  group_by(c1) %>% 
  summarise(n = n(), mY = mean(y), mPred = mean(.fitted),
            mX1 = mean(x1), mX2 = mean(x2), mX3 = mean(x3), 
            mX4 = mean(x4), mX5 = mean(x5), mX8 = mean(x8))

#--------------------------------------------------------------------------------

# F-tests for Finding Variable with Largest Effect 

#--------------------------------------------------------------------------------

# leave out c1
reg_form = y ~ x1 + x2 + x3 + x5 + x8
anova(lm(reg_form, data = df), model)

# leave out x1
reg_form = y ~ c1 + x2 + x3 + x5 + x8
anova(lm(reg_form, data = df), model)

# leave out x2
reg_form = y ~ c1 + x1 + x3 + x5 + x8
anova(lm(reg_form, data = df), model)

# leave out x3
reg_form = y ~ c1 + x1 + x2 + x5 + x8
anova(lm(reg_form, data = df), model)

# leave out x5
reg_form = y ~ c1 + x1 + x2 + x3 + x8
anova(lm(reg_form, data = df), model)

# leave out x8
reg_form = y ~ c1 + x1 + x2 + x3 + x5
anova(lm(reg_form, data = df), model)

#--------------------------------------------------------------------------------

# INTERACTION EFFECTS 

#--------------------------------------------------------------------------------

# list to hold models 
ifit = list()

# reference 
summary(fit)
var_map()

# screening interactions round 1
add1(fit, ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6 + c1*x8, test = 'F')
add1(fit, ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(fit, ~.+ x2*c1 + x2*x1 + x2*x3 + x2*x4 + x2*x5 + x2*x6 + x2*x8, test = 'F')
add1(fit, ~.+ x3*c1 + x3*x1 + x3*x2 + x3*x4 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(fit, ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x3 + x4*x5 + x4*x6 + x4*x8, test = 'F')
add1(fit, ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x4 + x5*x6 + x5*x8, test = 'F')
add1(fit, ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(fit, ~.+ x8*c1 + x8*x1 + x8*x2 + x8*x3 + x8*x4 + x8*x5 + x8*x6, test = 'F')

# update 1
ifit = append(ifit, list(update(fit, ~.+ x4*x5)))
summary(ifit[[1]])
anova(ifit[[1]])

# screening round 2 
add1(ifit[[1]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6 + c1*x8, test = 'F')
add1(ifit[[1]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[1]], ~.+ x2*c1 + x2*x1 + x2*x3 + x2*x4 + x2*x5 + x2*x6 + x2*x8, test = 'F')
add1(ifit[[1]], ~.+ x3*c1 + x3*x1 + x3*x2 + x3*x4 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[1]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x3 + x4*x6 + x4*x8, test = 'F')
add1(ifit[[1]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6 + x5*x8, test = 'F')
add1(ifit[[1]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[1]], ~.+ x8*c1 + x8*x1 + x8*x2 + x8*x3 + x8*x4 + x8*x5 + x8*x6, test = 'F')

# update 2
ifit = append(ifit, list(update(ifit[[1]], ~.+ x2*x8)))
summary(ifit[[2]])
anova(ifit[[2]])

# screening round 3
add1(ifit[[2]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6 + c1*x8, test = 'F')
add1(ifit[[2]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[2]], ~.+ x2*c1 + x2*x1 + x2*x3 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[2]], ~.+ x3*c1 + x3*x1 + x3*x2 + x3*x4 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[2]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x3 + x4*x6 + x4*x8, test = 'F')
add1(ifit[[2]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6 + x5*x8, test = 'F')
add1(ifit[[2]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[2]], ~.+ x8*c1 + x8*x1 + x8*x3 + x8*x4 + x8*x5 + x8*x6, test = 'F')

# update 3
ifit = append(ifit, list(update(ifit[[2]], ~.+ x2*x3)))
summary(ifit[[3]])
anova(ifit[[3]])

# screening round 4
add1(ifit[[3]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6 + c1*x8, test = 'F')
add1(ifit[[3]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[3]], ~.+ x2*c1 + x2*x1 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[3]], ~.+ x3*c1 + x3*x1 + x3*x4 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[3]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x3 + x4*x6 + x4*x8, test = 'F')
add1(ifit[[3]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6 + x5*x8, test = 'F')
add1(ifit[[3]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[3]], ~.+ x8*c1 + x8*x1 + x8*x3 + x8*x4 + x8*x5 + x8*x6, test = 'F')

# update 4
ifit = append(ifit, list(update(ifit[[3]], ~.+ x3*x4)))
summary(ifit[[4]])
anova(ifit[[4]])

# screening round 5
add1(ifit[[4]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6 + c1*x8, test = 'F')
add1(ifit[[4]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[4]], ~.+ x2*c1 + x2*x1 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[4]], ~.+ x3*c1 + x3*x1 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[4]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x6 + x4*x8, test = 'F')
add1(ifit[[4]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6 + x5*x8, test = 'F')
add1(ifit[[4]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[4]], ~.+ x8*c1 + x8*x1 + x8*x3 + x8*x4 + x8*x5 + x8*x6, test = 'F')

# update 5
ifit = append(ifit, list(update(ifit[[4]], ~.+ x5*x8)))
summary(ifit[[5]])
anova(ifit[[5]])

# screening round 6
add1(ifit[[5]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6 + c1*x8, test = 'F')
add1(ifit[[5]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[5]], ~.+ x2*c1 + x2*x1 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[5]], ~.+ x3*c1 + x3*x1 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[5]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x6 + x4*x8, test = 'F')
add1(ifit[[5]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6, test = 'F')
add1(ifit[[5]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[5]], ~.+ x8*c1 + x8*x1 + x8*x3 + x8*x4 + x8*x6, test = 'F')

# update 6
ifit = append(ifit, list(update(ifit[[5]], ~.+ c1*x8)))
summary(ifit[[6]])
anova(ifit[[6]])

# screening round 7
add1(ifit[[6]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6, test = 'F')
add1(ifit[[6]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[6]], ~.+ x2*c1 + x2*x1 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[6]], ~.+ x3*c1 + x3*x1 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[6]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x6 + x4*x8, test = 'F')
add1(ifit[[6]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6, test = 'F')
add1(ifit[[6]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[6]], ~.+ x8*x1 + x8*x3 + x8*x4 + x8*x6, test = 'F')

# update 7
ifit = append(ifit, list(update(ifit[[6]], ~.+ x4*x8)))
summary(ifit[[7]])
anova(ifit[[7]])

# screening round 8
add1(ifit[[7]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x4 + c1*x5 + c1*x6, test = 'F')
add1(ifit[[7]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[7]], ~.+ x2*c1 + x2*x1 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[7]], ~.+ x3*c1 + x3*x1 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[7]], ~.+ x4*c1 + x4*x1 + x4*x2 + x4*x6, test = 'F')
add1(ifit[[7]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6, test = 'F')
add1(ifit[[7]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[7]], ~.+ x8*x1 + x8*x3 + x8*x6, test = 'F')

# update 8
ifit = append(ifit, list(update(ifit[[7]], ~.+ c1*x4)))
summary(ifit[[8]])
anova(ifit[[8]])

# screening round 9
add1(ifit[[8]], ~.+ c1*x1 + c1*x2 + c1*x3 + c1*x5 + c1*x6, test = 'F')
add1(ifit[[8]], ~.+ x1*c1 + x1*x2 + x1*x3 + x1*x4 + x1*x5 + x1*x6 + x1*x8, test = 'F')
add1(ifit[[8]], ~.+ x2*c1 + x2*x1 + x2*x4 + x2*x5 + x2*x6, test = 'F')
add1(ifit[[8]], ~.+ x3*c1 + x3*x1 + x3*x5 + x3*x6 + x3*x8, test = 'F')
add1(ifit[[8]], ~.+ x4*x1 + x4*x2 + x4*x6, test = 'F')
add1(ifit[[8]], ~.+ x5*c1 + x5*x1 + x5*x2 + x5*x3 + x5*x6, test = 'F')
add1(ifit[[8]], ~.+ x6*c1 + x6*x1 + x6*x2 + x6*x3 + x6*x4 + x6*x5 + x6*x8, test = 'F')
add1(ifit[[8]], ~.+ x8*x1 + x8*x3 + x8*x6, test = 'F')

# reduce model 
rmod = lm(y ~ c1 + x2 + x3 + x4 + x5 + x8 + x4:x5 + x2:x8 + x2:x3 + 
              x3:x4 + x5:x8 + c1:x8 + x4:x8 + c1:x4, data = df)
anova(rmod, ifit[[8]])
summary(rmod)

#--------------------------------------------------------------------------------

# FINAL MODEL WITH INTERACTION TERMS 

#--------------------------------------------------------------------------------

# final model
model = lm(y ~ c1 + x2 + x3 + x4 + x5 + x8 + x4:x5 + x2:x8 + x2:x3 + 
               x3:x4 + x5:x8 + c1:x8 + x4:x8 + c1:x4, data = df)
summary(model)

# residual analysis 
p1 = assess_res_fit(model)
p2 = assess_norm_qq(model)
ggarrange(p1, p2, nrow = 1, ncol = 2)
test_shapiro(model)

df %>% ggplot(aes(x=x6, y=augment(model)$.resid)) + 
  geom_point(color='blue') + geom_hline(yintercept = 0, color='red') +
  ggtitle('Residuals vs. x6') + labs(x='x6', y='Residuals') + theme_classic()

df %>% ggplot(aes(x=x6*x1, y=augment(model)$.resid)) + 
  geom_point(color='blue') + geom_hline(yintercept = 0, color='red') +
  ggtitle('Residuals vs. x6*x1') + labs(x='x6*x1', y='Residuals') + theme_classic()

# confidence intervals 
inf_confints(model, 0.95)

# comparing prediction vs confidence intervals
predict(model, interval = 'confidence')[1:10,]
predict(model, interval = 'prediction')[1:10,]

# inspecting errors 
augment(model)
augment(model) %>% filter(.resid > 0.15) 
augment(model) %>% 
  group_by(c1) %>% 
  summarise(n = n(), mY = mean(y), mPr = mean(.fitted),
            mX2 = mean(x2), mX3 = mean(x3), mX4 = mean(x4), 
            mX5 = mean(x5), mX8 = mean(x8))
augment(model) %>%  
  filter(.resid > 0.1) %>% 
  group_by(c1) %>% 
  summarise(n = n(), mY = mean(y), mPr = mean(.fitted),
            mX2 = mean(x2), mX3 = mean(x3), mX4 = mean(x4), 
            mX5 = mean(x5), mX8 = mean(x8))
augment(model) %>% 
  filter(.resid > 0.15) %>% 
  group_by(c1) %>% 
  summarise(n = n(), mY = mean(y), mPr = mean(.fitted),
            mX2 = mean(x2), mX3 = mean(x3), mX4 = mean(x4), 
            mX5 = mean(x5), mX8 = mean(x8))

#--------------------------------------------------------------------------------




















