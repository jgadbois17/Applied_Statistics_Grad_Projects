#--------------------------------------------------------------------------------

# STAT 510 FINAL PROJECT 

#--------------------------------------------------------------------------------

# functions used
load_libraries = function(){
  # loads necessary libraries 
  library(tidyverse)
  library(GGally)
  library(ggpubr)
  library(broom)
  library(leaps)
  library(nnet)
  library(caret)
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

var_select_explore = function(data, reg_eq){
  # provides summary of linear regression 
  # given a dataset and regression formula 
  model = lm(reg_eq, data = data)
  summary(model)
}
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
inf_confints = function(model, level){
  coeffs = as.data.frame(model$coefficients)
  ci = tibble(Pred = rownames(coeffs), 
              Low = confint(model, level = level)[,1], 
              Est = model$coefficients,
              Up = confint(model, level = level)[,2])
  cat('\n\nConfidence Interval for Beta Coefficients:\n\n')
  ci %>% print(n = Inf)
}

#--------------------------------------------------------------------------------

train_test_split = function(data, train_size, seed = 7){
  test_size = 1-train_size
  psplits = c(train_size, test_size)
  set.seed(seed)
  pd = sample(2, nrow(data), replace = T, prob = psplits)
  df_split = list()
  train = data[pd == 1,]
  test = data[pd == 2,]
  df_split = append(df_split, list(train))
  df_split = append(df_split, list(test))
  return(df_split)
}

load_and_process_data = function(path){
  # given a path, loads data and preprocesses variables 
  # this is specific to this dataset 
  data = read_csv(path)
  data = data %>% select(genre:duration)
  df = data %>% mutate(y = energy*100, 
                       c1 = factor(genre), c2 = factor(mode), 
                       x1 = tempo, x2 = danceability*100, x3 = loudness, 
                       x4 = speechiness*100, x5 = acousticness*100, 
                       x6 = liveness*100, x7 = instrumentalness*100, 
                       x8 = valence*100, x9 = duration/1000) 
  df = df %>% select(y:x9) 
  df = df %>% mutate(c1 = if_else(c1=='Alternative/Reggae', '1', 
                                  if_else(c1=='Country', '2', 
                                          if_else(c1=='Electronic Dance', '3',
                                                  if_else(c1=='Hip Hop', '4', '5')))))
  df = df %>% mutate(c1 = factor(c1)) %>% print()
  return(df)
}

var_select_stepwise_lm = function(df, reg_eq){
  # performs stepwise selection given a regression equation 
  # of all possible variables 
  # shows summary and returns final model 
  fit0 = lm(y ~ 1, data = df)
  fitu = lm(formula = reg_eq, data = df)
  step = step(fit0, scope = list(lower = fit0, upper = fitu))
  summary(step)
  return(step)
}
var_select_stepwise_glm = function(df, response, reg_eq, family){
  fit0 = glm(y ~ 1, data = df, family = family)
  fit1 = glm(reg_eq, data = df, family = family)
  step = step(fit, scope = list(lower = fot0, upper = fit1), direction = 'both')
  summary(step)
  return(step)
}

test_wald = function(clf){
  # estimate significance for multinomial regression
  zvals = summary(clf)
  z = zvals$coefficients/zvals$standard.errors
  p = (1 - pnorm(abs(z), 0, 1))*2
  print(round(p, digits = 5))
}

xy_transform_summary = function(formula, df){
  xtr_fit = lm(formula, data = df)
  summary(xtr_fit)
}
xy_transform_res_fit = function(formula, df){
  xtr_fit = lm(formula, data = df)
  assess_res_fit(xtr_fit)
}
xy_transform_qq = function(formula, df){
  xtr_fit = lm(formula, data = df)
  assess_norm_qq(xtr_fit)
}
xy_transform_shapiro = function(formula, df){
  xtr_fit = lm(formula, data = df)
  test_shapiro(xtr_fit)
}

#--------------------------------------------------------------------------------



















