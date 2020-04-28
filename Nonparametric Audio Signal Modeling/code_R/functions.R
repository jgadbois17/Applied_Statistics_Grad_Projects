#--------------------------------------------------------------------------------

# STAT 560 PROJECT 
# MODELING THE PERIODIC VARIATIONS OF MUSIC SIGNALS 

# FUNCTIONS FOR PROJECT 

#--------------------------------------------------------------------------------

# load libraries 
load_libraries = function(){
  library(tuneR)
  library(tidyverse)
  library(broom)
  library(ggpubr)
  library(fANCOVA)
  library(fields)
}

# audio functions 
signal_load_process = function(path, start, stop){
  # loads signal from provided path of an audio file 
  # start is starting point of song in seconds 
  # stop is ending point inn seconds 
  # converts stereo signal to mono and normalizes it 
  audio = readWave(path, from = start, to = stop, units = 'seconds')
  ymono = mono(audio, which = 'both')
  ymono = normalize(ymono, unit = '1')
  str(ymono)
  return(ymono)
}
signal_subsamples = function(y, samp_length){
  # extracts signal array and splits it evenly by 
  # the length you provide 
  s = y@left
  ysplit = seq_along(s)
  samples = split(s, ceiling(ysplit/samp_length))
  return(samples)
}

# data functions 
data_signal = function(y){
  # creates dataset from signal consisting of 
  # y = sampled signal, n = discrete time index 
  df = tibble(n=1:length(y), y=y)
}
data_every_n = function(n, y){
  # creates new dataset that takes every nth 
  # data point in the signal dataset 
  n1 = n[seq(1, length(n), 5)]
  y1 = y[seq(1, length(y), 5)]
  df = tibble(n=n1, y=y1)
  df %>% print()
  return(df)
}

# functions to create plotting data 
df_preds_loess = function(fit, df_pred, conf_lev){
  # create dataset to plot the curve including values for 
  # predictions, confidence bands, standard errors, original signal 
  # fit: fitted curve 
  # data: df for predictions 
  # conf_lev: confidence level for intervals 
  alpha = 1-conf_lev 
  confidence = 1 - (alpha/2) 
  crit_val = qnorm(p = confidence)
  n = df_pred$n; y = df_pred$y
  pse = predict(fit, df_pred, se = T)
  p = pse$fit 
  s = pse$se.fit
  lb = p - crit_val * s 
  ub = p + crit_val * s
  df = data.frame(y=y, n=n, pred=p, se=s, low_cb=lb, upp_cb=ub)
  df = as_tibble(df)
  return(df)
}
df_preds_spline = function(fit, df_pred, conf_lev){
  # create dataset to plot the spline including values for 
  # predictions, confidence bands, standard errors, original signal
  # fit: fitted curve 
  # data: df for predictions 
  # conf_lev: confidence level for intervals 
  alpha = 1-conf_lev 
  confidence = 1 - (alpha/2) 
  crit_val = qnorm(p = confidence)
  
  n = df_pred$n; y = df_pred$y
  p = predict(fit, x = n) 
  s = predictSE(fit, x = n) 
  lb = p - crit_val * s 
  ub = p + crit_val * s 
  
  df = data.frame(y=y, n=n, pred=p, se=s, low_cb=lb, upp_cb=ub) 
  df = as_tibble(df)
  return(df)
}
curve_plot = function(df, type = c('loess', 'tp'), sub = ''){
  if (type == 'loess'){
    title = 'Loess Regression Curve'
  }else{
    title = 'Thin-Plate Smoothing Spline'
  }
  df %>% ggplot(aes(x=n)) + geom_point(aes(y=y), color='cyan') +
    geom_line(aes(y=pred), color='red') + 
    geom_line(aes(y=low_cb), color='red', lty=2) + 
    geom_line(aes(y=upp_cb), color='red', lty=2) +
    ggtitle(title, subtitle = sub) + theme_classic() +
    labs(x='Discrete Time Index', y='Amplitude')
}
curve_only_plot = function(df, type = c('loess', 'tp'), sub = ''){
  if (type == 'loess'){
    title = 'Loess Regression Curve'
  }else{
    title = 'Thin-Plate Smoothing Spline'
  }
  df %>% ggplot(aes(x=n)) + 
    geom_line(aes(y=pred), color='blue') + 
    geom_line(aes(y=low_cb), color='red', lty=2) + 
    geom_line(aes(y=upp_cb), color='red', lty=2) +
    ggtitle(title, subtitle = sub) + theme_classic() +
    labs(x='Discrete Time Index', y='Amplitude')
}

#--------------------------------------------------------------------------------









