#--------------------------------------------------------------------------------

# STAT 560 PROJECT 
# MODELING THE PERIODIC VARIATIONS OF MUSIC SIGNALS 
# LOESS REGRESSION AND THIN-PLATE SMOOTHING SPLINES 

#--------------------------------------------------------------------------------

# set up 
load_libraries()
xn = signal_load_process('results/Closer.wav', start = 70, stop = 71)

# visualizing signal
plot(xn, main='Waveform of Signal', xlab='Time (Seconds)', ylab='Amplitude')
plot(extractWave(xn, from = 1, to = 2205), main = 'Subsample of Signal: n = 2205',
     xunit = 'samples', xlab = 'Time Index', ylab = 'Amplitude')
plot(extractWave(xn, from = 1, to = 1000), main = 'Subsample of Signal: n = 1000',
     xunit = 'samples', xlab = 'Time Index', ylab = 'Amplitude')

# subsamples and data 
xsamps = signal_subsamples(xn, 2205)
xdf_samps = data_signal(xsamps[[1]]) %>% print()
xdf = data_every_n(xdf_samps$n, xdf_samps$y)

# scatter plot 
xdf %>% ggplot(aes(x=n, y=y)) + geom_point(color='cyan') +
  ggtitle('Scatterplot of Subsampled Signal') +
  labs(x='Discrete Time Index', y='Amplitude') +
  theme_dark()

#--------------------------------------------------------------------------------

# Loess Regression Degree 1
sm1 = list()
dl1 = list()
p1 = list()
subs1 = list('Smoothing Parameter = 0.01 | Deg. 1',
             'Smoothing Parameter = 0.02 | Deg. 1',
             'Smoothing Parameter = 0.03 | Deg. 1',
             'Smoothing Parameter = 0.04 | Deg. 1')

for (i in 1:4){
  span = i/100
  s = loess(y ~ n, data = xdf, span = span, degree = 1, family = 'gaussian')
  sm1 = append(sm1, list(s))
  d = df_preds_loess(s, df_pred = xdf_samps, conf_lev = 0.95)
  dl1 = append(dl1, list(d))
  p = curve_plot(d, type = 'loess', sub = subs1[[i]])
  p1 = append(p1, list(p))
}

ggarrange(p1[[1]], p1[[2]], p1[[3]], p1[[4]], nrow = 2, ncol = 2)

#--------------------------------------------------------------------------------

# Loess Regression Degree 2
sm2 = list()
dl2 = list()
p2 = list()
subs2 = list('Smoothing Parameter = 0.01 | Deg. 2',
             'Smoothing Parameter = 0.02 | Deg. 2',
             'Smoothing Parameter = 0.03 | Deg. 2',
             'Smoothing Parameter = 0.04 | Deg. 2')

for (i in 1:4){
  span = i/100
  s = loess(y ~ n, data = xdf, span = span, degree = 2, family = 'gaussian')
  sm2 = append(sm2, list(s))
  d = df_preds_loess(s, df_pred = xdf_samps, conf_lev = 0.95)
  dl2 = append(dl2, list(d))
  p = curve_plot(d, type = 'loess', sub = subs2[[i]])
  p2 = append(p2, list(p))
}

ggarrange(p2[[1]], p2[[2]], p2[[3]], p2[[4]], nrow = 2, ncol = 2)

#--------------------------------------------------------------------------------

# Thin-Plate Splines
attach(xdf)
tpm = list()
dtp = list()
p3 = list()
subs3 = list('Degree of Smoothness: M = 1',
             'Degree of Smoothness: M = 2',
             'Degree of Smoothness: M = 3',
             'Degree of Smoothness: M = 4')
for (i in 1:4){
  m = Tps(x=n, Y=y, m=(i))
  tpm = append(tpm, list(m))
  d = df_preds_spline(m, xdf_samps, 0.95)
  dtp = append(dtp, list(d))
  p = curve_plot(dtp[[i]], type = 'tp', sub = subs3[[i]])
  p3 = append(p3, list(p))
}

ggarrange(p3[[1]], p3[[2]], p3[[3]], p3[[4]], nrow = 2, ncol = 2)

#--------------------------------------------------------------------------------

# comparisons 
curve_only_plot(dl1[[1]], type = 'loess', sub = subs1[[1]])
curve_only_plot(dl2[[1]], type = 'loess', sub = subs2[[1]])
curve_only_plot(dtp[[2]], type = 'tp', sub = subs3[[2]])
curve_only_plot(dtp[[1]], type = 'tp', sub = subs3[[1]])
plot(extractWave(xn, from = 1, to = 2205), main = 'Subsample of Signal: n = 2205',
     xunit = 'samples', xlab = 'Time Index', ylab = 'Amplitude')

#--------------------------------------------------------------------------------














