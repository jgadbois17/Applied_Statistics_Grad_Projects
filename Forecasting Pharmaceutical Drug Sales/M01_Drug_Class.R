#--------------------------------------------------------------------------------

# TIME SERIES ANALYSIS AND FORECASTING
# PHARMACEUTICAL DRUG SALES VOLUME DATA | M01 

#--------------------------------------------------------------------------------

# libraries 
load_libraries() 

# load data 
variable_descriptions() 
load_weekly_data() -> df 

# date range 
df %>% summarise(first_date = min(date), last_date = max(date)) 

# plot time series 
df %>% 
    ggplot(aes(x = date, y = M01)) + 
    geom_line(color = 'cyan', size = 1) + 
    geom_smooth(method = 'loess', color = 'red', fill = 'gray90') + 
    ggtitle('Pharmaceutical Drug Class: M01', subtitle = 'Weekly Sales Volume') + 
    labs(x = '', y = 'Volume') + 
    scale_x_date(date_breaks = '1 year', date_labels = '%Y') + 
    theme_tq_dark() 

# split data 
df %>% filter(date < '2019-01-06') -> train 
df %>% filter(date >= '2019-01-06') -> test 

#--------------------------------------------------------------------------------

# EXPLORATORY DATA ANALYSIS 

#--------------------------------------------------------------------------------

# create time series object 
train %>% 
    tk_ts(select = M01, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 
has_timetk_idx(x) 

# dominant frequency (seasonal period) 
findfrequency(x) 

# plot time series with MA 
autoplot(x, ts.alpha = 0.5) + 
    autolayer(ma(x, 4), series = 'MA(4)', size = 1) + 
    autolayer(ma(x, 13), series = 'MA(13)', size = 1) + 
    ggtitle('Pharmaceutical Drug Class: M01') + 
    labs(x = '', y = 'volume', subtitle = 'Weekly Sales with Moving Averages') + 
    scale_x_yearmon(n = 6, format = '%Y') + 
    theme_tq() 

# plot sample ACF and PACF 
acf2(x, plot = T, main = '') 

# ADF test 
adf.test(x, alternative = 'stationary') 

# estimate difference orders for the series to be stationary 
ndiffs(x) 
nsdiffs(x) 

# plot sample ACF and PACF of first difference 
acf2(diff(x), plot = T, main = '') 

# ADF test of first difference 
adf.test(diff(x), alternative = 'stationary') 

#--------------------------------------------------------------------------------

# MODEL SELECTION 

#--------------------------------------------------------------------------------

# plot time series, ACF, and PACF 
diff(x) %>% ggtsdisplay(plot.type = 'partial', smooth = T, theme = theme_tq()) 

# model 1 
x %>% Arima(order = c(4, 1, 1)) -> fit1 
fit1 %>% residuals() %>% adf.test(alternative = 'stationary') 
fit1 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# model 2 
x %>% Arima(order = c(3, 1, 1), seasonal = c(0, 0, 0)) -> fit2 
fit2 %>% residuals() %>% adf.test(alternative = 'stationary') 
fit2 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# model 3 
x %>% Arima(order = c(2, 1, 1)) -> fit3 
fit3 %>% residuals() %>% adf.test(alternative = 'stationary') 
fit3 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# select model - glance 
fit1 %>% sw_glance() 
fit2 %>% sw_glance() 
fit3 %>% sw_glance() 

# select model - summary 
fit1 %>% summary() 
fit2 %>% summary() 
fit3 %>% summary() 

#--------------------------------------------------------------------------------

# PREDICT AND EVALUATE FORECAST 

#--------------------------------------------------------------------------------

# define model 
x %>% Arima(order = c(2, 1, 1)) -> fit 
fit %>% summary() 

# analyze residuals 
residuals(fit) %>% adf.test(alternative = 'stationary') 
residuals(fit) %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# forecast 
fit %>% forecast(h = 41) -> fc 
fc %>% accuracy(test$M01) 
sw_sweep(fc) %>% 
    ggplot(aes(x = index, y = M01, color = key)) + 
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = 'skyblue') + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = 'slateblue', alpha = 0.7) + 
    geom_line(size = 1) + 
    labs(title = 'M01 Validation Forecast', x = '', y = 'Volume', 
         subtitle = 'ARIMA(2, 1, 1)') + 
    scale_x_yearmon(n = 6, format = '%Y') + 
    theme_tq()

#--------------------------------------------------------------------------------

# FORECASTING FUTURE 2019 WEEKS 

#--------------------------------------------------------------------------------

# forecast future values 
df %>% 
    tk_ts(select = M01, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 
x %>% Arima(order = c(2, 1, 1)) -> fit 
fit %>% forecast(h = 11) -> fc_m01 

# plot forecast 
sw_sweep(fc_m01) %>% 
    ggplot(aes(x = index, y = M01, color = key)) + 
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = 'skyblue') + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = 'slateblue', alpha = 0.7) + 
    geom_line(size = 1) + 
    labs(title = 'M01 Forecast: Last 11 Weeks of 2019', x = '', y = 'Volume', 
         subtitle = 'ARIMA(2, 1, 1)') + 
    scale_x_yearmon(n = 6, format = '%Y') + 
    theme_tq() 

#--------------------------------------------------------------------------------








