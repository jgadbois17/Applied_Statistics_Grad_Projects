#--------------------------------------------------------------------------------

# TIME SERIES ANALYSIS AND FORECASTING
# PHARMACEUTICAL DRUG SALES VOLUME DATA | N02 

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
    ggplot(aes(x = date, y = N02)) + 
    geom_line(color = 'cyan', size = 1) + 
    geom_smooth(method = 'loess', color = 'red', fill = 'gray90') + 
    ggtitle('Pharmaceutical Drug Class: N02', subtitle = 'Weekly Sales Volume') + 
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
    tk_ts(select = N02, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 
has_timetk_idx(x) 

# dominant frequency (seasonal period) 
findfrequency(x) 

# plot time series with MA 
autoplot(x, ts.alpha = 0.5) + 
    autolayer(ma(x, 4), series = 'MA(4)', size = 1) + 
    autolayer(ma(x, 13), series = 'MA(13)', size = 1) + 
    ggtitle('Pharmaceutical Drug Class: N02') + 
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

#--------------------------------------------------------------------------------

# MODEL SELECTION 

#--------------------------------------------------------------------------------

# plot time series, ACF, and PACF 
diff(x) %>% ggtsdisplay(plot.type = 'partial', smooth = T, theme = theme_tq()) 

# model 1 
x %>% Arima(order = c(2, 1, 0), seasonal = c(1, 1, 0)) -> fit1 
fit1 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit1 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 2 
x %>% Arima(order = c(2, 1, 3), seasonal = c(1, 1, 0)) -> fit2 
fit2 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit2 %>% residuals() %>% adf.test(alternative = 'stationary') 

# model 3 
x %>% Arima(order = c(0, 1, 1), seasonal = c(1, 1, 0)) -> fit3 
fit3 %>% residuals() %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 
fit3 %>% residuals() %>% adf.test(alternative = 'stationary') 

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
x %>% Arima(order = c(2, 1, 3), seasonal = c(1, 1, 0)) -> fit 
fit %>% summary() 

# analyze residuals 
residuals(fit) %>% adf.test(alternative = 'stationary') 
residuals(fit) %>% 
    ggtsdisplay(plot.type = 'histogram', smooth = T, theme = theme_tq()) 

# forecast 
fit %>% forecast(h = 41) -> fc 
fc %>% accuracy(test$M01) 
sw_sweep(fc) %>% 
    ggplot(aes(x = index, y = N02, color = key)) + 
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = 'skyblue') + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = 'slateblue', alpha = 0.7) + 
    geom_line(size = 1) + 
    labs(title = 'N02 Validation Forecast', x = '', y = 'Volume', 
         subtitle = 'ARIMA(2, 1, 3)(1, 1, 0)[52]') + 
    scale_x_yearmon(n = 6, format = '%Y') + 
    theme_tq()

#--------------------------------------------------------------------------------

# FORECASTING FUTURE 2019 WEEKS 

#--------------------------------------------------------------------------------

# forecast future values 
df %>% 
    tk_ts(select = N02, start = 2014, frequency = 52, silent = T) %>% 
    tsclean() -> x 
x %>% Arima(order = c(2, 1, 3), seasonal = c(1, 1, 0)) -> fit 
fit %>% forecast(h = 11) -> fc_n02 

# plot forecast 
sw_sweep(fc_n02) %>% 
    ggplot(aes(x = index, y = N02, color = key)) + 
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), fill = 'skyblue') + 
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80), fill = 'slateblue', alpha = 0.7) + 
    geom_line(size = 1) + 
    labs(title = 'N02 Forecast: Last 11 Weeks of 2019', x = '', y = 'Volume', 
         subtitle = 'ARIMA(2, 1, 3)(1, 1, 0)[52]') + 
    scale_x_yearmon(n = 6, format = '%Y') + 
    theme_tq()

#--------------------------------------------------------------------------------



