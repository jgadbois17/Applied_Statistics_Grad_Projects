#--------------------------------------------------------------------------------

# FUNCTIONS FOR TIME SERIES ANALYSIS AND FORECASTING 

#--------------------------------------------------------------------------------

# load libraries 
load_libraries = function(){
    library(tidyverse); library(tidyquant); library(timetk); 
    library(tseries); library(astsa); library(sweep); library(forecast); 
    library(ggfortify); 
}

# data 
load_weekly_data = function(path = 'salesweekly.csv', print = T){
    read_csv(path) %>% 
        mutate(datum = mdy(datum)) %>% 
        rename(date = datum) %>% 
        mutate(total = M01AB+M01AE+N02BA+N02BE+N05B+N05C+R03+R06, 
               M01 = M01AB+M01AE, N02 = N02BA+N02BE, 
               N05 = N05B+N05C, R0 = R03+R06) %>% 
        select(date, total, M01, N02, N05, R0) -> df 
    if (print){ df %>% print() } 
    return(df) 
}

# variable descriptions 
variable_descriptions = function(){
    cat('\nPharmaceutical Drug Sales Volumes', 
        '\n---------------------------------------------------------------', 
        '\ntotal: total drug sales volume', 
        '\nM01  : anti-inflammatory and antirheumatic products', 
        '\nN02  : other analgesics and antipyretics', 
        '\nN05  : psycholeptics drugs', 
        '\nR0   : antihistamines and drugs for obstructive airway diseases', 
        '\n---------------------------------------------------------------')
}



