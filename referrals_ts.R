library(tidyverse)
library(readxl)
library(tibble)
library(ggplot2)
library(lubridate)
library(reshape2)
library(forcats)
library(forecast)



referrals  %>% 
mutate(mo = dplyr::case_when(
        month == 1 ~ "January",
        month == 2 ~ "February",
        month == 3 ~ "March",
        month == 4 ~ "April",
        month == 5 ~ "May",
        month == 6 ~ "June",
        month == 7 ~ "July",
        month == 8 ~ "August",
        month == 9 ~ "September",
        month == 10 ~ "October",
        month == 11 ~ "November",
        month == 12 ~ "December"
  ))  
  
  refer_ts1<-referrals %>% 
    group_by(year, month)  %>% 
    count()
    
# add missing counts
refer_ts1 <- refer_ts1  %>% 
    add_row(year = 1997, month = 12, n = 0)  %>% 
    add_row(year = 2008, month = 9, n = 0)  %>% 
    add_row(year = 2010, month = 5, n = 0)  %>% 
    add_row(year = 2012, month = 2, n = 0) %>% 
    arrange(year, month)

# check for missing:
refer_ts2  %>% 
    ggplot(aes(x = factor(year), y = factor(month), fill = n))   + 
    geom_tile()
    
    
referralsts <- ts(refer_ts2["n"], start = c(1997, 1), frequency = 12)

autoplot(referralsts)

autoplot(referralsts)  + 
    #scale_x_continuous(limits = c(2010,2019),
    #                  breaks = seq(2010, 2019, by = 1)) + 
    geom_smooth(span = 0.25) + 
    labs(title = "Referrals Longitudinally",
        subtitle = "Referrals spiked in 2000 but have leveled off to average of 5 per month.") + 
    theme_light()


ggseasonplot(referralsts, continuous = TRUE, polar = TRUE) + 
    theme_light() + 
    labs(title = "There is no Cyclical Pattern of Referrals to TKS",
        subtitle = "However, referral numbers have stabilized over time 
to an average of 5 referrals per month") + 
scale_color_gradient(high = "#d52b1e", low = "#6caddf") 
