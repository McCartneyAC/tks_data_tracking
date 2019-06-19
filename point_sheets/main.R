library(tidyverse)
getwd()
library(readxl)
library(lubridate)
library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
points<-read_xlsx("points_full.xlsx")
names(points)
points  %>% 
  mutate(day = factor(day))  %>% 
  mutate(week = factor(week)) %>% 
  select(date, day,week, meanR, meanT, meanF, mean, name) %>% 
  melt(id.vars = c("date", "day", "week", "name"))  %>%
  filter(variable != "mean") %>% 
  ggplot(aes(x = date, y = value, color = variable))  +
  geom_jitter(alpha = 0.4, width = 0.25, height = 0.25) +
  geom_smooth() +
  scale_color_inova() + 
  facet_wrap(~name) +
  theme_light() +
  labs(title= "Trajectory of all points", 
       subtitle = "2018-2019 Academic Year",
       y = "Average Daily Points"
  ) + 
  scale_y_continuous(limits = c(0.0, 3.0)) 


library(mccrr)
tidy_name = function(x) {
  x = tolower(substr(abbreviate(x), 1, 7))
  paste(c(x, rep('', 7 - nchar(x))), collapse = '')
}
tidy_name("Jonathan Aguilar-Lopez")

pointsgraph <- function(df) {
  students <- unique(df$name)
  for (i in students) {
    df %>%
      mutate(day = factor(day))  %>%
      mutate(week = factor(week)) %>%
      select(date, day, week, meanR, meanT, meanF, mean, name) %>%
      melt(id.vars = c("date", "day", "week", "name"))  %>%
      filter(variable != "mean") %>%
      filter(name == i) %>%
      ggplot(aes(x = date, y = value, color = variable))  +
      geom_jitter(alpha = 0.4,
                  width = 0.25,
                  height = 0.25) +
      geom_smooth() +
      scale_color_inova() +
      theme_light() +
      labs(title = "Trajectory of all points",
           subtitle = i,
           y = "Average Daily Points") +
      scale_y_continuous(limits = c(0.0, 3.0))
    ggsave(paste0(i, ".png"), device = "png")
  }

}
pointsgraph(points)
