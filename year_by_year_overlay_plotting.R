# Year by Year plotting with ggplot

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)

D = read_csv("C:/Users/Jacob/Downloads/ml-latest-small/ml-latest-small/ratings.csv")

i = 1
D %>% filter(movieId == i) %>% 
      mutate(date = as.Date(as.POSIXct(timestamp, origin = "1970-01-01"))) %>% 
      mutate(month = month(date, label=TRUE, abbr=TRUE), year = year(date)) %>% 
      filter(year > 2016) %>% # note the filter on year
      group_by(year, month) %>% tally() %>%
      group_by(year) %>%
      complete(month) %>% mutate(n = replace_na(n, replace = 0)) %>%
      ggplot(aes(month,  n, group = factor(year), colour = factor(year))) + geom_line() + ylim(0,10) +
      geom_point() +
      labs(x = "Month", colour = "Year")
