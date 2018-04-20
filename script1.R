# This is the first script
library(readr)
library(lubridate)
library(dplyr)

### Reading data from file
tab <- read_csv("data/reuters-newswire-2011.csv.zip", 
                  col_types=cols(publish_time=col_character()))

### Converting column publish_time to date-time
tab %>%
  mutate(publish_time=ymd_hm(publish_time)) -> tab

