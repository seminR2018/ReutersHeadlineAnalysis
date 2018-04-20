library(tidyverse)
library(tidytext)

## ---- Read CSV file and save the final table in feather format ----
if (file.exists("_data/newswire.feather")) {
  dta <- feather::read_feather("_data/newswire.feather")
} else {
  files <- dir("_data", pattern = "csv")
  dta <- map_df(files, function(file) {
    read_csv(paste0("_data/", file))
  })
  
  dta <- dta %>%
    separate(publish_time,
             into = c("Year", "Month", "Day", "Hour", "Minutes"),
             sep = c(4, 6, 8, 10))
  feather::write_feather(dta, "_data/newswire.feather")
}

## ---- Read the feather format of the data ----
dta %>% 
  rowwise() %>% 
  