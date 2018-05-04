library(tidyverse)
library(tidytext)

## ---- Read CSV file and save the final table in feather format ----
if (file.exists("data/newswire.feather")) {
  dta <- feather::read_feather("data/newswire.feather")
} else {
  files <- dir("data", pattern = "csv")
  dta <- map_df(files, function(file) {
    read_csv(paste0("data/", file))
  })
  
  dta <- dta %>%
    separate(publish_time,
             into = c("Year", "Month", "Day", "Hour", "Minutes"),
             sep = c(4, 6, 8, 10))
  feather::write_feather(dta, "_data/newswire.feather")
}

## ---- Read the feather format of the data ----
word_count <- function(txt, token, ...){
  tbl <- tibble(text = txt)
  out <- tbl %>% unnest_tokens(col_name, text, token, ...) %>% 
    nrow()
  return(out)
}

with_counts <- dta %>% 
  sample_n(1000) %>% 
  rowwise() %>% 
  mutate(word = word_count(headline_text, "words"),
         character = word_count(headline_text, "characters"),
         word_den = word/character,
         punct = map_int(headline_text, ~nchar(gsub("[^[:punct:]]", "", .x))),
         upper = word_count(headline_text, "regex", pattern = "[A-Z]"),
         lower = word_count(headline_text, "regex", pattern = "[a-z]"),
         ul_ratio = upper/lower)

with_counts %>% 
  select(Year, word, character, word_den, punct, upper, lower) %>% 
  gather(type, value, -Year) %>% 
  ggplot(aes(Year, value)) +
  facet_wrap(~type, scales = 'free_y') +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red", width = 0.3) +
  stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", group = 1) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
