## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE, 
  warning = FALSE, 
  message = FALSE,
  comment = NULL, 
  cache = TRUE,
  out.width = '100%'
  )

## ------------------------------------------------------------------------
library(tidyverse)
library(tidytext)
library(sentimentr)
library(magrittr)
library(lubridate)

## ------------------------------------------------------------------------
k_scale <- function(x) paste0(round(x/1000), "K")

## ---- eval=!file.exists("data/newswire_counts.feather"), cache = TRUE----
if (file.exists("data/newswire.feather")) {
  dta <- feather::read_feather("data/newswire.feather")
} else {
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
}

## ---- cache=TRUE, eval = file.exists('data/newswire_counts.feather')-----
newswire <- feather::read_feather("data/newswire_counts.feather")

## ---- cache=TRUE, eval = !file.exists('data/newswire_counts.feather')----
if (file.exists("data/newswire_counts.feather")) {
  feather::read_feather("data/newswire_counts.feather")
} else {
  newswire <- dta %>%
  mutate(
    word              = map_int(strsplit(headline_text, split = " "), length),
    character         = nchar(gsub(" ", "", headline_text)),
    avg_word_length   = character / word,
    word_density      = word / (character + 1),
    punctuation       = nchar(gsub("[^[:punct:]]", "", headline_text)),
    upper             = nchar(gsub("[^[:upper:]+]", "", headline_text)),
    lower             = nchar(gsub("[^[:lower:]+]", "", headline_text)),
    ul_ratio          = upper / lower
  )
  feather::write_feather(with_counts, "data/newswire_counts.feather")
}

## ------------------------------------------------------------------------
my_scale <- function(x) paste0(round(x/1000), "K")
dta <- newswire
dta %>% 
  ggplot(aes(word)) +
  geom_histogram() +
  facet_wrap(~ Year, ncol = 4) +
  scale_y_continuous(labels = my_scale)

## ------------------------------------------------------------------------
dta %>% 
  ggplot(aes(character)) +
  geom_histogram() +
  facet_wrap(~ Year, ncol = 4) +
  scale_y_continuous(labels = my_scale)

## ------------------------------------------------------------------------
dta %>% 
  ggplot(aes(word_density)) +
  geom_histogram() +
  facet_wrap(~ Year, ncol = 4) +
  scale_y_continuous(labels = my_scale)

## ------------------------------------------------------------------------
dta %>% 
  ggplot(aes(punctuation)) +
  geom_histogram(bins = 18) +
  facet_wrap(~ Year, ncol = 4) +
  scale_y_continuous(labels = my_scale)

## ---- fig.asp=1----------------------------------------------------------
p1 <- dta %>% 
  group_by(Year, Month) %>% 
  summarize(Month_Count = n()) %>% 
  mutate(Month = as.integer(Month)) %>% 
  ggplot(aes(Month, Month_Count)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(labels = my_scale) +
  facet_grid(Year ~ .)
p2 <- dta %>% 
  group_by(Year, Day) %>% 
  summarize(Day_Count = n()) %>% 
  mutate(Day = as.integer(Day)) %>% 
  ggplot(aes(Day, Day_Count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = my_scale) +
  facet_grid(Year ~ .)
gridExtra::grid.arrange(p1, p2, ncol = 2)

## ---- fig.asp=1----------------------------------------------------------
p1 <- dta %>% 
  group_by(Year, Hour) %>% 
  summarize(Hour_Count = n()) %>% 
  mutate(Hour = as.integer(Hour)) %>% 
  ggplot(aes(Hour, Hour_Count)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(labels = my_scale) +
  facet_grid(Year ~ .)
p2 <- dta %>% 
  group_by(Year, Minutes) %>% 
  summarize(Minutes_Count = n()) %>% 
  mutate(Minutes = as.integer(Minutes)) %>% 
  ggplot(aes(Minutes, Minutes_Count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = my_scale) +
  facet_grid(Year ~ .)
gridExtra::grid.arrange(p1, p2, ncol = 2)

## ------------------------------------------------------------------------
dta1 <- dta %>% 
  select(Year, Month, Day, Hour, Minutes, headline_text) %>% 
  mutate(WeekDay = wday(as_date(paste(Year, Month, Day)), label = TRUE))
dta_sub <- dta1 %>%
  group_by(Year) %>% 
  sample_n(100, replace = FALSE) %>%
  ungroup()
sentiments_data <- dta_sub %>% 
  get_sentences() %$%
  sentiment_by(headline_text, list(1:n()))
  
sentiments_with_headline <- sentiments_data %>% 
  left_join(dta_sub, by = c("Year", "Month", "Day", "Hour", "Minutes")) %>% 
  as_tibble() %>% 
  arrange(desc(ave_sentiment))

## ------------------------------------------------------------------------
plt_dta1 %>% 
  mutate(`Publish Date` = as_date(paste(Year, Month, Day))) %>% 
  ggplot(aes(`Publish Date`, ave_sentiment)) +
  geom_line() +
  geom_hline(yintercept = 0, color = "blue") +
  facet_wrap(~Year, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = '1 month', date_labels = "%b")

