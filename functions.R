library(tidyverse)
library(tidytext)
library(sentimentr)
library(magrittr)
library(lubridate)

get_word <- function(text) map_int(strsplit(text, split = " "), length)
get_character <- function(text) nchar(gsub(" ", "", text))
get_avg_word_length <- function(chr, word) chr / word
get_word_density <- function(chr, word) word / (chr + 1)
get_punctuation <- function(text) nchar(gsub("[^[:punct:]]", "", text))
get_upper <- function(text) nchar(gsub("[^[:upper:]+]", "", text))
get_lower <- function(text) nchar(gsub("[^[:lower:]+]", "", text))
get_ul_ratio <- function(upper, lower) upper / lower
get_sentiments <- function(text_column) {
  out <- get_sentences(text_column) %>% 
    sentiment() %>% 
    group_by(element_id) %>% 
    summarise(
      avg_sentiment = sum(word_count * sentiment) / sum(word_count)
    )
  out$avg_sentiment
}
