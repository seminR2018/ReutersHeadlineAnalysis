require(tidyverse)
require(tidytext)
require(sentimentr)

get_sentiments <- function(text_column) {
  out <- get_sentences(text_column) %>% 
    sentiment() %>% 
    group_by(element_id) %>% 
    summarise(
      avg_sentiment = sum(word_count * sentiment) / sum(word_count)
    )
  out$avg_sentiment
}
