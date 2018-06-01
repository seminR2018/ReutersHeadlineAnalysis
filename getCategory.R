library(tidyverse)

getCategory <- function(x, categories){
  # look for categories in the text
  # Only the first category found in the text will be returned
  tibble( idx = seq_along(x), text = x) %>% 
    unnest_tokens( word, text ) %>% 
    filter( word %in% categories) %>%
    distinct( idx, .keep_all = T ) %>% 
    # return one word per text
    right_join(tibble( idx = seq_along(x)), by="idx") %>% 
    with( word )
}