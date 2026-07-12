library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(lubridate)
library(textdata)
library(stringr)
library(readr)
library(EmojiSentR)


# (1) load in the data ----------------------------------------------------

complete_data <- read_csv("text_data_new_emoji.csv")



# (2) extract sentiment score ---------------------------------------------

tidy_data <- complete_data %>% 
  select(Author_ID, text, month) %>%
  group_by(Author_ID, month) %>%
  summarise(text = paste(text, collapse = ". "), .groups = "drop") %>%
  unnest_tokens(output = word, input = text) 


diction <- get_sentiments("bing")  # define which lexicon we use


sentiment_score <- tidy_data %>%
  inner_join(diction, relationship = "many-to-many") %>% 
  filter(sentiment %in% c("positive", "negative")) %>%   
  count(Author_ID, month, sentiment) %>% 
  pivot_wider(names_from = sentiment,  
              values_from = n,  
              values_fill = 0) %>%  
  mutate(original_score= positive - negative) %>% 
  filter(!(is.na(original_score)))

# coalesce(original_score, 0); if original_score is NA, then return 0, otherwise, return the value of original_score. Although this situation is misleading when using alone, but since we alreay exclude the sitaution of both are NA, so at least one of original or emoji_sentiment is non-NA, so the worst sitaution is one is 5 another is NA, then 5=5+0 is what we want




# (3) load in the plot and data cleaning functions ------------------------

source("function.r")



# (4) plot ----------------------------------------------------------------

range(sentiment_score$original_score, na.rm = T)


# the not weighted average plot (the denominator is the total number of positive post sent by the user within a month)
plot_interaction(input_data = sentiment_score, response_col = "original_score", 
                 file_name = "bing_combine_txt", 
                 y_lower = -60, y_upper = 60, line_title_position = 55, 
                 ylab = "bing_combine_txt")


# (5) Save the output data ------------------------------------------------

wide <- make_wide_t(complete_data = complete_data, 
                    sentiment_df = sentiment_score, 
                    score_col = "original_score")

write.csv(wide, "bing_combine_txt.csv", row.names = F)
