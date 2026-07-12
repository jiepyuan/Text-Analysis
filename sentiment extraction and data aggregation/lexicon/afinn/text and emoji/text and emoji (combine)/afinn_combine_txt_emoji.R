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


diction <- get_sentiments("afinn")  # define which lexicon we use


sentiment_score <- tidy_data %>%
  inner_join(diction, relationship = "many-to-many") %>%
  select(Author_ID, month, value) %>% 
  group_by(Author_ID, month) %>% 
  summarise(
    original_score = sum(value)
  )


sum_na <- function(x) {  # define a function to help us extract the emoji sentiment score 
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(sum(x, na.rm = TRUE))
  }
}



emoji_month <- complete_data %>%
  transmute(
    Author_ID,
    month = substr(month, 1, 7),
    emoji_sentiment = as.numeric(emoji_sentiment)
  ) %>%
  group_by(Author_ID, month) %>%
  summarise(
    emoji_sentiment = sum_na(emoji_sentiment),
    .groups = "drop"
  )


sentiment_score <- sentiment_score %>%
  full_join(emoji_month, by = c("Author_ID", "month")) %>%
  mutate(
    score = if_else(
      is.na(original_score) & is.na(emoji_sentiment),
      NA_real_,
      coalesce(original_score, 0) + coalesce(emoji_sentiment, 0)
    )
  ) %>%
  filter(!is.na(score)) %>%   
  select(Author_ID, month, original_score, emoji_sentiment, score)



# coalesce(original_score, 0); if original_score is NA, then return 0, otherwise, return the value of original_score. Although this situation is misleading when using alone, but since we alreay exclude the sitaution of both are NA, so at least one of original or emoji_sentiment is non-NA, so the worst sitaution is one is 5 another is NA, then 5=5+0 is what we want



# (3) load in the plot and data cleaning functions ------------------------

source("function.r")


# (4) plot ----------------------------------------------------------------

range(sentiment_score$score, na.rm = T)


# the not weighted average plot (the denominator is the total number of positive post sent by the user within a month)
plot_interaction(input_data = sentiment_score, response_col = "score", 
                 file_name = "afinn_combine_txt_emoji", 
                 y_lower = -60, y_upper = 60, line_title_position = 55, 
                 ylab = "afinn_combine_txt_emoji")


# (5) Save the output data ------------------------------------------------

wide <- make_wide_t(complete_data = complete_data, 
                    sentiment_df = sentiment_score, 
                    score_col = "score")

write.csv(wide, "afinn_combine_txt_emoji.csv", row.names = F)
