library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(lubridate)
library(textdata)
library(stringr)
library(readr)
library(EmojiSentR)


# (0) load in the data ----------------------------------------------------

complete_data <- read_csv("text_data_new_emoji.csv")



# (1) extract sentiment score ---------------------------------------------

tidy_data <- complete_data %>% # I use author_id since it's a unique identification number which would not change like username
  select(Author_ID, text, date, emoji_sentiment) %>% 
  unnest_tokens(output = word, input = text)


diction <- get_sentiments("afinn")  # define which lexicon we use


sentiment_score <- tidy_data %>%
  inner_join(diction, relationship = "many-to-many") %>%
  select(Author_ID, date, value) %>% 
  group_by(Author_ID, date) %>% 
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



emoji_month <- complete_data %>%  # extract the emoji sentiment for each month
  transmute(
    Author_ID,
    date = date,
    emoji_sentiment = as.numeric(emoji_sentiment)
  ) %>%
  group_by(Author_ID, date) %>%
  summarise(
    emoji_sentiment = sum_na(emoji_sentiment),
    .groups = "drop"
  )



sentiment_score <- sentiment_score %>%
  full_join(emoji_month, by = c("Author_ID", "date")) %>%
  mutate(
    score = if_else(
      is.na(original_score) & is.na(emoji_sentiment),
      NA_real_,
      coalesce(original_score, 0) + coalesce(emoji_sentiment, 0)
    )
  ) %>%
  # drop rows where both signals are NA
  filter(!(is.na(original_score) & is.na(emoji_sentiment))) %>%
  select(Author_ID, date, original_score, emoji_sentiment, score)



# coalesce(original_score, 0); if original_score is NA, then return 0, otherwise, return the value of original_score. Although this situation is misleading when using alone, but since we alreay exclude the sitaution of both are NA, so at least one of original or emoji_sentiment is non-NA, so the worst sitaution is one is 5 another is NA, then 5=5+0 is what we want



# (2) calculate the average -----------------------------------------------

sentiment_avg_pos <- sentiment_score %>% 
  filter(score > 0) %>% 
  mutate(month_floor = floor_date(date, unit = "month"),
         month = format(month_floor, "%Y-%m")) %>% 
  select(Author_ID, month, score) %>% 
  filter(!is.na(score)) %>%   # drop rows where score is NA
  group_by(Author_ID, month) %>% 
  summarise(
    tweet_num = n(),                      # number of non-NA tweets
    sum_score = sum(score),               # sum of scores (all non-NA)
    score     = mean(score)               # mean of scores (all non-NA)
  ) %>% 
  ungroup()



test <- sentiment_score %>%  # since we also need to consider the weight cases, where the average is calculated by total sum of positive sentiment score/total number of post sent within a month (instead of total number of positive sentiment post)
  mutate(month_floor = floor_date(date, unit = "month")) %>%
  mutate(month = format(month_floor, "%Y-%m")) %>% 
  select(Author_ID, month, original_score) %>% 
  group_by(Author_ID, month) %>%  # combine each unique combination of Author_ID and month into one row, for each combination
  summarise(
    tweet_num_total = n(), # calculate the frequency of this unique combination appear
  ) %>% 
  ungroup()



sentiment_avg_pos <- sentiment_avg_pos %>% 
  left_join(test, by = c("Author_ID", "month")) %>% 
  mutate(score_new = sum_score/tweet_num_total)



# (3) load in the plot and data cleaning functions ------------------------

source("function.r")


# (4) plot ----------------------------------------------------------------

range(sentiment_avg_pos$score, na.rm = T)

range(sentiment_avg_pos$score_new, na.rm = T)


# the not weighted average plot (the denominator is the total number of positive post sent by the user within a month)
plot_interaction(input_data = sentiment_avg_pos, response_col = "score", 
                 file_name = "afinn_separate_pos_avg_txt_emoji", 
                 y_lower = 0, y_upper = 36, line_title_position = 30, 
                 ylab = "afinn_separate_pos_avg_txt_emoji")


# the weighted average plot (the denominator is the total number of post sent by the user within a month)
plot_interaction(input_data = sentiment_avg_pos, response_col = "score_new", 
                 file_name = "afinn_separate_pos_avg_txt_emoji_weighted", 
                 y_lower = 0, y_upper = 36, line_title_position = 30, 
                 ylab = "afinn_separate_pos_avg_txt_emoji_weighted")



# (5) Save the output data ------------------------------------------------

wide <- make_wide_t(complete_data = complete_data, 
                    sentiment_df = sentiment_avg_pos, 
                    score_col = "score")

wide_weighted <- make_wide_t(complete_data = complete_data, 
                             sentiment_df = sentiment_avg_pos, 
                             score_col = "score_new") 

write.csv(wide, "afinn_separate_pos_avg_txt_emoji.csv", row.names = F)
write.csv(wide_weighted, "afinn_separate_pos_avg_txt_emoji_weighted.csv", row.names = F)