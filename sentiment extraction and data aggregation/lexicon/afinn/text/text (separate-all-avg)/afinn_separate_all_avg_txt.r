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
  ) %>% 
  filter(!(is.na(original_score)))


# (2) calculate the average -----------------------------------------------

sentiment_avg_all <- sentiment_score %>% 
  mutate(month_floor = floor_date(date, unit = "month"),
         month = format(month_floor, "%Y-%m")) %>% 
  select(Author_ID, month, original_score) %>% 
  group_by(Author_ID, month) %>% 
  summarise(
    tweet_num = n(),                      # number of non-NA tweets
    sum_score = sum(original_score),               # sum of scores (all non-NA)
    score     = mean(original_score)               # mean of scores (all non-NA)
  ) %>% 
  ungroup()


# (3) load in the plot and data cleaning functions ------------------------

source("function.r")



# (4) plot ----------------------------------------------------------------

range(sentiment_avg_all$score, na.rm = T)


# the not weighted average plot (the denominator is the total number of positive post sent by the user within a month)
plot_interaction(input_data = sentiment_avg_all, response_col = "score", 
                 file_name = "afinn_separate_all_avg_txt", 
                 y_lower = -30, y_upper = 36, line_title_position = 18, 
                 ylab = "afinn_separate_all_avg_txt")


# (5) Save the output data ------------------------------------------------
wide <- make_wide_t(complete_data, sentiment_avg_all, score_col = "score")

write.csv(wide, "afinn_separate_all_avg_txt.csv")
