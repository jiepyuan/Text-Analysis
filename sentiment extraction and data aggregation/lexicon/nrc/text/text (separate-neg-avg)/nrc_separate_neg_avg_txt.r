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


diction <- get_sentiments("nrc")  # define which lexicon we use


sentiment_score <- tidy_data %>%
  inner_join(diction, relationship = "many-to-many") %>% 
  filter(sentiment %in% c("positive", "negative")) %>%   
  count(Author_ID, date, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n,  
              values_fill = 0) %>%  
  mutate(original_score= positive - negative)%>% 
  filter(!(is.na(original_score)))

# (2) calculate the average -----------------------------------------------

sentiment_avg_neg <- sentiment_score %>% 
  filter(original_score < 0) %>% 
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



test <- sentiment_score %>%  # since we also need to consider the weight cases, where the average is calculated by total sum of positive sentiment score/total number of post sent within a month (instead of total number of positive sentiment post)
  mutate(month_floor = floor_date(date, unit = "month")) %>%
  mutate(month = format(month_floor, "%Y-%m")) %>% 
  select(Author_ID, month, original_score) %>% 
  group_by(Author_ID, month) %>%  # combine each unique combination of Author_ID and month into one row, for each combination
  summarise(
    tweet_num_total = n(), # calculate the frequency of this unique combination appear
  ) %>% 
  ungroup()



sentiment_avg_neg <- sentiment_avg_neg %>% 
  left_join(test, by = c("Author_ID", "month")) %>% 
  mutate(score_new = sum_score/tweet_num_total)



# (3) load in the plot and data cleaning functions ------------------------

source("function.r")


# (4) plot ----------------------------------------------------------------

range(sentiment_avg_neg$score, na.rm = T)

range(sentiment_avg_neg$score_new, na.rm = T)


# the not weighted average plot (the denominator is the total number of positive post sent by the user within a month)
plot_interaction(input_data = sentiment_avg_neg, response_col = "score", 
                 file_name = "nrc_separate_neg_avg_txt", 
                 y_lower = -20, y_upper = 0, line_title_position = -18, 
                 ylab = "nrc_separate_neg_avg_txt")


# the weighted average plot (the denominator is the total number of post sent by the user within a month)
plot_interaction(input_data = sentiment_avg_neg, response_col = "score_new", 
                 file_name = "nrc_separate_neg_avg_txt_weighted", 
                 y_lower = -20, y_upper = 0, line_title_position = -18, 
                 ylab = "nrc_separate_neg_avg_txt_weighted")



# (5) Save the output data ------------------------------------------------

wide <- make_wide_t(complete_data = complete_data, 
                    sentiment_df = sentiment_avg_neg, 
                    score_col = "score")

wide_weighted <- make_wide_t(complete_data = complete_data, 
                             sentiment_df = sentiment_avg_neg, 
                             score_col = "score_new") 

write.csv(wide, "nrc_separate_neg_avg_txt.csv", row.names = F)
write.csv(wide_weighted, "nrc_separate_neg_avg_txt_weighted.csv", row.names = F)