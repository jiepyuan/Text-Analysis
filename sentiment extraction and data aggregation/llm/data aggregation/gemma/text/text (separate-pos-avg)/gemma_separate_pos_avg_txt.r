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

complete_data <- read_csv("text_data_new.csv")
gemma_data <- read_csv("gemma_output_separate_text.csv")[,-1]

identical(gemma_data$Author_ID, complete_data$Author_ID) # it's true, so we just add the sentiment score column to the complete data

complete_data$original_score <- gemma_data$sentiment_score

# (1) extract sentiment score ---------------------------------------------


sentiment_score <- complete_data %>%
  select(Author_ID, date, original_score) %>%  
  filter(!(is.na(original_score)))


# (2) calculate the average -----------------------------------------------

sentiment_avg_pos <- sentiment_score %>% 
  filter(original_score > 5) %>% 
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
                 file_name = "gemma_separate_pos_avg_txt", 
                 y_lower = 5, y_upper = 10, line_title_position = 9.5, 
                 ylab = "gemma_separate_pos_avg_txt")


# the weighted average plot (the denominator is the total number of post sent by the user within a month)
plot_interaction(input_data = sentiment_avg_pos, response_col = "score_new", 
                 file_name = "gemma_separate_pos_avg_txt_weighted", 
                 y_lower = 5, y_upper = 10, line_title_position = 9.5, 
                 ylab = "gemma_separate_pos_avg_txt_weighted")



# (5) Save the output data ------------------------------------------------

wide <- make_wide_t(complete_data = complete_data, 
                    sentiment_df = sentiment_avg_pos, 
                    score_col = "score")

wide_weighted <- make_wide_t(complete_data = complete_data, 
                             sentiment_df = sentiment_avg_pos, 
                             score_col = "score_new") 

write.csv(wide, "gemma_separate_pos_avg_txt.csv", row.names = F)
write.csv(wide_weighted, "gemma_separate_pos_avg_txt_weighted.csv", row.names = F)