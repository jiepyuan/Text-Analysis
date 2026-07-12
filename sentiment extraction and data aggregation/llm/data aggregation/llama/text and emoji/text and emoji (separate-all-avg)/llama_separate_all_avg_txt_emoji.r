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
llama_data <- read_csv("llama_output_separate_text_emoji.csv")[,-1]

identical(llama_data$Author_ID, complete_data$Author_ID) # it's true, so we just add the sentiment score column to the complete data

complete_data$original_score <- llama_data$sentiment_score

# (1) extract sentiment score ---------------------------------------------


sentiment_score <- complete_data %>%
  select(Author_ID, date, original_score) %>%  
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
                 file_name = "llama_separate_all_avg_txt_emoji", 
                 y_lower = 0, y_upper = 10, line_title_position = 9.5, 
                 ylab = "llama_separate_all_avg_txt_emoji")


# (5) Save the output data ------------------------------------------------
wide <- make_wide_t(complete_data, sentiment_avg_all, score_col = "score")

write.csv(wide, "llama_separate_all_avg_txt_emoji.csv")
