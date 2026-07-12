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

complete_data <- read_csv("text_data_new.csv")
llama_data <- read_csv("llama_output_combine_text.csv")[,-1]  # we can't set llama_data to complete since the make_wide_t function will extract the time-invariant variable from the complete data and the sentiment score from sentiment_score data which is from llama_data; If the original llama_data has the time_invariant variable then we are good to go, otherwise, no


# (2) extract sentiment score ---------------------------------------------

sentiment_score <- llama_data %>%
  rename(original_score = sentiment_score) %>% 
  select(Author_ID, month, original_score) %>% 
  filter(!(is.na(original_score)))


# coalesce(original_score, 0); if original_score is NA, then return 0, otherwise, return the value of original_score. Although this situation is misleading when using alone, but since we alreay exclude the sitaution of both are NA, so at least one of original or emoji_sentiment is non-NA, so the worst sitaution is one is 5 another is NA, then 5=5+0 is what we want




# (3) load in the plot and data cleaning functions ------------------------

source("function.r")



# (4) plot ----------------------------------------------------------------

range(sentiment_score$original_score, na.rm = T) # there are two observation below the lower boundary (<0, -3 and -1.4), because there are only two, i dind't remove them 


# the not weighted average plot (the denominator is the total number of positive post sent by the user within a month)
plot_interaction(input_data = sentiment_score, response_col = "original_score", 
                 file_name = "llama_combine_txt", 
                 y_lower = 0, y_upper = 10, line_title_position = 9.5, 
                 ylab = "llama_combine_txt")


# (5) Save the output data ------------------------------------------------

wide <- make_wide_t(complete_data = complete_data, 
                    sentiment_df = sentiment_score, 
                    score_col = "original_score")

write.csv(wide, "llama_combine_txt.csv", row.names = F)
