library(ollamar)
library(readr)
library(pbapply)
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(lubridate)
library(textdata)
library(stringr)
library(EmojiSentR)
library(stringi)

# define the LLMs function 
# define the LLMs function (minimal-change fix: guard numeric conversion)
llm_sentiment_analysis <- function(data, col_index, select_model, creativity_degree){
  
  n <- nrow(data)
  output <- rep(NA_real_, n)  # preallocate numeric output (safer & faster)
  
  pb <- txtProgressBar(min = 0, max = n, style = 3)  # create progress bar
  on.exit(try(close(pb), silent = TRUE), add = TRUE)  # ensure the bar closes on error
  
  # regex pattern for the first numeric token (supports negative & decimals)
  pat <- "(-?\\d+(?:\\.\\d+)?)"
  
  for (i in seq_len(n)) {
    
    if (is.na(data[i, col_index])) {
      output[i] <- NA_real_
    } else {
      prompt_content <- paste0(
        "Rate the sentiment of the following sentence on a scale from 0 (very negative) to 10 (very positive). ",
        "Output only a single numeric value with no explanation or text. Sentence: ",
        data[i, col_index]
      )
      
      temp_output <- try(
        generate(
          model = select_model,
          prompt = prompt_content,
          stream = FALSE,
          output = "text",
          temperature = creativity_degree
        ),
        silent = TRUE
      )
      
      if (!inherits(temp_output, "try-error")) {
        # Convert to character to be safe
        tmp_chr <- as.character(temp_output)
        
        # Only convert to numeric if the pattern exists; otherwise set NA
        if (grepl(pat, tmp_chr, perl = TRUE)) {
          output_value <- as.numeric(sub(paste0(".*?", pat, ".*"), "\\1", tmp_chr, perl = TRUE))
          output[i] <- output_value
        } else {
          output[i] <- NA_real_
        }
      } else {
        output[i] <- NA_real_
      }
    }
    
    setTxtProgressBar(pb, i)  # update the progress bar
  }
  
  result <- data
  result$sentiment_score <- output
  return(result)
}

# load in the data
real_data <- read_csv("text_data_final.csv")

emoji_rx <- "\\p{Extended_Pictographic}(?:\\p{EMod}|\\uFE0F|\\u200D\\p{Extended_Pictographic})*"

clean_data <- real_data %>%
  mutate(
    text_raw      = text,
    emoji_only    = sapply(stri_extract_all_regex(text_raw, emoji_rx), function(x) paste(x, collapse = "")),
    text_no_emoji = stri_replace_all_regex(text_raw, emoji_rx, ""),
    text_no_emoji = stri_trim_both(text_no_emoji)
  )


# text only  --------------------------------------------------------------

mistral_output_separate_text <- llm_sentiment_analysis(data = clean_data, col_index = which(colnames(clean_data)=="text_no_emoji"), select_model = "mistral:7b", creativity_degree = 0)

save(mistral_output_separate_text, file = "mistral_output_separate_text.RData")
write.csv(mistral_output_separate_text, "mistral_output_separate_text.csv")


gemma_output_separate_text <- llm_sentiment_analysis(data = clean_data, col_index = which(colnames(clean_data)=="text_no_emoji"), select_model = "gemma3:4b", creativity_degree = 0)

save(gemma_output_separate_text, file = "gemma_output_separate_text.RData")
write.csv(gemma_output_separate_text, "gemma_output_separate_text.csv")

llama_output_separate_text <- llm_sentiment_analysis(data = clean_data, col_index = which(colnames(clean_data)=="text_no_emoji"), select_model = "llama3.1:8b", creativity_degree = 0)

save(llama_output_separate_text, file = "llama_output_separate_text.RData")
write.csv(llama_output_separate_text, "llama_output_separate_text.csv")






# text and emoji ----------------------------------------------------------
mistral_output_separate_text_emoji <- llm_sentiment_analysis(data = clean_data, col_index = which(colnames(clean_data)=="text"), select_model = "mistral:7b", creativity_degree = 0) 

save(mistral_output_separate_text_emoji, file = "mistral_output_separate_text_emoji.RData")
write.csv(mistral_output_separate_text_emoji, "mistral_output_separate_text_emoji.csv")


# gemma -------------------------------------------------------------------

gemma_output_separate_text_emoji <- llm_sentiment_analysis(data = clean_data, col_index = which(colnames(clean_data)=="text"), select_model = "gemma3:4b", creativity_degree = 0)

save(gemma_output_separate_text_emoji, file = "gemma_output_separate_text_emoji.RData")
write.csv(gemma_output_separate_text_emoji, "gemma_output_separate_text_emoji.csv")

# llama -------------------------------------------------------------------

llama_output_separate_text_emoji <- llm_sentiment_analysis(data = clean_data, col_index = which(colnames(clean_data)=="text"), select_model = "llama3.1:8b", creativity_degree = 0)

save(llama_output_separate_text_emoji, file = "llama_output_separate_text_emoji.RData")
write.csv(llama_output_separate_text_emoji, "llama_output_separate_text_emoji.csv")
