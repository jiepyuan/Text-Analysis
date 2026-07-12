# (1) The plot function  --------------------------------------------------
plot_interaction <- function(input_data, response_col, file_name,
                             y_lower, y_upper, line_title_position,
                             ylab = NULL, xtick = TRUE,
                             img_width = 15, img_height = 10,
                             units = "in", res = 300) {
  # Basic checks
  if (!response_col %in% names(input_data)) {
    stop(sprintf("Column '%s' not found in input_data.", response_col))
  }
  if (!("month" %in% names(input_data) && "Author_ID" %in% names(input_data))) {
    stop("input_data must contain columns: month, Author_ID")
  }
  
  # Open device (with safe closing on exit)
  png(filename = paste0(file_name, ".png"),
      width = img_width, height = img_height, units = units, res = res)
  on.exit(dev.off(), add = TRUE)
  
  # Create month factor (sorted by lexicographic order, equivalent to YYYY-MM time order)
  month_fac <- factor(input_data$month, levels = sort(unique(input_data$month)))
  
  # Y-axis label
  if (is.null(ylab)) ylab <- response_col
  
  # Main interaction plot
  interaction.plot(
    x.factor     = month_fac,
    trace.factor = input_data$Author_ID,
    response     = as.numeric(input_data[[response_col]]),
    xlab = "Date (Year-Month)", ylab = ylab,
    col  = seq_along(unique(input_data$Author_ID)),
    legend = FALSE,
    ylim = c(y_lower, y_upper),
    xtick = xtick,   # keep xtick parameter
    lwd = 1.35,
    type = "l"
  )
  
  ## --- Event vertical lines & labels (skip missing months) ---
  event_months <- c("2020-04", "2020-09", "2020-12", "2021-03", "2021-05")
  event_labels <- c(
    "CDC recommends public mask use",
    "HHS: COVID-19 vaccines \nwill be free",
    "FDA grants Pfizer vaccine EUA",
    "Biden sets May 1\nuniversal adult \nvaccine eligibility",
    "CDC: Fully \nvaccinated can \ngo mask-free"
  )
  
  # Map event months to factor levels; NA if not present
  x_pos <- match(event_months, levels(month_fac))
  
  # Draw vertical lines (only for months that exist)
  if (any(!is.na(x_pos))) {
    abline(v = x_pos[!is.na(x_pos)], lty = 5, lwd = 1.5, col = "red")
  }
  
  # Add text labels (only for months that exist)
  for (i in seq_along(x_pos)) {
    if (!is.na(x_pos[i])) {
      text(x = x_pos[i] + 0.1, y = line_title_position,
           labels = event_labels[i],
           adj = 0, col = "red", cex = 0.9, font = 2)
    }
  }
  
  invisible(paste0(file_name, ".png"))
}




# (2) The data cleaning function  -----------------------------------------
make_wide_t <- function(complete_data, sentiment_df, score_col) {
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(zoo)
  
  # Ensure the Author_ID and month are in the correct format
  df <- complete_data %>%
    mutate(
      Author_ID = as.character(Author_ID),
      month     = substr(month, 1, 7),   # "YYYY-MM"
      date      = as.Date(date)
    )
  
  # Define the time line, t0=2020-04 and t14=2021-6, overall 15 month
  month_seq <- seq(from = as.yearmon("2020-04"), by = 1/12, length.out = 15)
  month_index <- tibble(
    month = format(as.Date(month_seq), "%Y-%m"),
    t     = 0:14
  )
  
  # Define a function that used to extract the first non-NA value for the time-invariant variables
  first_non_na <- function(x) {
    x2 <- x[!is.na(x)]
    if (length(x2) == 0) NA else x2[1]
  }
  
  # Use the function defined above to extract the value for each time-invariant variable
  static_vars <- df %>%
    group_by(Author_ID) %>%
    summarise(
      Gender               = first_non_na(Gender),
      Education            = first_non_na(Education),
      Region               = first_non_na(Region),
      Place                = first_non_na(Place),
      Urbanity             = first_non_na(Urbanity),
      Country              = first_non_na(Country),
      bio_sentiment_first  = first_non_na(bio_sentiment_first),
      bio_sentiment_last   = first_non_na(bio_sentiment_last),
      Follower_count_avg   = first_non_na(Follower_count_avg),
      .groups = "drop"
    )
  
  # Use the last function to define the time-variant variables value for each month (the last value of the month)
  
  monthly_vars <- df %>%
    filter(month %in% month_index$month) %>%
    group_by(Author_ID, month) %>%
    summarise(
      Cases        = last(Cases,        order_by = date),
      Vaccination1 = last(Vaccination1, order_by = date),
      Vaccination2 = last(Vaccination2, order_by = date),
      .groups = "drop"
    ) %>%
    left_join(month_index, by = "month")
  
  # For each sentiment score for each user for each month, pair the corresponding time index (t), t=1 means it's the first month of collection such as 2020-04
  sentiment_norm <- sentiment_df %>%
    transmute(
      Author_ID = as.character(Author_ID),
      month     = substr(month, 1, 7),
      score     = as.numeric(.data[[score_col]])  # allow the score column name be different
    ) %>%
    inner_join(month_index, by = "month")  
  
  # Make the sentiment_score become wide format
  score_wide <- sentiment_norm %>%
    select(Author_ID, t, score) %>%
    pivot_wider(
      id_cols = Author_ID,
      names_from = t,
      values_from = score,
      names_glue = "t{t}",
      names_sort = TRUE
    )
  
  # Make another three time-variant variables (cases, vaccination1, vaccination2) as wide format
  c19_wide <- monthly_vars %>%
    select(Author_ID, t, Cases, Vaccination1, Vaccination2) %>%
    pivot_wider(
      id_cols = Author_ID,
      names_from = t,
      values_from = c(Cases, Vaccination1, Vaccination2),
      names_glue = "{.value}_t{t}",
      names_sort = TRUE
    )
  
  # left join, combine the time-invariant variable with the time-variant variables
  wide_final <- static_vars %>%
    left_join(score_wide, by = "Author_ID") %>%
    left_join(c19_wide,   by = "Author_ID")
  
  return(wide_final)
}