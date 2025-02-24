library(dplyr)
library(stringr)

# Unified function to filter and reorder columns containing a keyword
filter_and_reorder <- function(df, keyword) {
  matching_cols <- names(df)[map_lgl(df, ~ any(str_detect(as.character(.), keyword), na.rm = TRUE))]
  
  df %>%
    select(all_of(matching_cols), everything())  # Move matching cols to the front
}

# # Example dataset
# df <- tibble(
#   A = c("apple", "banana", "grape"),
#   B = c("dog", "cat", "apple"),
#   C = c(1, 2, 3),
#   D = c("tree", "flower", "grass")
# )
# 
# # Example usage
# keyword <- "apple"
# filtered_df <- filter_and_reorder(df, keyword)
# 
# # View result
# print(filtered_df)
