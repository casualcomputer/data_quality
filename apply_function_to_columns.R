# 1. Define select_time_var to handle (df, valid_columns)
select_time_var <- function(df, valid_columns) {
  # Ensure valid_columns is a character vector
  valid_columns <- unlist(valid_columns, use.names = FALSE)
  
  if (!is.data.frame(df)) {
    stop("Error: The input is not a data frame.")
  }
  
  existing_columns <- intersect(valid_columns, colnames(df))
  
  if (length(existing_columns) == 0) {
    stop("Error: None of the specified columns exist in the data frame.")
  }
  
  return(df[ , existing_columns, drop = FALSE])
}

# 2. Define apply_function_to_columns to call func(df, valid_columns)
apply_function_to_columns <- function(list_of_df, valid_columns, func) {
  if (!is.list(list_of_df)) {
    stop("Error: The first argument must be a list of data frames.")
  }
  
  results <- lapply(seq_along(list_of_df), function(i) {
    df <- list_of_df[[i]]
    
    if (!is.data.frame(df)) {
      return(paste0("Error: Element ", i, " is not a data frame."))
    }
    
    tryCatch({
      df <- func(df, valid_columns)
      return(df)
    }, error = function(e) {
      return(paste0("Error in applying function to DataFrame ", i, ": ", e$message))
    })
  })
  
  return(results)
}

# 3. Usage
#time_df <- data.frame(
#  A = 1:3,
#  B = 4:6,
#  C = 7:9
#)
#valid_columns <- c("A", "B")

#apply_function_to_columns(
#  list_of_df = list(time_df),
#  valid_columns = valid_columns,
#  func = select_time_var
#)
