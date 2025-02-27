apply_function_to_columns <- function(list_of_df, show_columns = c("A", "B"), func) {
  
  # Check if list_of_df is actually a list
  if (!is.list(list_of_df)) {
    stop("Error: The first argument must be a list of data frames.")
  }
  
  # Iterate through the list of data frames
  results <- lapply(seq_along(list_of_df), function(i) {
    df <- list_of_df[[i]]
    
    # Check if df is a data frame
    if (!is.data.frame(df)) {
      return(paste0("Error: Element ", i, " is not a data frame."))
    }
    
    # Check if all required columns exist
    missing_cols <- setdiff(show_columns, colnames(df))
    if (length(missing_cols) > 0) {
      return(paste0("Error: Missing columns in DataFrame ", i, ": ", paste(missing_cols, collapse = ", ")))
    }
    
    # Apply function to selected columns
    tryCatch({
      df[, show_columns] <- lapply(df[, show_columns, drop = FALSE], func)
      return(df)
    }, error = function(e) {
      return(paste0("Error in applying function to DataFrame ", i, ": ", e$message))
    })
  })
  
  return(results)
}
