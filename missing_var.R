#' Compute Percentage of Missing Values by a Grouping Variable
#'
#' @param df A data.frame or data.table containing the dataset.
#' @param date_col A string specifying the column name used for grouping (e.g., a date or categorical variable).
#' @param group_func A function to extract the grouping variable (e.g., `year`, `month`). Defaults to `year`.
#'
#' @return A data frame with variables and their percentage of missing values per group.
#' @import data.table
#' @export
#'
#' @examples
#' df <- data.frame(
#'   date_var = seq(as.Date("2020-01-01"), by = "month", length.out = 12),
#'   var1 = c(1, NA, 3, 4, NA, 6, 7, NA, 9, 10, 11, NA),
#'   var2 = c(NA, 2, 3, NA, 5, NA, 7, 8, 9, 10, 11, 12)
#' )
#' missing_by_year <- missing_percentage_by_group(df, "date_var", year)
#' missing_by_month <- missing_percentage_by_group(df, "date_var", month)
#' print(missing_by_year)
#' print(missing_by_month)

missing_percentage_by_group <- function(df, date_col, group_func = year) {
  # Validate inputs
  if (!is.data.frame(df)) {
    stop("Error: 'df' must be a data.frame or data.table.")
  }
  
  if (!is.character(date_col) || !(date_col %in% colnames(df))) {
    stop("Error: 'date_col' must be a valid column name in the dataset.")
  }
  
  if (!is.function(group_func)) {
    stop("Error: 'group_func' must be a valid function (e.g., year, month).")
  }
  
  # Convert to data.table if necessary
  df.DT <- as.data.table(df)

  # Ensure the date_col is of Date type (or convertible)
  if (!inherits(df.DT[[date_col]], "Date") && !inherits(df.DT[[date_col]], "POSIXt")) {
    stop("Error: 'date_col' must be a Date or POSIXt column.")
  }
  
  # Add group variable
  df.DT[, group_var := tryCatch(group_func(get(date_col)), 
                                error = function(e) stop("Error applying group_func: ", e$message))]
  
  # Compute missing percentage by group
  missing_by_group <- df.DT[, lapply(.SD, function(x) sum(is.na(x)) / .N * 100), by = group_var]

  # Ensure at least one missing value exists
  if (all(missing_by_group[, -1] == 0, na.rm = TRUE)) {
    warning("Warning: No missing values detected in the dataset.")
  }

  # Transpose results for better readability
  df_t <- as.data.frame(t(missing_by_group))
  colnames(df_t) <- missing_by_group$group_var
  df_t$var_name <- colnames(missing_by_group)
  rownames(df_t) <- NULL
  
  return(df_t)
}
