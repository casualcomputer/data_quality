# Load required packages
library(data.table)    # For fast, memory-efficient data reading
library(janitor)       # For cleaning and standardizing column names
library(dplyr)         # For elegant data manipulation (if further processing is needed)
library(summarytools)  # For comprehensive, well-formatted summary reports
library(ggplot2)       # For visualizing distributions and outliers

# 1. Data Loading & Preparation
load_and_prepare_data <- function(file_path) {
  # Read data efficiently using data.table's fread()
  df <- tryCatch({
    fread(file_path)
  }, error = function(e) {
    stop("Error loading file: ", e)
  })
  
  # Convert to a standard data frame for compatibility with many R functions
  df <- as.data.frame(df)
  
  # Replace empty strings with NA for all character columns
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[x == ""] <- NA
    }
    return(x)
  })
  
  # Standardize column names using janitor's clean_names() (e.g., lower_case_with_underscores)
  df <- janitor::clean_names(df)
  
  # Convert columns that likely represent dates (contain "date") to Date format
  date_cols <- grep("date", names(df), ignore.case = TRUE, value = TRUE)
  for (col in date_cols) {
    df[[col]] <- as.Date(df[[col]], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y"))
  }
  
  return(df)
}

# 2. Data Summarization
summarize_data <- function(df) {
  cat("===== Data Summary Report (using summarytools::dfSummary) =====\n")
  # summarytools provides a rich, browser-friendly overview including distributions, missing values, and graphs
  summary_report <- dfSummary(df, plain.ascii = FALSE, style = "grid",
                              valid.col = FALSE, graph.magnif = 0.75)
  print(summary_report, method = "render")
}

# 3. Missing Value Analysis
analyze_missing <- function(df) {
  cat("===== Missing Value Analysis =====\n")
  missing_summary <- data.frame(
    Variable      = names(df),
    MissingCount  = sapply(df, function(x) sum(is.na(x))),
    MissingPercent= round(sapply(df, function(x) mean(is.na(x)) * 100), 2)
  )
  print(missing_summary)
  return(missing_summary)
}

# 4. Duplicate Records Check
analyze_duplicates <- function(df) {
  cat("===== Duplicate Records Analysis =====\n")
  dup_count <- sum(duplicated(df))
  cat("Duplicate rows found:", dup_count, "\n")
  if (dup_count > 0) {
    dup_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
    print(dup_rows)
  }
  return(dup_count)
}

# 5. Outlier Detection for Numeric Variables
detect_outliers <- function(df) {
  cat("===== Outlier Detection (Numeric Variables) =====\n")
  numeric_vars <- sapply(df, is.numeric)
  outlier_list <- list()
  
  for (col in names(df)[numeric_vars]) {
    x <- df[[col]]
    if (all(is.na(x))) next  # Skip columns that are completely NA
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_val <- IQR(x, na.rm = TRUE)
    lower_bound <- Q1 - 1.5 * IQR_val
    upper_bound <- Q3 + 1.5 * IQR_val
    outliers <- x[x < lower_bound | x > upper_bound]
    outlier_list[[col]] <- outliers
    
    if (length(outliers) > 0) {
      cat("Outliers in", col, ":\n")
      print(outliers)
      # Visualize outliers using ggplot2 if desired:
      ggplot(data.frame(x = x), aes(x = x)) +
        geom_boxplot(fill = "skyblue") +
        ggtitle(paste("Boxplot of", col)) +
        theme_minimal() -> p
      print(p)
    } else {
      cat("No significant outliers detected in", col, "\n")
    }
  }
  
  return(outlier_list)
}

# 6. Frequency Analysis for Categorical Variables
frequency_analysis <- function(df) {
  cat("===== Frequency Analysis (Categorical Variables) =====\n")
  categorical_vars <- sapply(df, function(x) is.character(x) || is.factor(x))
  for (col in names(df)[categorical_vars]) {
    cat("Frequency table for", col, ":\n")
    freq_tbl <- table(df[[col]], useNA = "ifany")
    print(freq_tbl)
    cat("\n")
  }
}

# 7. Master Pipeline: Data Quality System
data_quality_pipeline <- function(file_path) {
  cat("********** Data Quality Pipeline Start **********\n\n")
  
  # Step 1: Load & Prepare Data
  df <- load_and_prepare_data(file_path)
  
  # Step 2: Summarize Data
  summarize_data(df)
  
  # Step 3: Missing Value Analysis
  analyze_missing(df)
  
  # Step 4: Duplicate Check
  analyze_duplicates(df)
  
  # Step 5: Outlier Detection
  detect_outliers(df)
  
  # Step 6: Frequency Analysis for Categorical Variables
  frequency_analysis(df)
  
  cat("\n********** Data Quality Pipeline End **********\n")
  
  # Return the cleaned and processed data frame for further analysis
  return(df)
}

# Example usage:
# cleaned_df <- data_quality_pipeline("your_dataset.csv")
