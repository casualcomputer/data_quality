library(data.table)

load_and_clean_data <- function(file_path) {
  # Load the data efficiently
  df <- fread(file_path)
  
  # Convert to a data frame (optional)
  df <- as.data.frame(df)
  
  # Replace missing text values with NA
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x[x == ""] <- NA
    }
    return(x)
  })
  
  # Standardize column names
  names(df) <- make.names(colnames(df), unique = TRUE)
  
  # Convert columns ending with "date" to Date format
  for (col in names(df)) {
    if (grepl("date$", col, ignore.case = TRUE)) { # Ensure it matches column names ending with "date"
      df[[col]] <- as.Date(df[[col]], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d-%m-%Y"))
    }
  }
  
  return(df)
}

# Example usage:
# df <- load_and_clean_data("combined_file_python.csv")


library(DBI)
library(odbc)

# Function to load a table from an ODBC connection
load_table_from_odbc <- function(datasource_name, table_name) {
  # Establish the ODBC connection
  con <- dbConnect(odbc::odbc(), datasource_name)
  
  # Construct the SQL query
  query <- paste0("SELECT * FROM ", table_name)
  
  # Fetch the table
  output_table <- dbGetQuery(con, query)
  
  # Close the connection
  dbDisconnect(con)
  
  return(output_table)
}

# Example usage:
# df <- load_table_from_odbc("MyDataSource", "my_table_name")
