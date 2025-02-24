library(odbc)
library(DBI)

#' Read and Format an SQL Script from a File
#'
#' @param filepath A string specifying the path to the SQL script file.
#' @return A cleaned SQL query string.
#' @examples
#' sql_query <- read_sql_file("path/to/query.sql")
read_sql_file <- function(filepath) {
  con <- file(filepath, "r") # Open file for reading
  sql_string <- ""
  
  while (TRUE) {
    line <- readLines(con, n = 1, warn = FALSE)
    if (length(line) == 0) {
      break
    }
    line <- gsub("\\t", " ", line)  # Replace tabs with spaces
    if (grepl("--", line)) {
      line <- paste(sub("--", "/*", line), "*/") # Convert SQL comments to block comments
    }
    sql_string <- paste(sql_string, line)
  }
  
  close(con)
  return(sql_string)
}

#' Execute an SQL Script and Return a Data Frame
#'
#' @param odbc_conn An active ODBC connection object.
#' @param sql_query A string containing the SQL query to execute.
#' @return A data frame containing the query results.
#' @examples
#' con <- dbConnect(odbc::odbc(), "MyDatabase")
#' data <- execute_sql(con, "SELECT * FROM my_table")
#' dbDisconnect(con)
execute_sql <- function(odbc_conn, sql_query) {
  if (!dbIsValid(odbc_conn)) {
    stop("Invalid ODBC connection.")
  }
  
  result <- tryCatch({
    query_result <- dbSendQuery(odbc_conn, sql_query)
    data <- dbFetch(query_result, n = -1) # Fetch all rows
    dbClearResult(query_result)
    return(data)
  }, error = function(e) {
    warning("SQL execution failed: ", conditionMessage(e))
    return(NULL)
  })
  
  return(result)
}

#' Load Table Using an SQL Script
#'
#' @param odbc_dsn The ODBC Data Source Name (DSN).
#' @param sql_file_path Path to the SQL script file.
#' @return A data frame containing the query results.
#' @examples
#' data <- load_table_from_sql("MyDatabase", "query.sql")
load_table_from_sql <- function(odbc_dsn, sql_file_path) {
  con <- dbConnect(odbc::odbc(), dsn = odbc_dsn)
  
  # Ensure the connection is valid
  if (!dbIsValid(con)) {
    stop("Failed to connect to the database.")
  }
  
  sql_query <- read_sql_file(sql_file_path) # Read and format SQL query
  data <- execute_sql(con, sql_query) # Execute query and fetch data
  
  dbDisconnect(con) # Close connection
  return(data)
}

odbc_dsn <- "MyDatabase"  # Replace with your DSN
sql_file_path <- "path/to/your/query.sql"  # Replace with the actual path

table_data <- load_table_from_sql(odbc_dsn, sql_file_path)
print(table_data)
