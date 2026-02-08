library(DBI)
library(odbc)

open_con <- function() {
  con <- dbConnect(
    odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = "ANTONIJO",
    Database = "RazvojIT",
    Trusted_Connection = "Yes"
  )
  

  if (!DBI::dbIsValid(con)) {
    stop("Konekcija prema SQL Serveru nije validna.")
  }
  
  con
}

