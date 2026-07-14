#################### read/write data files to database #################### 

get_connection <- function() {
  odbc::dbConnect(
    odbc::odbc(),
    Driver = "SQL Server",
    Server = Sys.getenv("DB_SERVER"),
    Database = Sys.getenv("DB_NAME"),
    Trusted_Connection = "Yes"
  )
}

db_append <- function(schema, table, df){
  
  con <- get_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  cols <- DBI::dbListFields(
    con,
      DBI::Id(schema = schema, table = table)
      )
  
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = table),
    df |> select(any_of(cols)),
    append = TRUE
  )
}

db_write <- function(schema, table, df){
  
  con <- get_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  cols <- DBI::dbListFields(
    con,
    DBI::Id(schema = schema, table = table)
    )
  
  DBI::dbWriteTable(
    con,
    DBI::Id(schema = schema, table = table),
    df |> select(any_of(cols)),
    overwrite = TRUE
  )
}


db_read <- function(schema, table) {
  
  con <- get_connection()
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  DBI::dbReadTable(
    con,
    DBI::Id(schema = schema, table = table)
  )
  
}