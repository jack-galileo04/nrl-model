#################### SQL Sever database connection #################### 

db_connect <- function(){
  DBI::dbConnect(
    odbc::odbc(),
    driver = "SQL Server",
    server = "JACKS_L5\\SQLEXPRESS",
    database = "nrl_model",
    trusted_connection = "yes"
  )
}

#################### writing data files to database #################### 

db_write <- function(connection, schema, table, df){
  
  DBI::dbWriteTable(
    connection,
    DBI::Id(schema = schema, table = table),
    df,
    append = TRUE
  )
}
