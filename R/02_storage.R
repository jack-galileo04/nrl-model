#################### writing data files to database #################### 

db_append <- function(connection, schema, table, df){
  
  DBI::dbWriteTable(
    connection,
    DBI::Id(schema = schema, table = table),
    df,
    append = TRUE
  )
}

db_write <- function(connection, schema, table, df){
  
  DBI::dbWriteTable(
    connection,
    DBI::Id(schema = schema, table = table),
    df,
    overwrite = TRUE
  )
}