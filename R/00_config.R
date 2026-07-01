#################### packages #################### 

#install.packages(c("nrlR","elo","fuzzyjoin","tidyverse","tidymodels","here","DBI","odbc","httr","jsonlite","validate","janitor"))

library(nrlR)
library(elo)
library(fuzzyjoin)
library(tidyverse)
library(tidymodels)
library(here)
library(DBI)
library(odbc)
library(httr)
library(jsonlite)
library(validate)
library(janitor)

#con = odbc::dbConnect(
#  odbc::odbc(), 
#  Driver = "SQL Server",
#  Server = Sys.getenv("DB_SERVER"),
#  Database = Sys.getenv("DB_NAME"),
#  Trusted_Connection = "Yes"
#)