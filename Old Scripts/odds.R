

library(httr)
library(jsonlite)
library(tidyverse)

Sys.setenv(API_KEY = "acaed33a86d9eff701cfa616e249c4e1")

api_key <- Sys.getenv("API_KEY")

sports_resp <- GET(
  "https://api.the-odds-api.com/v4/sports/",
  query = list(apiKey = api_key)
)

stop_for_status(sports_resp)

sports <- fromJSON(content(sports_resp, "text", encoding = "UTF-8"))

sports |> 
  






