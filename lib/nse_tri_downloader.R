library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

# Earliest TR data is available for the Nifty 50 index from June 30, 1999
DATA_AVAILABLE_FROM <- as_date("1999-06-30")

buildUrl <- function(index, from, to) {
  glue(
    'https://nseindia.com/products/dynaContent/equities/indices/total_returnindices.jsp?',
    'indexType={index}&',
    'fromDate={strftime(from, "%d-%m-%Y")}&', 
    'toDate={strftime(to, "%d-%m-%Y")}'
    ) %>%
    URLencode()
}

fetchData <- function(url) {
  url %>%
    read_html() %>%
    html_nodes("td") %>%
    html_text() %>%
    head(-2) %>%
    matrix(ncol=2, byrow=T)
}

fromDates <- seq(ymd('1999-06-30'), today(), by = '364 days')
toDates <- c(tail(fromDates, -1) - days(1), today())

for (i in seq_along(fromDates)) {
  print(buildUrl("NIFTY 50", fromDates[i], toDates[i]))
}







r <- 
  buildUrl("NIFTY%2050", DATA_AVAILABLE_FROM, DATA_AVAILABLE_FROM + days(364)) %>%
  read_html() %>%
  html_nodes("td") %>%
  html_text() %>%
  head(-2) %>%
  matrix(ncol=2, byrow=T) %>%
  as_tibble() %>%
  rename(Date = V1, Value = V2) %>%
  mutate(Date = as.Date(Date, format="%d-%b-%Y"))

r
