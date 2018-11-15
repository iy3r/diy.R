library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

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
    head(-1) %>%
    matrix(ncol=2, byrow=T) %>%
    as_tibble()
}

triData <- function(index) {
  DATA_AVAILABLE_FROM <- as_date("1999-06-30")
  fromDates <- seq(ymd('1999-06-30'), today(), by = '100 days')
  toDates <- c(tail(fromDates, -1) - days(1), today())
  datalist <- list()
  
  for (i in seq_along(fromDates)) {
    tryCatch({
      url <- buildUrl(index, fromDates[i], toDates[i])
      datalist[[i]] <- fetchData(url)
    }, warning = function(w) {
      message(paste(w, " : ", url))
    }, error = function(e) {
      message(paste(e, " : ", url))
    })
  }
  
  bind_rows(datalist) %>%
    rename(Date = V1, Value = V2) %>%
    mutate(Date = as.Date(Date, format="%d-%b-%Y"))
}

data <- triData('NIFTY 50')
