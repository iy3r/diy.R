library(tidyverse)
library(lubridate)
library(glue)
library(rvest)

parse = function(text){
  dates = str_extract_all(text, "(?<=\")(\\d+-\\w+-\\d+)(?=\")")[[1]]
  values = str_extract_all(text, "(?<=\")(\\d+\\.?\\d*)(?=\")")[[1]]
  
  tibble(date = dates, value = values) %>%
    mutate(
      date = as.Date(date, format="%d-%b-%Y"),
      value = as.numeric(value)
      )
}

fetch = function(index, from, to) {
  glue(
    'https://nseindia.com/products/dynaContent/equities/indices/total_returnindices.jsp?',
    'indexType={index}&',
    'fromDate={strftime(from, "%d-%m-%Y")}&', 
    'toDate={strftime(to, "%d-%m-%Y")}'
  ) %>% 
    URLencode() %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="csvContentDiv"]') %>%
    html_text() %>%
    parse()
}

download = function(index) {
  start = as_date("1999-06-30")
  end = today()
  periods = ceiling((end - start) %>% as.numeric() / 364)
  
  df = tibble(date = as_date(NA), value = numeric())  
  for(period in 1:periods) {
    df = bind_rows(df, fetch(index, start, start + 364))
    start = start + 365
  }
  
  return(df)
}

df = download("NIFTY 50")

df
