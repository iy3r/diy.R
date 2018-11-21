# This source code serves as a reference implementation for 
# exploring more advanced string searching and data wrangling 
# algorithms and is presented for educational purposes only.
#
# Application of this source code may be governed by the terms
# and conditions of the service with which the end-user intends 
# to use it with. The end-user shall bear full responsibility of 
# adhering to any such terms.
#
# This source code is licenced under the MIT license found in the
# LICENSE file in the root directory of this source tree.

library(tidyverse)
library(lubridate)
library(glue)
library(rvest)
library(progress)

download_index_tri = function(index) {
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
  
  start = as_date("1999-06-30")
  end = today()
  periods = ceiling((end - start) %>% as.numeric() / 364)
  
  df = tibble(date = as_date(NA), value = numeric())  
  pb = progress_bar$new(total = periods)
  
  for(period in 1:periods) {
    pb$tick()
    result = function() { fetch(index, start, start + 364)}
    error = inherits(try(result(), silent=TRUE), 'try-error')
    if (error) {
      start = start + 365
    } else {
      df <- bind_rows(df, result())
      start = start + 365
    }
  }
  
  return(df)
}