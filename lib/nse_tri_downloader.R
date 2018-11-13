library(lubridate)
library(glue)
library(rvest)

# Earliest TR data is available for the Nifty 50 index from June 30, 1999
DATA_AVAILABLE_FROM <- as_date("1999-06-30")

URL <- function(index, from, to) {
  glue(
    'https://nseindia.com/products/dynaContent/equities/indices/total_returnindices.jsp?',
    'indexType={index}&',
    'fromDate={strftime(from, "%d-%m-%Y")}&', 
    'toDate={strftime(to, "%d-%m-%Y")}'
    ) %>%
    URLencode()
}

r <- read_html(URL("NIFTY%2050", DATA_AVAILABLE_FROM, as_date("2000-01-01"))) %>%
  html_nodes("td") %>%
  html_text() %>%
  head(-2) %>%
  matrix(ncol=2, byrow=T) %>%
  as_tibble()




