# This source code is licenced under the MIT license found in the
# LICENSE file in the root directory of this source tree.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Read raw data
#
# Example usage of reference functions:
# source("../reference/download_index_tri.R", chdir=TRUE)
# nifty_50 = download_index_tri("NIFTY 50")

nifty_50 = download_index_tri("NIFTY 50")
nifty_next_50 = download_index_tri("NIFTY NEXT 50")
nifty_midcap_100 = download_index_tri("NIFTY MIDCAP 100")
nifty_smallcap_100 = download_index_tri("NIFTY SMALLCAP 100")


# Function to calculate rolling returns 
rolling_return = function(data, period, label) {
  data %>%
    group_by(y = year(date), m = month(date)) %>%
    summarise(date = last(date), value = last(value)) %>%
    ungroup %>%
    select(-c(y, m)) %>%
    mutate(m120 = ((value / lag(value, period)) ^ (12 / period) - 1) * 100) %>%
    rename(!!label := m120) %>%
    select(-value)
}


# Calculate rolling returns from each index
n50 = rolling_return(nifty_50, 120, "NIFTY 50 TRI")
nn50 = rolling_return(nifty_next_50, 120, "NIFTY NEXT 50 TRI")
nm100 = rolling_return(nifty_midcap_100, 120, "NIFTY MIDCAP 100 TRI")
ns100 = rolling_return(nifty_smallcap_100, 120, "NIFTY SMALLCAP 100 TRI")


# Combine rolling returns into single data frame and plot
n50 %>%
  left_join(nn50, by=c("date")) %>%
  left_join(nm100, by=c("date")) %>%
  left_join(ns100, by=c("date")) %>%
  gather(index, ann_return, -date) %>%
  ggplot(aes(x=date, y=ann_return, color=index)) +
  geom_line() +
  ggtitle("10y rolling returns") + 
  theme(legend.position="bottom",legend.direction="horizontal")

