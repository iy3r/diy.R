setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../library/download_index_tri.R", chdir=T)
library(tidyverse)

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

nifty_50 = download_index_tri("NIFTY 50")
nifty_next_50 = download_index_tri("NIFTY NEXT 50")
nifty_midcap_100 = download_index_tri("NIFTY MIDCAP 100")
nifty_smallcap_100 = download_index_tri("NIFTY SMALLCAP 100")

n50 = rolling_return(nifty_50, 120, "NIFTY 50 TRI")
nn50 = rolling_return(nifty_next_50, 120, "NIFTY NEXT 50 TRI")
nm100 = rolling_return(nifty_midcap_100, 120, "NIFTY MIDCAP 100 TRI")
ns100 = rolling_return(nifty_smallcap_100, 120, "NIFTY SMALLCAP 100 TRI")


n50 %>%
  left_join(nn50, by=c("date")) %>%
  left_join(nm100, by=c("date")) %>%
  left_join(ns100, by=c("date")) %>%
  gather(index, ann_return, -date) %>%
  ggplot(aes(x=date, y=ann_return, color=index)) +
  geom_line() +
  ggtitle("10y rolling returns") + 
  theme(legend.position="bottom",legend.direction="horizontal")

