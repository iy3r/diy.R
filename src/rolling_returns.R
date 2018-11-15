setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("../lib/download_tri.R", chdir=T)
library(timetk)
library(PerformanceAnalytics)


data <- download_tri("NIFTY 50") 

df <- data %>% 
  tk_xts(select = Value, date_var = Date)

returns <- Return.calculate(df[endpoints(df, on="months", k=1)])
returns <- returns[-1, ]

charts.RollingPerformance(R = returns, width = 120)

