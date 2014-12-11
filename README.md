timeseries
==========
This is a work in progress R package that will help you calculate and plot time-series data at great speed. 

10 Million points can be process in ~300 ms, reducing it down to only the critical points required to conceptually view your data.

## To Download
Install package devtools if not installed
```R
install.packages("devtools")
```
Load devtools and Download timeseriesr package (auth_token is for private repos only)
```R
library(devtools)
install_github("aaronbenz/timeseries", auth_token = "e8893981609f88e0c682e7f8b1b41aae35fe412d")
```
