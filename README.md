[![Build Status](https://travis-ci.org/aaronbenz/timeseriesr.svg?branch=master)](https://travis-ci.org/aaronbenz/timeseriesr)
[![Coverage Status](https://img.shields.io/coveralls/aaronbenz/timeseriesr.svg)](https://coveralls.io/r/aaronbenz/timeseriesr?branch=master)
[![codecov.io](http://codecov.io/github/aaronbenz/timeseriesr/coverage.svg?branch=master)](http://codecov.io/github/aaronbenz/timeseriesr?branch=master)
timeseries
==========
This is a work-in-progress R package that will help you calculate and plot time-series data at great speed. 

10 Million points can be processed in ~300 ms, reducing it down to only the critical points required to conceptually view your data.

## To Download
Install package devtools if not installed
```r
install.packages("devtools")
```
Load devtools and Download timeseriesr package
```r
library(devtools)
install_github("aaronbenz/timeseriesr")
```

##Why use the timeseriesr package?
The goal of this package is to provide you with common time-series use cases while using more popular data types like data.frames/data.tables, as opposed to ts/its/irts/timeSeries/ti/mts/zoo/xts, because honestly, who has time for that when data.frames are so easy to understand. Nonetheless, I do like what xts package has been able to do and I recommend investigating it if you are about to dive deep into time-series analytics.

In the future, I will create a vignette to help you understand how to use this package, but for now, let me introduce you to the main function/algorithm that has inspired this work. 

`dtreduce` and `vreduce` give you the ability to plot millions of points almost instantly by drastically reducing the total amount of points. In the so-called internet of things, we often encounter time-stamped sensor data sets that consist of more points than we have pixels, and there is no reason to plot millions of points in order to understand what is going on. Let's take a simple example using a sine curve:

```r
###WARNING: DO NOT TRY TO PRINT/PLOT THIS
df <- data.frame(time = 1:5e6, value = sin(seq(1,10, length.out = 5e6)))
```
This data set is about 60 MB. If you tried to do a `plot(df)`, you would find yourself waiting for minutes to view this plot, and that's if you're lucky to escape without R crashing. Really, just unacceptable.

You could try a sampling method to get 1 out of every 10 points, and in this case, it would work. However, you would ultimately be unconfident in your result because you could very likely be skipping critical spikes.

`dtreduce` solves this problem by gathering points of significance given a tolerance that you choose. So, for example:
```r
df_reduced <- dtreduce(df, column = "value")
library(ggplot2)
ggplot(df_reduced, aes(time, value)) + geom_step()
#OR
plot(df_reduced)
```

Note that visualizing 5 million points with accuracy only took about 190 milliseconds!!!

```r
library(microbenchmark)
microbenchmark(plot(dtreduce(df,"value")))
#Unit: milliseconds
#                        expr      min       lq     mean   median       uq      max neval
# plot(dtreduce(df, "value")) 120.7472 157.1538 190.1654 199.1449 215.9864 243.4482   100
```

If you are interested in contributing to this package and future development, have any comments or suggestions, feel free to contact me or to make a pull request.

