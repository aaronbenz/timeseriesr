timeseries
==========
This is a work in progress R package that will help you calculate and plot time-series data at great speed. 

10 Million points can be process in ~300 ms, reducing it down to only the critical points required to conceptually view your data.

## To Download
Install package devtools if not installed
```r
install.packages("devtools")
```
Load devtools and Download timeseriesr package
```r
library(devtools)
install_github("aaronbenz/timeseries")
```

##Why use the timeseriesr package???
The goal of this package is to provide you with common time-series use cases while using more popular data types like data.frames/data.tables as opposed to ts/its/irts/timeSeries/ti/mts/zoo/xts, because honestly, who has time for that when data.frames are so easy to understand. None the less, I do like what xts package has been able to do and I recommend investigating it if you are about to dive deep into time-series analytics.

In the future, I will create a vignette to help you understand how to use this package, but for now, let me introduce you to the main function/algorithm that has inspired this work. 

`dtreduce` and `vreduce` gives you the ability to plot millions of points almost instantly by drastically reducing the total amount of points. In the internet of things, we can often encounter time-stamped sensor data that consists of more points than we have pixels, and there is no reason that we should have to plot millions of points in order to understand what is going on. Lets take a simple using a sin curve:

```r
###WARNING: DO NOT TRY TO PRINT/PLOT THIS
df <- data.frame(time = 1:5e6, value = sin(seq(1,10, length.out = 5e6)))
```
This simple curve is about 60 MBs. If you tried to do a simple `plot(df)`, you will find yourself waiting for minutes to view this simple plot, and that's if your lucky to escape without R crashing. Simple put, unacceptable. 

You could try a sampling method to get say 1 out of every 10 points, and in this case, it would work. However, you would ultimately be unconfident in your result because you could very likely be skipping critical spikes.

`dtreduce` solves this problem by gathering points of significance given a tolerance that you choose. So, for example:
```r
df_reduced <- dtreduce(df, column = "value")
library(ggplot2)
ggplot(df_reduced, aes(time, value)) + geom_step()
#OR
plot(df_reduced)
```

Oh, and by the way, visualizing 5 million points with accuracy only took about 190 milliseconds!!!
```r
library(microbenchmark)
microbenchmark(plot(dtreduce(df,"value")))
#Unit: milliseconds
#                        expr      min       lq     mean   median       uq      max neval
# plot(dtreduce(df, "value")) 120.7472 157.1538 190.1654 199.1449 215.9864 243.4482   100
```

If you are interested in contributing to this package and future development, have any comments or suggestions, feel free to contact me.

