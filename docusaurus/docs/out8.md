---
id: doc8
title: Parallel Annotation
sidebar_label: Parallel Annotation
---



## Parallel Annotation

I know your time is precious.

Annotations can take a while if you have quite some text to annotate. Luckily performing text annotation is trivially paralleliseable. In order to gain some of your precious time, the udpipe package integrates with the parallel package which is shipped by R-core. To run annotations in parallel, you can do as follows:

- Get your data as a data.frame with columns doc_id and text


```r
library(udpipe)
data(brussels_reviews)
x <- subset(brussels_reviews, language %in% "fr")
x <- data.frame(doc_id = x$id, text = x$feedback, stringsAsFactors = FALSE)
dim(x)
```

```
[1] 500   2
```

- Download the model if you don't have the model already


```r
model <- udpipe_download_model(language = "french")
```



- Provide the path to the model and give to udpipe the data.frame with columns *doc_id* and *text* and **on how many cores** you would like to execute the annotation.


```r
path <- model$file_model
path
```

```
[1] "C:/Users/Jan/Dropbox/Work/RForgeBNOSAC/BNOSAC/udpipe/vignettes/french-gsd-ud-2.3-181115.udpipe"
```

```r
annotation <- udpipe(x, path, parallel.cores = 2)
```

Don't forget to save your annotation such that you can load it back in later.


```r
saveRDS(annotation, file = "anno.rds")
annotation <- readRDS(file = "anno.rds")
```


## When to run in parallel?

It only makes sense to run annotation in parallel if you have many CPU cores and have enough data to annotate. As udpipe models are Rcpp pointers to the loaded models on disk which can not be passed on to the parallel threads, each thread will load the model again which takes some time next to the internal setup of the parallel backend. 

You can gain a speedup similar as the amount of cores you have on your machine. That can be done by using the parallel.cores argument wisely alongside parallel.chunksize argument which indicates the size of the chunks that the text data will be splitted into to perform the annotation (the default of this argument is set to the size of the data / parallel.cores). 


```r
system.time(annotation <- udpipe(x, path, parallel.cores = 2))
system.time(annotation <- udpipe(x, path, parallel.cores = 2, parallel.chunksize = 50))
system.time(annotation <- udpipe(x, path, parallel.cores = 1))
```

The following calls are also possible (pass on a character vector or a pre-tokenised list)


```r
x <- setNames(x$text, x$doc_id)
system.time(annotation <- udpipe(x, path, parallel.cores = 2))
x <- split(annotation$token, annotation$doc_id)
system.time(annotation <- udpipe(x, path, parallel.cores = 2))
```

## Using other packages allowing parallel computation

Another possibility is to use the future.apply R package. Below is a snippet how such a parallel annotation looks like with that package

```
library(udpipe)
library(data.table)
library(future.apply)
data(brussels_reviews)
x <- subset(brussels_reviews, language %in% "fr")
x <- data.frame(doc_id = x$id, text = x$feedback, stringsAsFactors = FALSE)
## Download the model
udpipe_download_model(language = "french-gsd", model_dir = getwd())

## Run annotation over 4 CPU cores
plan(multiprocess, workers = 4L)
anno <- split(x, seq(1, nrow(x), by = 50))
anno <- future_lapply(anno, FUN=function(x, ...) udpipe(x, "french-gsd", ...), model_dir = getwd())
anno <- rbindlist(anno)
```

> Enjoy the speed gain.

