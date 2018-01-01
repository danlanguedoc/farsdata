---
title: "farsdata Package Overview"
author: "Dan Languedoc"
date: "2018-01-01"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{farsdata Package Overview}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---



This vignette provides an overview of the farsdata package.  This package consists of 3 main
 and 2 helper functions designed to make using the Fatality Analysis Reporting System 
 a little easier

## Package Functions

the 3 main functions are the following

- `fars_read_years()`
- `fars_summarize_years()`
- `fars_map_state()`


"fars_read_years" allows you to read the csv data for one or more years.  The data is 
returned as a summarized list with one dataframe per input year.  Invalid years or years with no
value will return NA as that list element


```r
data <- fars_read(c(2013,2014))
```

"fars_summarize_years()"  takes a vector of the years to query and returns a dataframe 
consisting of summarized monthly data by year


```r
data <- fars_summarize_years(2013:2015)
```

"fars_map_state()" takes the summarized data and returns a plot for a given state index and year.
The index must map to an index in the dataset


```r
data <- fars_map_state(5,2014)
```


The datafile needs to be available in the extdata path in the project

