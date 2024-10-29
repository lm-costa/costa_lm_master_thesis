
# 

## **Loading internal functions**

``` r
functions_files <- list.files('r/functions/',full.names = T)
purrr::map(functions_files,source)
```
