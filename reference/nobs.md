# Number of observations in a given object

[`stats::nobs()`](https://rdrr.io/r/stats/nobs.html) is a generic
function that returns the number of observations from a model. permute
provides methods for several other types of R object.

## Usage

``` r
# S3 method for class 'numeric'
nobs(object, ...)

# S3 method for class 'integer'
nobs(object, ...)

# S3 method for class 'matrix'
nobs(object, ...)

# S3 method for class 'data.frame'
nobs(object, ...)

# S3 method for class 'character'
nobs(object, ...)

# S3 method for class 'factor'
nobs(object, ...)
```

## Arguments

- object:

  A data frame or matrix, or a numeric, integer, character, or factor
  vector.

- ...:

  Arguments passed to other methods.

## Value

The numeric number of observations in `object`.

## Details

These methods return the number of observations in numeric, integer,
character, or factor vectors, matrices, and data frames.

## Author

Gavin Simpson

## Examples

``` r
set.seed(1)
## numeric vector
len <- sample(1:10, 1)
v <- as.numeric(sample(1:100, len))
len
#> [1] 3
obs <- nobs(v)
isTRUE(all.equal(len, obs))
#> [1] TRUE

## integer
len <- sample(1L:10L, 1)
obs <- nobs(len)
isTRUE(all.equal(len, obs))
#> [1] FALSE
```
