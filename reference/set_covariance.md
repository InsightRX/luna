# Function to set covariance between parameters in the omega block

One caveat is that it will remove any existing covariances, since
currently there is no feature in pharmr/pharmpy to extract the
covariance info.

## Usage

``` r
set_covariance(model, covariance)
```

## Arguments

- covariance:

  character vector specifying the parameters and initial value for the
  correlation between the respective parameters, e.g.
  `c("CL~V" = 0.1, "Q~V2" = 0.2)`.

## Value

Pharmpy model object
