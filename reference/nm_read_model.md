# Parse NONMEM model file into a list containing blocks of code

Parse NONMEM model file into a list containing blocks of code

## Usage

``` r
nm_read_model(modelfile = NULL, as_block = FALSE, code = NULL)
```

## Arguments

- modelfile:

  NONMEM model filename

- as_block:

  import code blocks as block of text (`TRUE`, default) or as separate
  lines (`FALSE`)

- code:

  NONMEM code (alternative to specifying file name)
