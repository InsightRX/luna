# Add a tag to a run Either add a tag or clear tags from a run

Add a tag to a run Either add a tag or clear tags from a run

## Usage

``` r
luna_tag(id, tag = NULL, clear = FALSE, force = TRUE, verbose = FALSE)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- tag:

  tag to add (character vector)

- clear:

  should existing notes be cleared? Default is `FALSE`

## Examples

``` r
if (FALSE) { # \dontrun{
luna_tag("run1", "final") # adds a tag
luna_tag("run1", "intermediate", clear=TRUE) # first clears existing tags, then adds a tag
luna_tag("run1", clear=TRUE) # only clears existing tags
} # }
```
