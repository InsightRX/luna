# Annotate a run Either add a note or clear notes from a run

Annotate a run Either add a note or clear notes from a run

## Usage

``` r
luna_note(
  id = NULL,
  note = NULL,
  clear = FALSE,
  force = TRUE,
  verbose = FALSE,
  element = "notes"
)
```

## Arguments

- id:

  run id, e.g. `run1`. This will be the folder in which the NONMEM model
  is run.

- note:

  note(s) to add (character vector)

- clear:

  should existing notes be cleared? Default is `FALSE`

- element:

  what element in the YAML contains the notes, default is `notes`.

## Examples

``` r
if (FALSE) { # \dontrun{
luna_note("run1", "Initial model") # adds a note
luna_note("run1", "Initial model", clear=TRUE) # first clears existing notes, then adds a note
luna_note("run1", clear=TRUE) # only clears existing notes
} # }
```
