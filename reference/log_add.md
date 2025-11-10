# Add event to log

Add event to log

## Usage

``` r
log_add(
  event = c("action", "error", "plot"),
  action = "modelfit",
  id,
  context = NA
)
```

## Arguments

- event:

  type of event (`action` or `error`)

- action:

  action type, e.g. `modelfit`

- id:

  run id

- context:

  other information, e.g. error message
