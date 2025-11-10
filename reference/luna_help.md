# Ask AI agent to help

By default it will pick the most recent error from NONMEM as prompt, but
can also ask questions.

## Usage

``` r
luna_help(prompt = NULL, model = "gpt-4.1-mini", verbose = TRUE, ...)
```

## Arguments

- prompt:

  question to ask LLM. By default NULL, which means it will look at the
  log and extract the last event, e.g. an error from NONMEM

- model:

  LLM, e.g. "gpt-4.1-mini"

- verbose:

  verbose output?

- ...:

  optional parameters passed onto ellmer::chat()

## Examples

``` r
if (FALSE) { # \dontrun{
luna_help() # will get help from LLM on last NONMEM error
luna_help("How do I write a model with non-linear elimination?")
} # }
```
