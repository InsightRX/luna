# Get a very crude estimate for V to serve as initial estimate for CL and V, without performing an NCA. The calculation is based on the assumption that often in clinical trial data, there is at least a peak and a trough (and likely other samples) taken, hence it's possible to get a crude estimate for CL and V from that. For 2-compartment models we just set Q and V to half and twice the size of CL and V, which is often a good starting point. In most scenarios this is sufficiently close to the final estimates that estimation methods will be able to find the global minimum.

Get a very crude estimate for V to serve as initial estimate for CL and
V, without performing an NCA. The calculation is based on the assumption
that often in clinical trial data, there is at least a peak and a trough
(and likely other samples) taken, hence it's possible to get a crude
estimate for CL and V from that. For 2-compartment models we just set Q
and V to half and twice the size of CL and V, which is often a good
starting point. In most scenarios this is sufficiently close to the
final estimates that estimation methods will be able to find the global
minimum.

## Usage

``` r
get_initial_estimates_from_data(data, n_cmt = 1, scale_observations = NULL)
```

## Arguments

- data:

  NONMEM-style dataset

- n_cmt:

  number of distribution / elimination compartments.
