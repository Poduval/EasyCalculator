# make_slab_adjustments

function to calculate tax between slabs

## Usage

``` r
make_slab_adjustments(x, slab_min = 0, slab_max = 0, slab_pct = 0)
```

## Arguments

- x:

  taxable amount after basic rebate adjustments

- slab_min:

  slab minimum

- slab_max:

  slab maximum

- slab_pct:

  percentage of tax to be paid

## Examples

``` r
if (FALSE) { # \dontrun{
library(EasyCalculator)
make_slab_adjustments(2000000, 1200000, 1500000, 0.3)
make_slab_adjustments(2000000, 1500000, Inf, 0.3)
} # }
```
