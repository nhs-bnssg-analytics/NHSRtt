# Pivot the parameters passed into the function so relationships between the parameters remain consistent but giving control to providing more or less focus on extreme bins

Pivot the parameters passed into the function so relationships between
the parameters remain consistent but giving control to providing more or
less focus on extreme bins

## Usage

``` r
apply_parameter_skew(params, skew, skew_method = "rotate", pivot_bin = NULL)
```

## Arguments

- params:

  numeric vector of parameters; assumed to be in order of increasing
  waiting times (bins)

- skew:

  numeric; length 1, a multiplier to be used on the final parameter. A
  skew of 1 will keep the params identical to the input params. Value
  must be greater than 0

- skew_method:

  character; one of "rotate" or "uniform". The "rotate" method will
  multiply the highest bin by the skew value, the pivot_bin will remain
  the same as its input, and all of the other bins between the pivot bin
  and the highest bin will be multiplied by an interpolated value
  between 1 and the skew value. Bins between the second bin and the
  pivot bin will be multiplied by interpolated value between (1 / skew)
  and 1. If the method is "uniform", then all of the bins greater than
  or equal to the pivot bin will be multiplied by the skew value, and
  the bins below the pivot bin will be multiplied by (1 / skew)

- pivot_bin:

  numeric; when applying the skew, whichever method, select a bin to
  pivot around. If NULL, the mid-bin between the highest available bin
  and the second bin will be used. The first item of the parameter
  inputs is bin 0, so a pivot_bin of 2, will be the third item

## Value

a revised numeric vector of parameters

## Details

The skew parameter is applied to the final item of the params vector.
The inverse of the skew parameter is applied to the second item of the
params vector. The adjustment made to the items in between item to and
item n are calculated by a linear extrapolation between the inverse skew
and the skew. The first item is held constant.

## Examples

``` r
apply_parameter_skew(
  params = c(0.03, 0.02, 0.02, 0.01, 0.04, 0.05),
  skew = 1.05
)
#> [1] 0.03000000 0.01904762 0.01952381 0.01000000 0.04100000 0.05250000
```
