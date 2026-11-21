# Method to calculate final payable tax

Income tax calculator. Returns the tax amount payable under the old or
new regime, for a numeric vector of incomes or a data frame.

default method for numeric vector input

Function to calculate final payable tax

## Usage

``` r
tax_payable(x, ...)

# Default S3 method
tax_payable(annual_income, financial_year = 2024, tax_regime = "new")

# S3 method for class 'data.frame'
tax_payable(x)
```

## Arguments

- x:

  annual income

- ...:

  additional parameters

- annual_income:

  annual income

- financial_year:

  financial year

- tax_regime:

  tax regime, old or new

## Examples

``` r
if (FALSE) { # \dontrun{
library(EasyCalculator)
tax_payable(1150000)
} # }
if (FALSE) { # \dontrun{
library(EasyCalculator)
x <- cbind.data.frame(id = c(1, 2, 3), annual_income = c(1400000, 2000000, 3000000))
tax_payable(x)
} # }
```
