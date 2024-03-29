% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tax_calculator_functions.R
\name{tax_payable}
\alias{tax_payable}
\alias{tax_payable.default}
\alias{tax_payable.data.frame}
\title{Method to calculate final payable tax}
\usage{
tax_payable(x, ...)

\method{tax_payable}{default}(annual_income, financial_year = 2024, tax_regime = "new")

\method{tax_payable}{data.frame}(x)
}
\arguments{
\item{x}{annual income}

\item{...}{additional parameters}

\item{annual_income}{annual income}

\item{financial_year}{financial year}

\item{tax_regime}{tax regime, old or new}
}
\description{
Returns tax amount payable

default method for numeric vector input

Function to calculate final payable tax
}
\examples{
\dontrun{
library(taxcalculator)
tax_payable(1150000)
}
\dontrun{
library(taxcalculator)
x <- cbind.data.frame(id = c(1, 2, 3), annual_income = c(1400000, 2000000, 3000000))
tax_payable(x)
}

}
