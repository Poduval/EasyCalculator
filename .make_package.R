# Initialize ====

library(devtools)
pkg <- normalizePath("EasyCalculators", mustWork = TRUE)

document(pkg)
check(pkg, quiet = FALSE)
build(pkg, path = "5-Release/", quiet = FALSE, binary = FALSE)
install(pkg)

# install.packages("release/EasyCalculator_23.5.0.tar.gz", repos = NULL)

if (FALSE) {
  # RUN IN A FRESH SESSION

  packageVersion(pkg = "EasyCalculator")
  packageDescription(pkg = "EasyCalculator")
  news(package = "EasyCalculator")

  library(EasyCalculator)
  options(scipen = 999)

  tax_payable(2500000)
  tax_payable(2500000, tax_regime = "old")
  tax_payable(seq(1000000, 3000000, 500000))

  library(dplyr)
  expand.grid(tax_regime = c("old", "new"),
              annual_income = c(1000000, 3000000, 500000),
              stringsAsFactors = FALSE) %>%
    mutate(id = 1:n(),
           declared_80C = annual_income * 0.10,
           declared_NPS = pmax(50000, annual_income * 0.05)) %>%
    arrange(tax_regime) %>%
    tax_payable()

}

