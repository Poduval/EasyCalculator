
# Initialize ====
library(usethis)
library(devtools)

# Files to be ignored for packaging ====

use_build_ignore(c("^make_package.R$", "[.]Rproj$", 
                   "^backlog$", "^logo.png$"), escape = FALSE)

# Update description file ====
use_description(
  fields = list(
    Title = "Financial calculators",
    Version = "0.0.1",
    Description = "Easy to use financial calculators for India.",
    `Authors@R` = "person('Rakesh', 'Poduval', email = 'poduval.rakesh@icloud.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.6.3)",
    LazyData = TRUE
  ),
  roxygen = TRUE
)

use_package("stats", "Imports")
use_package("utils", "Imports")
use_package("plyr", "imports")
use_gpl_license(2)
use_github_links() # use this if this project is on github

# Datasets to be used in the package ====
# library(openxlsx)
# data.temp <- read.xlsx("Materials/data.xlsx", sheet = 1)
# use_data()
# save(data.temp, file = "EasyCalculators/data/data.RData")

# Documentation ====
use_logo("logo.png")
# use_vignette()

# Tests ====
# use_testthat()
# use_revdep()
# use_cran_comments()
use_github_action("check-standard")
use_github_action("test-coverage")
use_github_action("pkgdown")

# Packaging ====

document()
# test()
# build_vignettes()
check(manual = FALSE, cran = FALSE, quiet = FALSE)
build(quiet = FALSE, binary = FALSE, path = "release/")
install()

# install.packages("release/EasyCalculator_0.0.1.tar.gz", repos = NULL)
# devtools::install_github("Poduval/EasyCalculator")

if (FALSE) {
  # RUN IN A FRESH SESSION

  packageVersion(pkg = "EasyCalculator")
  packageDescription(pkg = "EasyCalculator")
  news(package = "EasyCalculator")

  library(EasyCalculators)
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
    arrange(tax_regime) -> x
  x
  tax_payable(x)
}

