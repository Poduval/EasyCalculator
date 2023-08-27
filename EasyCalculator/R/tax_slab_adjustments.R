utils:::globalVariables(c(
  "data.taxslabs", "tax_regime", "financial_year", "slab.id", "slab.min", 
  "slab.max", "slab.percent", "slab.info"))

#' @title make_slab_adjustments
#' @rdname make_slab_adjustments
#' @name make_slab_adjustments
#' @description function to calculate tax between slabs
#' @param x taxable amount after basic rebate adjustments
#' @param slab_min slab minimum
#' @param slab_max slab maximum
#' @param slab_pct percentage of tax to be paid
#'
#' @examples
#' \dontrun{
#' library(taxcalculator)
#' make_slab_adjustments(2000000, 1200000, 1500000, 0.3)
#' make_slab_adjustments(2000000, 1500000, Inf, 0.3)
#' }
#'
#' @import plyr
#'
make_slab_adjustments <- function(x, slab_min = 0, slab_max = 0, slab_pct = 0) {

  slab_max <- pmax(slab_min, slab_max)
  slab_min[!is.finite(slab_min)] <- 0
  is_slab_max_finite <- is.finite(slab_max)
  x <- pmax(0, x - slab_min)
  x[is_slab_max_finite] <- pmin(
    x[is_slab_max_finite],
    slab_max[is_slab_max_finite] - slab_min[is_slab_max_finite])
  x * slab_pct
}

#' @title tax slabs for both old and new regime
#' @name data.taxslabs
#' @rdname data.taxslabs
#' @description tax slabs for both old and new regime
#'
#' @format data.taxslabs: A data frame (10, 7):
#' \describe{
#'   \item{slab.id}{slab identification number}
#'   \item{slab.min}{slab minimum amount}
#'   \item{slab.max}{slab maximum amount}
#'   \item{slab.percent}{tax percentage charged for the slab}
#'   \item{tax_regime}{old or new regime}
#'   \item{financial_year}{financial year}
#'   \item{slab.info}{comments or additional information}
#' }
#' @export
#'
data.taxslabs <- read.table(
  text = "
  slab.id|slab.min|slab.max|slab.percent|tax_regime|financial_year|slab.info
  1|0|300000|0|new|2024|no tax slab
  2|300000|600000|0.05|new|2024|NA
  3|600000|900000|0.1|new|2024|NA
  4|900000|1200000|0.15|new|2024|NA
  5|1200000|1500000|0.2|new|2024|NA
  6|1500000|NA|0.3|new|2024|fixed tax beyond
  1|0|250000|0|old|2024|no tax slab
  2|250000|500000|0.05|old|2024|NA
  3|500000|1000000|0.2|old|2024|NA
  4|1000000|NA|0.3|old|2024|fixed tax beyond", header = TRUE, sep = "|")

#' Example - For testing purpose
if (FALSE) {
  
  
  dd <- plyr::join(xx, data.taxslabs, by = key_columns, type = "inner")
  dd <- plyr::mutate(dd, tax_from_income_slab = make_slab_adjustments(
    taxable_amount, slab.min, slab.max, slab.percent))
  dd <- aggregate(tax_from_income_slab ~ id , data = dd, FUN = "sum")
  xx <- plyr::join(xx, dd, by = c("id"), type = "left")
  xx <- plyr::mutate(xx, tax_value = tax_from_income_slab)
  
}