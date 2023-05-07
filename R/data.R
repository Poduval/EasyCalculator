utils:::globalVariables(c(
  "data.taxslabs", "data.taxrebate", "data.taxrebate_under87A",
  "data.rebate_on_investments",
  "tax_regime", "financial_year",
  "slab.id", "slab.min", "slab.max", "slab.percent", "slab.info",
  "rebate.id", "rebate.code", "rebate.limit", "rebate.info"))

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

#' @title tax rebates
#' @name data.taxrebate
#' @rdname data.taxrebate
#' @description tax rebates before tax slab adjustments
#'
#' @format data.taxrebate: A data frame (11, 6):
#' \describe{
#'   \item{rebate.id}{rebate identification number}
#'   \item{rebate.code}{rebate code/short name}
#'   \item{rebate.limit}{maximum rebate}
#'   \item{tax_regime}{old or new regime}
#'   \item{financial_year}{financial year}
#'   \item{rebate.info}{comments or additional information}
#' }
#' @export
#'
data.taxrebate <- read.table(
  text = "
  rebate.id|rebate.code|rebate.limit|tax_regime|financial_year|rebate.info
  1|BASE|300000|new|2024|Basic exemption
  2|SD|50000|new|2024|Standard deduction
  1|BASE|200000|old|2024|basic exemption
  2|SD|50000|old|2024|standard deduction", header = TRUE, sep = "|")

#' @title tax rebates under step 2
#' @name data.rebate_on_investments
#' @rdname data.rebate_on_investments
#' @description tax rebates after tax slab adjustments
#'
#' @format data.rebate_on_investments: A data frame:
#' \describe{
#'   \item{rebate.id}{rebate identification number}
#'   \item{rebate.code}{rebate code/short name}
#'   \item{rebate.limit}{maximum rebate}
#'   \item{tax_regime}{old or new regime}
#'   \item{financial_year}{financial year}
#'   \item{rebate.info}{comments or additional information}
#' }
#' @export
#'
data.rebate_on_investments <- read.table(
  text = "
  rebate.id|rebate.code|rebate.limit|tax_regime|financial_year|rebate.info
  1|NPS|50000|old|2024|National pension scheme
  2|80C|150000|old|2024|Tax saving investments
  3|80D|150000|old|2024|Medical insurance
  4|24B|200000|old|2024|Home loan
  5|80TTA|10000|old|2024|80TTA", header = TRUE, sep = "|")

#' @title tax rebates
#' @name data.taxrebate_under87A
#' @rdname data.taxrebate
#' @description tax rebates before tax slab adjustments
#'
#' @format data.taxrebate: A data frame (11, 6):
#' \describe{
#'   \item{rebate.id}{rebate identification number}
#'   \item{rebate.code}{rebate code/short name}
#'   \item{rebate.limit}{maximum rebate}
#'   \item{tax_regime}{old or new regime}
#'   \item{financial_year}{financial year}
#'   \item{rebate.info}{comments or additional information}
#' }
#' @export
#'
data.taxrebate_under87A <- read.table(
  text = "
  rebate.id|rebate.code|rebate.limit|tax_regime|financial_year|rebate.info
  1|87A|25000|new|2024|87A
  2|87A|12500|old|2024|87A", header = TRUE, sep = "|")



