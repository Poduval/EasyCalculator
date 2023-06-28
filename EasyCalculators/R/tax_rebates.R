utils:::globalVariables(c(
  "data.taxrebate", "data.taxrebate_under87A", "data.rebate_on_investments",
  "tax_regime", "financial_year",
  "rebate.id", "rebate.code", "rebate.limit", "rebate.info"))

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



