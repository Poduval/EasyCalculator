# ==== GLOBAL VARIABLES ====
utils:::globalVariables(c(
  "annual_income", "taxable_amount", "tax_value",
  "limit_under_basic_exmption",
  "rebate_under_87A",  "limit_under_87A",
  "rebate_under_investments",
  "tax_from_income_slab", "tax_surcharge",
  # DATASETS
  "data.taxrebate", "data.taxrebate_under87A", "data.rebate_on_investments",
  "tax_regime", "financial_year",
  "rebate.id", "rebate.code", "rebate.limit", "rebate.info",
  "data.taxslabs", "tax_regime", "financial_year", "slab.id", "slab.min", 
  "slab.max", "slab.percent", "slab.info"))

options(scipen = 9999)

# ==== FUNCTIONS ====

#' @rdname tax_payable
#' @name tax_payable
#' @title Method to calculate final payable tax
#' @description Returns tax amount payable
#' @param x annual income
#' @param ... additional parameters
#'
#' @export
#'
tax_payable <- function(x, ...) UseMethod("tax_payable")

#' @rdname tax_payable
#' @name tax_payable
#' @title Function to calculate final payable tax
#' @description default method for numeric vector input
#' @param annual_income annual income
#' @param financial_year financial year for which tax needs to be calculated
#' @param tax_regime new or old
#'
#' @examples
#' \dontrun{
#' library(taxcalculator)
#' tax_payable(1150000)
#' }
#' @export
#'
tax_payable.default <- function(annual_income, financial_year = 2024, 
                                tax_regime = "new") {

  stopifnot(length(tax_regime) == 1, length(financial_year) == 1,
            tax_regime %in% unique(data.taxslabs$tax_regime),
            financial_year %in% unique(data.taxslabs$financial_year))

  x <- data.frame(
    id = 1:length(annual_income),
    annual_income = annual_income,
    financial_year = financial_year,
    tax_regime = tax_regime)

  tax_payable.data.frame(x)
}

#' @rdname tax_payable
#' @name tax_payable
#' @title Function to calculate final payable tax
#' @description Function to calculate final payable tax
#' @param annual_income annual income
#' @param financial_year financial year
#' @param tax_regime tax regime, old or new
#'
#' @examples
#' \dontrun{
#' library(taxcalculator)
#' x <- cbind.data.frame(id = c(1, 2, 3), annual_income = c(1400000, 2000000, 3000000))
#' tax_payable(x)
#' }
#'
#' @import plyr
#' @importFrom stats aggregate reshape
#'
#' @export
#'
tax_payable.data.frame <- function(x) {

  if(!all(c("id", "annual_income") %in% names(x)))
    stop("Mandatory fields are missing: id, annual_income\n")

  # handle missing fields ====
  if(!"financial_year" %in% names(x)) x[, "financial_year"] <- 2024
  if(!"tax_regime" %in% names(x)) x[, "tax_regime"] <- "new"
  
  # validate & replacements for required fields ====
  x$financial_year[!x$financial_year %in% unique(data.taxslabs$financial_year)] <- 2024
  x$tax_regime[!x$tax_regime %in% unique(data.taxslabs$tax_regime)] <- "new"
  
  # defining output structure ====
  output <- data.frame(
    id = x$id,
    financial_year = x$financial_year,
    tax_regime = x$tax_regime,
    annual_income = x$annual_income,
    taxable_amount = NA_real_,
    tax_value = NA_real_,
    limit_under_basic_exmption = NA_real_,
    investments_declared = NA_character_,
    rebate_under_investments = NA_real_,
    limit_under_investments = NA_real_,
    tax_from_income_slab = NA_real_,
    rebate_under_87A = NA_real_,
    limit_under_87A = NA_real_,
    tax_surcharge = NA_real_)

  # merge investment declarations ====
  investment_declarations <- c("declared_NPS", "declared_80C","declared_80D",
                               "declared_24B", "declared_80TTA")
  
  if (any(investment_declarations %in% names(x))) {
    
    investment_declarations <- grep("^declared_", names(x), value = TRUE)
    inversment_section <- gsub("declared_", "", investment_declarations)
    output[, "investments_declared"] <-
      paste0(inversment_section[1], ":", x[, investment_declarations[1]])
    if(length(investment_declarations) > 1) {
      for(p in 2:length(investment_declarations)) {
        output[, "investments_declared"] <- paste(
          output[, "investments_declared"],
          paste0(inversment_section[p], ":", x[, investment_declarations[p]]),
          sep = ",")
      }
    }
  } else {
    output[, "investments_declared"] <- NA_character_
  }
  
  # calculation of payable tax ====
  key_vars <- c("tax_regime", "financial_year")

  ## tax rebates based on regime and financial year ====
 
  dd <- plyr::join(x, data.taxrebate, by = key_vars, type = "inner")
  dd <- aggregate(rebate.limit ~ id , data = dd, FUN = "sum")
  dd <- plyr::rename(dd, replace = c("rebate.limit" = "limit_under_basic_exmption"))
  xx <- plyr::join(x, dd, by = c("id"), type = "left")
  xx <- plyr::mutate(xx, taxable_amount = pmax(0, annual_income - limit_under_basic_exmption))

  ## tax rebates based on investment declaration ====
  ## these are valid only for 'old' regime
  
  if (any(investment_declarations %in% names(x))) {

    # prepare declared investments to be compared
    data.roi_wide <- stats::reshape(
      data.rebate_on_investments[, c(key_vars, "rebate.code", "rebate.limit")],
      direction = "wide", idvar = key_vars,
      timevar = "rebate.code")
    dd <- plyr::join(xx, data.roi_wide, by = key_vars, type = "inner")

    if (nrow(dd) >= 1) {
      
      invest_pos <- match(investment_declarations, names(dd))
      invest_pos <- invest_pos[!is.na(invest_pos)]
      limits_pos <- match(gsub("declared_", "", names(dd)[invest_pos]),
                          gsub("rebate.limit.", "", names(dd)))
      dd[, invest_pos] <- pmin(dd[, invest_pos], dd[, limits_pos])

      if(length(invest_pos) == 1) {
        dd[, "rebate_under_investments"] <- dd[, invest_pos]
        dd[, "limit_under_investments"] <- dd[, limits_pos]
        } else {
          dd[, "rebate_under_investments"] <- rowSums(dd[, invest_pos])
          dd[, "limit_under_investments"] <- rowSums(dd[, limits_pos])
        }
      dd <- dd[, c("id", "rebate_under_investments", "limit_under_investments")]
      xx <- plyr::join(xx, dd, by = c("id"), type = "left")
      xx <- plyr::mutate(
        xx,
        rebate_under_investments = ifelse(is.na(rebate_under_investments), 0, 
                                          rebate_under_investments),
        taxable_amount = pmax(0, taxable_amount - rebate_under_investments))
      }
    }

  ## tax based slab adjustments ====
  dd <- plyr::join(xx[, c("id", key_vars, "taxable_amount")], 
                   data.taxslabs, by = key_vars, type = "inner")
  dd <- plyr::mutate(dd, 
                     tax_from_income_slab = make_slab_adjustments(
                       taxable_amount, slab.min, slab.max, slab.percent))
  dd <- aggregate(tax_from_income_slab ~ id , data = dd, FUN = "sum")
  xx <- plyr::join(xx, dd, by = c("id"), type = "left")
  xx <- plyr::mutate(xx, tax_value = tax_from_income_slab)

  # tax adjustment with 87A ====
  # (single entry for given year & regime)
  # adding surcharges on top of total payable tax.

  xx <- plyr::join(xx, data.taxrebate_under87A[c(key_vars, "rebate.limit")],
                   by = key_vars, type = "left")
  xx <- plyr::rename(xx, replace = c("rebate.limit" = "limit_under_87A"))
  xx <- plyr::mutate(
    xx,
    rebate_under_87A = ifelse(
      taxable_amount <= 500000 + (tax_regime == "new") * 200000,
      pmin(tax_value, limit_under_87A), 0),
    limit_under_87A = ifelse(rebate_under_87A == 0, NA_real_, rebate_under_87A),
    tax_surcharge = 0.04,
    tax_value = pmax(0, tax_value - rebate_under_87A),
    tax_value = tax_value + tax_value * tax_surcharge)

  # output structure ====
  update_rows <- match(output$id, xx$id)
  update_cols <- intersect(names(output), names(xx))
  output[update_rows, update_cols] <- xx[, update_cols]

  structure(output, class = c("data.frame", "ITR"))

}

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

# ==== METHODS ====

#' print.ITR
#' @description print method for class "ITR"
#' @param x a ITR class object
#' @param partial logical, whether to print only a subset of columns
#' @param ... additional parameters for print.data.frame
#' @export
#'
print.ITR <- function(x, partial = TRUE, ...) {
  if (partial) {
    cols_to_print <- c("id", "taxable_amount", "tax_value", "financial_year")
    print(x[, cols_to_print], ...)
  } else print(x, ...)
}

# ==== DATA =====

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
