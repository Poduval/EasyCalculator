utils:::globalVariables(c(
  "annual_income", "taxable_amount", "tax_value",
  "limit_under_basic_exmption",
  "rebate_under_87A",  "limit_under_87A",
  "rebate_under_investments",
  "tax_from_income_slab", "tax_surcharge"))

options(scipen = 9999)

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
tax_payable.default <- function(annual_income, financial_year = 2024, tax_regime = "new") {

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
    stop("Missing mandatory fields: id, annual_income\n")

  # handling missing columns ====
  missing_columns <- setdiff(c("financial_year", "tax_regime"), names(x))
  if(!"financial_year" %in% names(x)) x[, "financial_year"] <- 2024
  if(!"tax_regime" %in% names(x)) x[, "tax_regime"] <- "new"

  # validating columns ====
  x$tax_regime[!x$financial_year %in% unique(data.taxslabs$financial_year)] <- 2024
  x$tax_regime[!x$tax_regime %in% unique(data.taxslabs$tax_regime)] <- "new"

  # defining result structure ====
  # Consolidating investment declarations for output
  # x$declared_NPS <- c(40000, 60000, 50000)
  # x$declared_80C <- c(90000, 140000, 160000)

  investment_declarations <- c("declared_NPS", "declared_80C","declared_80D",
                               "declared_24B", "declared_80TTA")

  if (any(investment_declarations %in% names(x))) {

    investment_declarations <- grep("^declared_", names(x), value = TRUE)
    inversment_section <- gsub("declared_", "", investment_declarations)
    x[, "declared_under_investments"] <-
      paste0(inversment_section[1], ":", x[, investment_declarations[1]])
    if(length(investment_declarations) > 1) {
      for(p in 2:length(investment_declarations)) {
        x[, "declared_under_investments"] <- paste(
          x[, "declared_under_investments"],
          paste0(inversment_section[p], ":", x[, investment_declarations[p]]),
          sep = ",")
      }
    }
  } else {
    x[, "declared_under_investments"] <- NA_character_
  }

  output <- data.frame(
    id = x$id,
    financial_year = x$financial_year,
    tax_regime = x$tax_regime,
    annual_income = x$annual_income,
    tax_value = NA_real_,
    taxable_amount = NA_real_,
    limit_under_basic_exmption = NA_real_,
    declared_under_investments = x$declared_under_investments,
    rebate_under_investments = NA_real_,
    limit_under_investments = NA_real_,
    tax_from_income_slab = NA_real_,
    rebate_under_87A = NA_real_,
    limit_under_87A = NA_real_,
    tax_surcharge = NA_real_)

  # calculation of payable tax ====
  key_columns <- c("tax_regime", "financial_year")

  ## tax rebates based on regime and financial year ====
  if (all(!c(key_columns, "annual_income") %in% names(x)))
    stop("Required columns are not present in the data \n")
  dd <- plyr::join(x, data.taxrebate, by = key_columns, type = "inner")
  dd <- aggregate(rebate.limit ~ id , data = dd, FUN = "sum")
  dd <- plyr::rename(dd, replace = c("rebate.limit" = "limit_under_basic_exmption"))
  xx <- plyr::join(x, dd, by = c("id"), type = "left")
  xx <- plyr::mutate(xx, taxable_amount = pmax(0, annual_income - limit_under_basic_exmption))

  ## tax rebates based on investment declaration ====

  if (any(investment_declarations %in% names(x))) {

    # prepare declared investments to be compared
    invest_var <- c(key_columns, "rebate.code", "rebate.limit")
    data.roi_wide <- stats::reshape(data.rebate_on_investments[invest_var],
                                     direction = "wide", idvar = key_columns,
                                     timevar = "rebate.code")
    dd <- plyr::join(xx, data.roi_wide, by = key_columns, type = "inner")

    if (nrow(dd) >= 1) {
      invest_pos <- match(investment_declarations, names(dd))
      invest_pos <- invest_pos[!is.na(invest_pos)]
      limits_pos <- match(gsub("declared_", "", names(dd)[invest_pos]),
                          gsub("rebate.limit.", "", names(dd)))

      dd[, invest_pos] <- pmin(dd[, invest_pos], dd[, limits_pos])

      if(length(invest_pos) == 1) {
        dd[, "rebate_under_investments"] <- dd[, invest_pos]
        } else dd[, "rebate_under_investments"] <- rowSums(dd[, invest_pos])

      if(length(limits_pos) == 1) {
        dd[, "limit_under_investments"] <- dd[, limits_pos]
      } else dd[, "limit_under_investments"] <- rowSums(dd[, limits_pos])

      dd <- dd[, c("id", "rebate_under_investments", "limit_under_investments")]
      xx <- plyr::join(xx, dd, by = c("id"), type = "left")
      xx <- plyr::mutate(
        xx,
        rebate_under_investments = ifelse(is.na(rebate_under_investments), 0, rebate_under_investments),
        taxable_amount = pmax(0, taxable_amount - rebate_under_investments))
      }
    }

  ## tax based slab adjustments ====
  dd <- plyr::join(xx, data.taxslabs, by = key_columns, type = "inner")
  dd <- plyr::mutate(dd, tax_from_income_slab = make_slab_adjustments(
    taxable_amount, slab.min, slab.max, slab.percent))
  dd <- aggregate(tax_from_income_slab ~ id , data = dd, FUN = "sum")
  xx <- plyr::join(xx, dd, by = c("id"), type = "left")
  xx <- plyr::mutate(xx, tax_value = tax_from_income_slab)

  # tax adjustment with 87A ====
  # (single entry for given year & regime)
  # adding surcharges on top of total payable tax.

  xx <- plyr::join(xx, data.taxrebate_under87A[c(key_columns, "rebate.limit")],
                   by = key_columns, type = "left")
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
