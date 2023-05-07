
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
