#' Summarize Life Years Lost at one specific age
#'
#' \code{summary} for objects of class \code{lyl_aggregated} summarizes Life Years Lost.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param object An object of class \code{lyl_aggregated} (obtained with function
#' \code{lyl_aggregated} or \code{lyl_aggregated_range}).
#'
#' @param decimals Number of decimals to be reported (default is 2).
#'
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return A table with the summary of the results.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_aggregated}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_aggregated_range}} for estimation of Life Years Lost for a range of different ages.}
#' }
#'
#' @references \itemize{
#'     \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
#'     \emph{PLoS ONE}. 2020;15(3):e0228073.}
#' }
#' @examples
#' # Load simulated data as example
#' data(aggreg_data)
#' data(pop_ref)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 70 years and before age 90 years
#' lyl_summary_data70 <- lyl_aggregated(data = aggreg_data, age = age, rates = rate,
#'                                      data0 = pop_ref, age0 = age, surv0 = survival,
#'                                      age_specific = 70, tau = 90)
#'
#' # Summarize and plot the data
#' summary(lyl_summary_data70)
#'

summary.lyl_aggregated <- function(object, decimals = 2, ...) {
  cat(paste0("Differences in estimates comparing '", object$data,
             "' with '", object$data0, "'.\n"))

  LYL <- object[["LYL"]]

  if (object$type == "age_specific") {
    cat(paste0("Estimates at age ", object[["age_specific"]], " years [maximum age tau = ", object[["tau"]], " years]"))
    LYL2 <- object$tau - object$age_specific - LYL
  } else {
    cat(paste0("Estimates at ages ", object[["age_begin"]], "-", object[["age_end"]], " years [maximum age tau = ", object[["tau"]], " years]"))
    LYL2 <- LYL[, c("TotalLYL", "TotalLYL0")]
    colnames(LYL2) <- c("life_exp", "life_exp0")
    LYL <- LYL[, c("life_exp", "life_exp0")]
  }


  LYL$category <- "Remaining life expectancy"
  LYL2$category <- "Total Life Years Lost"

  LYL <- rbind(LYL, LYL2)
  LYL$diff <- LYL$life_exp - LYL$life_exp0

  LYL <- LYL[, c("category", "life_exp", "life_exp0", "diff")]

  print(knitr::kable(LYL, digits = decimals, col.names = c("", object$data, object$data0, "Difference"),
                     format = "rst", align = "lrrr"))
  #cat("*Confidence intervals can be estimated with function 'lyl_ci'\n")

  return(invisible(LYL[1, ]))
}
