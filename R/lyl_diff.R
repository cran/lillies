#' Summarize differences in Life Years Lost.
#'
#' \code{lyl_diff} summarizes differences in estimated Life Years Lost
#' in two different populations: \code{lyl_estimation} compared to \code{lyl_estimation0}.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param lyl_population1 Population of interest: An object of class \code{lyl} or \code{lyl_range} (obtained with
#' functions \code{lyl} or \code{lyl_range}). Alternatively, an object of class \code{lyl_ci} can be provided for
#' bootstrapped confidence intervals.
#'
#' @param lyl_population0 Reference population: An object of class \code{lyl} or \code{lyl_range} (obtained with
#' functions \code{lyl} or \code{lyl_range}). Alternatively, an object of class \code{lyl_ci} can be provided for
#' bootstrapped confidence intervals.
#'
#' @param decimals Number of decimals to be reported (default is 2).
#'
#' @param level Confidence level if \code{lyl_population1} or \code{lyl_population0} is obtained with the \code{lyl_ci}
#' function (default is 0.95 for 95\% confidence intervals)
#'
#' @param weights Vector with age distribution of disease/condition onset to be used when Life Years Lost are estimated
#' over a range of ages (with \code{lyl_range} function). If weights are not provided (dafault is \code{weights = NA}),
#' then the differences in Life Years Lost at each age is provided. If weights are provided, then a weighted average is provided.
#'
#' @return A table with the summary of the differences between two populations.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#' }
#'
#' @references \itemize{
#'     \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
#'     \emph{PLoS ONE}. 2020;15(3):e0228073.}
#' }
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' ### For the overall population and for those with a disease
#' diseased <- simu_data[!is.na(simu_data$age_disease), ]
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after age 45
#' # years and before age 95 years
#' lyl_estimation0 <- lyl(data = simu_data, t = age_death, status = cause_death,
#'                        age_specific = 45, tau = 95)
#' lyl_estimation1 <- lyl(data = diseased, t0 = age_disease,
#'                        t = age_death, status = cause_death,
#'                        age_specific = 45, tau = 95)
#' lyl_diff(lyl_estimation1, lyl_estimation0)
#'
#' \donttest{
#' # Calculate bootstrapped confidence interval (3 iterations to test)
#' lyl_estimation1_ci <- lyl_ci(lyl_estimation1, niter = 3)
#'
#' lyl_estimation0_ci <- lyl_ci(lyl_estimation0, niter = 3)
#' lyl_diff(lyl_estimation1_ci, lyl_estimation0_ci)
#'
#' # It is also possible to assume no uncertainty for one of the estimates
#' lyl_diff(lyl_estimation1_ci, lyl_estimation0)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after each age
#' # from 0 to 94 years and before age 95 years
#' lyl_estimation2 <- lyl_range(data = simu_data, t = age_death, status = cause_death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#' lyl_estimation3 <- lyl_range(data = diseased, t0 = age_disease,
#'                              t = age_death, status = cause_death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#' lyl_diff(lyl_estimation3, lyl_estimation2)
#' lyl_diff(lyl_estimation3, lyl_estimation2, weights = diseased$age_disease)
#'
#' # Calculate bootstrapped confidence interval (3 iterations to test)
#' lyl_estimation3_ci <- lyl_ci(lyl_estimation3, niter = 3)
#' lyl_diff(lyl_estimation3_ci, lyl_estimation2, weights = diseased$age_disease)
#' }

lyl_diff <- function(lyl_population1, lyl_population0, decimals = 2, level = 0.95, weights = NA) {

  if (!(class(lyl_population1) %in% c("lyl", "lyl_range", "lyl_ci")) | !(class(lyl_population0) %in% c("lyl", "lyl_range", "lyl_ci"))) {
    stop("The two objects must be obtained from the same function: either 'lyl' or 'lyl_range' (or subsequent 'lyl_ci').",
         call. = FALSE)
  }

  # Check for same tau and causes of death
  if ((lyl_population1[["tau"]] != lyl_population0[["tau"]])) {
    stop("The two objects must have the same 'tau'.",
         call. = FALSE)
  }

  if (!identical(colnames(lyl_population1[["LYL"]]), colnames(lyl_population0[["LYL"]]))) {
    stop("The two objects must have the same causes of death.",
         call. = FALSE)
  }

  parameters <- as.list(match.call.defaults()[-1])

  # Without confidence intervals
  if ((class(lyl_population1) == "lyl") & (class(lyl_population0) == "lyl")) {
    if (lyl_population1[["age_specific"]] != lyl_population0[["age_specific"]]) {
      stop("The two objects must have the same 'age_specific' parameter.",
           call. = FALSE)
    }
    age_begin <- NA
    age_end <- NA
    age_specific <- lyl_population1[["age_specific"]]
    return(lyl_diff_lyl(lyl_population1, lyl_population0, decimals, weights, age_specific, age_begin, age_end, parameters))
  }

  if ((class(lyl_population1) == "lyl_range") & (class(lyl_population0) == "lyl_range")) {
    if ((lyl_population1[["age_begin"]] != lyl_population0[["age_begin"]]) | (lyl_population1[["age_end"]] != lyl_population0[["age_end"]])) {
      stop("The two objects must have the same 'age_begin' and 'age_end'.",
           call. = FALSE)
    }
    age_begin <- lyl_population1[["age_begin"]]
    age_end <- lyl_population1[["age_end"]]
    age_specific <- NA
    return(lyl_diff_lyl(lyl_population1, lyl_population0, decimals, weights, age_specific, age_begin, age_end, parameters))
  }

  # With confidence intervals
  if ((class(lyl_population1) == "lyl_ci") & (class(lyl_population0) == "lyl_ci")) {

    niter1 <- lyl_population1[["niter"]]
    niter0 <- lyl_population0[["niter"]]
    niter <- niter0

    if (lyl_population1[["type"]] != lyl_population0[["type"]]) {
      stop("The two objects must be obtained from the same function: either 'lyl' or 'lyl_range' (or subsequent 'lyl_ci').",
           call. = FALSE)
    }

    if (lyl_population1[["type"]] == "age_range") {
      if ((lyl_population1[["age_begin"]] != lyl_population0[["age_begin"]]) | (lyl_population1[["age_end"]] != lyl_population0[["age_end"]])) {
        stop("The two objects must have the same 'age_begin' and 'age_end'.",
             call. = FALSE)
      }
      nages <- lyl_population1[["age_end"]] - lyl_population1[["age_begin"]] + 1
      age_begin <- lyl_population1[["age_begin"]]
      age_end <- lyl_population1[["age_end"]]
      age_specific <- NA

    } else {
      if (lyl_population1[["age_specific"]] != lyl_population0[["age_specific"]]) {
        stop("The two objects must have the same 'age_specific' parameter.",
             call. = FALSE)
      }
      nages <- 1
      age_begin <- NA
      age_end <- NA
      age_specific <- lyl_population1[["age_specific"]]
    }

    if (niter1 != niter0) {
      parameters <- as.list(match.call.defaults()[-1])
      message(paste0("'", as.character(parameters$lyl_population1), "' and '", as.character(parameters$lyl_population0), "' should be based on  the same number of bootstrap iterations."))
      message(paste0("'", as.character(parameters$lyl_population1), "' is based on ", niter1, "iterations."))
      message(paste0("'", as.character(parameters$lyl_population0), "' is based on ", niter0, "iterations."))

      if (niter1 < niter0) {
        message(paste0("The first ", (niter0 - niter1), " iterations of '", as.character(parameters$lyl_population1), "' are repeated to achieve the same number."))
        data_extra <- lyl_population1[["LYL_ci"]][1 : ((niter0 - niter1) * nages), ]
        data_extra$iteration <- niter1 + 1 + floor((0:((niter0 - niter1)*nages - 1)) / nages)
        lyl_population1[["LYL_ci"]] <- rbind(lyl_population1[["LYL_ci"]], data_extra)
        niter <- niter0
      } else {
        message(paste0("The first ", (niter1 - niter0), " iterations of '", as.character(parameters$lyl_population0), "' are repeated to achieve the same number."))
        data_extra <- lyl_population0[["LYL_ci"]][1 : ((niter1 - niter0) * nages), ]
        data_extra$iteration <- niter0 + 1 + floor((0:((niter1 - niter0)*nages - 1)) / nages)
        lyl_population0[["LYL_ci"]] <- rbind(lyl_population0[["LYL_ci"]], data_extra)
        niter <- niter1
      }
    }

    return(lyl_diff_lyl_ci(lyl_population1, lyl_population0, decimals, level, weights, type = lyl_population1[["type"]], age_specific, age_begin, age_end, niter, parameters))
  }

  # Age specific (one with confidence interval and the other without)
  if ((class(lyl_population1) == "lyl_ci") & (class(lyl_population0) == "lyl")) {

    if (lyl_population1[["type"]] == "age_specific") {
      if (lyl_population1[["age_specific"]] != lyl_population0[["age_specific"]]) {
        stop("The two objects must have the same 'age_specific' parameter.",
             call. = FALSE)
      }
      message(paste0("'", as.character(parameters$lyl_population0), "' is not based on boostrapped iterations (assumed estimate without uncertainty)."))
      lyl_population0[["LYL_ci"]] <-  lyl_population0[["LYL"]][rep(1, lyl_population1[["niter"]]), ]
      lyl_population0[["LYL_ci"]]$iteration <- rep(1:lyl_population1[["niter"]], each = 1)

      age_begin <- NA
      age_end <- NA
      age_specific <- lyl_population1[["age_specific"]]
      niter <- lyl_population1[["niter"]]
      type <- "age_specific"

      return(lyl_diff_lyl_ci(lyl_population1, lyl_population0, decimals, level, weights, type, age_specific, age_begin, age_end, niter, parameters))
    }
  }

  if ((class(lyl_population0) == "lyl_ci") & (class(lyl_population1) == "lyl")) {

    if (lyl_population0[["type"]] == "age_specific") {
      if (lyl_population1[["age_specific"]] != lyl_population0[["age_specific"]]) {
        stop("The two objects must have the same 'age_specific' parameter.",
             call. = FALSE)
      }
      message(paste0("'", as.character(parameters$lyl_population1), "' is not based on boostrapped iterations (assumed estimate without uncertainty)."))
      lyl_population1[["LYL_ci"]] <-  lyl_population1[["LYL"]][rep(1, lyl_population0[["niter"]]), ]
      lyl_population1[["LYL_ci"]]$iteration <- rep(1:lyl_population0[["niter"]], each = 1)

      age_begin <- NA
      age_end <- NA
      age_specific <- lyl_population0[["age_specific"]]
      niter <- lyl_population0[["niter"]]
      type <- "age_specific"

      return(lyl_diff_lyl_ci(lyl_population1, lyl_population0, decimals, level, weights, type, age_specific, age_begin, age_end, niter, parameters))
    }
  }

  # Age range (one with confidence interval and the other without)
  if ((class(lyl_population1) == "lyl_ci") & (class(lyl_population0) == "lyl_range")) {

    if (lyl_population1[["type"]] == "age_range") {
      if ((lyl_population1[["age_begin"]] != lyl_population0[["age_begin"]]) | (lyl_population1[["age_end"]] != lyl_population0[["age_end"]])) {
        stop("The two objects must have the same 'age_begin' and 'age_end'.",
             call. = FALSE)
      }
      message(paste0("'", as.character(parameters$lyl_population0), "' is not based on boostrapped iterations (assumed estimate without uncertainty)."))
      nages <- lyl_population1[["age_end"]] - lyl_population1[["age_begin"]] + 1
      lyl_population0[["LYL_ci"]] <-  lyl_population0[["LYL"]][rep(1:nrow(lyl_population0[["LYL"]]), lyl_population1[["niter"]]), ]
      lyl_population0[["LYL_ci"]]$iteration <- rep(1:lyl_population1[["niter"]], each = nages)

      age_begin <- lyl_population1[["age_begin"]]
      age_end <- lyl_population1[["age_end"]]
      age_specific <- NA
      niter <- lyl_population1[["niter"]]
      type <- "age_range"

      return(lyl_diff_lyl_ci(lyl_population1, lyl_population0, decimals, level, weights, type, age_specific, age_begin, age_end, niter, parameters))
    }
  }

  if ((class(lyl_population0) == "lyl_ci") & (class(lyl_population1) == "lyl_range")) {

    if (lyl_population0[["type"]] == "age_range") {
      if ((lyl_population1[["age_begin"]] != lyl_population0[["age_begin"]]) | (lyl_population1[["age_end"]] != lyl_population0[["age_end"]])) {
        stop("The two objects must have the same 'age_begin' and 'age_end'.",
             call. = FALSE)
      }
      message(paste0("'", as.character(parameters$lyl_population1), "' is not based on boostrapped iterations (assumed estimate without uncertainty)."))
      nages <- lyl_population1[["age_end"]] - lyl_population1[["age_begin"]] + 1
      lyl_population1[["LYL_ci"]] <-  lyl_population1[["LYL"]][rep(1:nrow(lyl_population1[["LYL"]]), lyl_population0[["niter"]]), ]
      lyl_population1[["LYL_ci"]]$iteration <- rep(1:lyl_population0[["niter"]], each = nages)

      age_begin <- lyl_population0[["age_begin"]]
      age_end <- lyl_population0[["age_end"]]
      age_specific <- NA
      niter <- lyl_population0[["niter"]]
      type <- "age_range"

      return(lyl_diff_lyl_ci(lyl_population1, lyl_population0, decimals, level, weights, type, age_specific, age_begin, age_end, niter, parameters))
    }
  }


  stop("The two objects must be obtained from the same function: either 'lyl' or 'lyl_range' (or subsequent 'lyl_ci').",
       call. = FALSE)
}
