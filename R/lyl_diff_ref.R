#' Summarize differences in Life Years Lost.
#'
#' \code{lyl_diff} summarizes differences in estimated Life Years Lost
#' in two different populations: \code{lyl_estimation1} compared to a life table provided in \code{data_ref}.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param lyl_population1 Population of interest: An object of class \code{lyl} or \code{lyl_range} (obtained with
#' functions \code{lyl} or \code{lyl_range}). Alternatively, an object of class \code{lyl_ci} can be provided for
#' bootstrapped confidence intervals.
#'
#' @param data_ref A dataframe, where each raw represents an age, for the population of
#' reference The dataframe will contain information on age-specific mortality rates
#' or survivial probability (if both parameters are provided, rates will be used).
#' @param age Variable in \code{data_ref} containing information on age.
#' @param rates Variable in \code{data_ref} containing information on age-specific mortality rates.
#' @param surv Variable in \code{data_ref} containing information on age-specific survival probability.
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
#' @param lyl_population0 Parameter automatically created.
#'
#' @return A table with the summary of the differences between two populations.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#' data(pop_ref)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after age 45
#' # years and before age 95 years for those with a disease
#' diseased <- simu_data[!is.na(simu_data$age_disease), ]
#' lyl_estimation1 <- lyl(data = diseased, t0 = age_disease,
#'                        t = age_death, status = cause_death,
#'                        age_specific = 45, tau = 95)
#' lyl_diff_ref(lyl_estimation1, pop_ref, age = age, surv = survival)
#' lyl_diff_ref(lyl_estimation1, pop_ref, age = age, rates = mortality_rates)
#'
#' # Calculate bootstrapped confidence interval (3 iterations to test; more are necessary)
#' lyl_estimation1_ci <- lyl_ci(lyl_estimation1, niter = 3)
#' lyl_diff_ref(lyl_estimation1_ci, pop_ref, age = age, surv = survival)
#'
#' \donttest{
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after each age
#' # from 0 to 94 years and before age 95 years
#' lyl_estimation2 <- lyl_range(data = diseased, t0 = age_disease,
#'                              t = age_death, status = cause_death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#' lyl_diff_ref(lyl_estimation2, pop_ref, age = age, surv = survival)
#' lyl_diff_ref(lyl_estimation2, pop_ref, age = age, surv = survival, weights = diseased$age_disease)
#' }
#'

lyl_diff_ref <- function(lyl_population1, data_ref, age, surv, rates, decimals = 2, level = 0.95, weights = NA, lyl_population0) {

  if (!(class(lyl_population1) %in% c("lyl", "lyl_range", "lyl_ci"))) {
    stop("Object 'lyl_population1' must be obtained from one of the following functions: 'lyl' or 'lyl_range' (or subsequent 'lyl_ci').",
         call. = FALSE)
  }

  parameters <- as.list(match.call.defaults()[-1])
  parameters$lyl_population0 <- as.character(parameters$data_ref)

  tmp0 <- aggregate_prepare_data(data_ref, age, surv, rates, tau = lyl_population1$tau,
                                 parameters, names = c("data_ref", "age", "surv", "rates"))

  #Remove competing risks (not available in life tables)
  if (lyl_population1[["competing_risks"]]) {
    lyl_population1[["competing_risks"]] <- 0
    lyl_population1[["death_labels"]] <- "Dead"

    LYL1 <- lyl_population1$LYL[, c("age", "life_exp")]
    LYL1$x <- lyl_population1[["tau"]] - LYL1$age - LYL1$life_exp
    LYL1 <- LYL1[, c("age", "x", "life_exp")]
    colnames(LYL1) <- c("age", lyl_population1[["death_labels"]], "life_exp")
    lyl_population1[["LYL"]] <- LYL1

    if (class(lyl_population1) == "lyl_ci") {
      LYL1 <- lyl_population1$LYL_ci[, c("age", "life_exp", "iteration")]
      LYL1$x <- lyl_population1[["tau"]] - LYL1$age - LYL1$life_exp
      LYL1 <- LYL1[, c("age", "x", "life_exp", "iteration")]
      colnames(LYL1) <- c("age", lyl_population1[["death_labels"]], "life_exp", "iteration")
      lyl_population1[["LYL_ci"]] <- LYL1
    }
  }


  ####### Age_specific
  if ((class(lyl_population1) == "lyl") |
      ((class(lyl_population1) == "lyl_ci") & !is.null(lyl_population1[["type"]]) & lyl_population1[["type"]] == "age_specific")
      ) {

    age_begin <- NA
    age_end <- NA
    age_specific <- lyl_population1[["age_specific"]]

    # Check that all data for reference population is available
    if (sum(!(age_specific:(lyl_population1[["tau"]] - 1) %in% tmp0$time)) > 0) {
      stop(paste0("Mortality rates or survival for each age from ", age_specific,
                  " until ", (lyl_population1[["tau"]] - 1), " must be supplied for the reference population ('data_ref')."),
           call. = FALSE)
    }

    tmp0 <- tmp0[tmp0$time >= age_specific, ]
    tmp0$S <- tmp0$S / (tmp0$S[tmp0$time == age_specific])

    LYL <- data.frame(cbind(
      age = age_specific,
      x = NA,
      life_exp = pracma::trapz(tmp0$time, tmp0$S)
    ))
    LYL$x <- lyl_population1[["tau"]] - LYL$age - LYL$life_exp

    colnames(LYL) <- c("age", lyl_population1[["death_labels"]], "life_exp")

    LYL0 <- list(
      LYL = LYL
    )

    if ((class(lyl_population1) == "lyl")) {
      return(lyl_diff_lyl(lyl_population1, LYL0, decimals, weights, age_specific, age_begin, age_end, parameters))
    } else {
      niter <- lyl_population1[["niter"]]

      LYL0[["LYL_ci"]] <-  LYL0[["LYL"]][rep(1, niter), ]
      LYL0[["LYL_ci"]]$iteration <- rep(1:niter, each = 1)

      return(lyl_diff_lyl_ci(lyl_population1, LYL0, decimals, level, weights, type = lyl_population1[["type"]], age_specific, age_begin, age_end, niter, parameters))
    }

  }

  ####### Age_range
  if ((class(lyl_population1) == "lyl_range") |
      ((class(lyl_population1) == "lyl_ci") & !is.null(lyl_population1[["type"]])  & lyl_population1[["type"]] == "age_range")
  ) {

    age_begin <- lyl_population1[["age_begin"]]
    age_end <- lyl_population1[["age_end"]]
    age_specific <- NA

    # Check that all data for reference population is available
    if (sum(!(age_begin:(lyl_population1[["tau"]] - 1) %in% tmp0$time)) > 0) {
      stop(paste0("Mortality rates or survival for each age from ", age_begin,
                  " until ", (lyl_population1[["tau"]] - 1), " must be supplied for the reference population ('data_ref')."),
           call. = FALSE)
    }

    LYL <- data.frame(cbind(
      age = age_begin:age_end,
      x = NA,
      life_exp = NA
    ))

    for(i in 1:nrow(LYL)) {
      age0 <- age_begin + i - 1
      tmp0 <- tmp0[tmp0$time >= age0, ]
      tmp0$S <- tmp0$S / (tmp0$S[tmp0$time == age0])
      LYL$life_exp[i] <- pracma::trapz(tmp0$time, tmp0$S)
    }

    LYL$x <- lyl_population1[["tau"]] - LYL$age - LYL$life_exp

    colnames(LYL) <- c("age", lyl_population1[["death_labels"]], "life_exp")

    LYL0 <- list(
      LYL = LYL
    )

  if ((class(lyl_population1) == "lyl_range")) {
    return(lyl_diff_lyl(lyl_population1, LYL0, decimals, weights, age_specific, age_begin, age_end, parameters))
  } else {
    niter <- lyl_population1[["niter"]]
    nages <- age_end - age_begin + 1
    LYL0[["LYL_ci"]] <-  LYL0[["LYL"]][rep(1:nrow(LYL0[["LYL"]]), niter), ]
    LYL0[["LYL_ci"]]$iteration <- rep(1:niter, each = nages)

    return(lyl_diff_lyl_ci(lyl_population1, LYL0, decimals, level, weights, type = lyl_population1[["type"]], age_specific, age_begin, age_end, niter, parameters))
  }

  }

}
