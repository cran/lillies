#' Confidence intervals for Life Years Lost.
#'
#' \code{lyl_ci} estimates confidence intervals for Life Years Lost using non-parametric
#' bootstrap. The confidence level can be specified when summarizing the results with
#' the function \code{\link{summary.lyl_ci}}.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param lyl_estimation An object of class \code{lyl} or \code{lyl_range}.
#'
#' @param niter Number of iterations for the bootstrap (default is 1,000).
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#'     \item{\code{\link{lyl_diff}} to compare Life Years Lost for two populations.}
#'     \item{\code{\link{summary.lyl_ci}} to summarize objects obtained with function \code{lyl_ci}.}
#'     \item{\code{\link{plot.lyl_ci}} to plot objects obtained with function \code{lyl_ci}.}
#' }
#'
#' @return A list with class \code{"lyl_ci"} containing the following components:
#' \itemize{
#'     \item{\code{LYL}: Data frame with one observation per age and at least 3 variables: \code{age}; \code{life_exp}
#'     which is the estimated remaining life expectancy at age \code{age_specific} years
#'     and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
#'     cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
#'     the total overall Life Years Lost}
#'     \item{\code{LYL_ci}: Data frame with one observation per age-iteration and at least 4 variables: \code{age}; \code{iteration}, which
#'     correspond to each specific iteration; \code{life_exp} which is the estimated remaining life expectancy at age \code{age_specific} years
#'     and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
#'     cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
#'     the total overall Life Years Lost}
#'     \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
#'     \item{\code{age_specific}: Specific age at which the Life Years Lost have been estimated}
#'     \item{\code{age_begin}: Specific starting age at which the Life Years Lost have been estimated}
#'     \item{\code{age_end}: Specific ending age at which the Life Years Lost have been estimated}
#'     \item{\code{death_labels}: Label(s) for death status}
#'     \item{\code{competing_risks}: Logical value (\code{TRUE} = more than one cause of death (competing risks))}
#'     \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}.}
#'     \item{\code{niter}: Number of iterations used to estimate the confidence intervals}
#' }
#'
#' @seealso \code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.
#'
#' @references \itemize{
#'     \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
#'     \emph{PLoS ONE}. 2020;15(3):e0228073.}
#' }
#'
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 45 years and before age 95 years
#' lyl_estimation <- lyl(data = simu_data, t = age_death, status = death,
#'                       age_specific = 45, tau = 95)
#'
#' \donttest{
#' # Calculate bootstrapped confidence interval (3 iterations to test; more are necessary)
#' lyl_estimation_ci <- lyl_ci(lyl_estimation, niter = 3)
#' summary(lyl_estimation_ci)
#' plot(lyl_estimation_ci)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after each age from 0 to 94 years and before age 95 years
#' lyl_estimation2 <- lyl_range(data = simu_data, t = age_death, status = death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#'
#' # Calculate bootstrapped confidence interval (3 iterations to test; more are necessary)
#' lyl_estimation_ci2 <- lyl_ci(lyl_estimation2, niter = 3)
#' summary(lyl_estimation_ci2, weights = simu_data$age_disease)
#' plot(lyl_estimation_ci2, weights = simu_data$age_disease)
#' }

lyl_ci <- function(lyl_estimation, niter = 1000)  {

  if (missing(lyl_estimation)) {
    stop("An object of class 'lyl' or 'lyl_range' must be supplied.",
         call. = FALSE)
  }

  if (!methods::is(lyl_estimation, "lyl") & !methods::is(lyl_estimation, "lyl_range")) {
  #if (!(class(lyl_estimation) %in% c("lyl", "lyl_range"))) {
    stop("'lyl_ci' works only with objects obtained with functions 'lyl' or 'lyl_range'.",
         call. = FALSE)
  }

  message(paste0("Bootstrap estimation of confidence intervals [", niter, " iterations]"))

  if (methods::is(lyl_estimation, "lyl")) {
  #if (class(lyl_estimation) == "lyl") {
    age_begin <- age_end <- lyl_estimation[["age_specific"]]
  }

  if (methods::is(lyl_estimation, "lyl_range")) {
  #if (class(lyl_estimation) == "lyl_range") {
    age_begin <- lyl_estimation[["age_begin"]]
    age_end <- lyl_estimation[["age_end"]]
  }

  size_population <- nrow(lyl_estimation[["data"]])
  tau <- lyl_estimation[["tau"]]
  competing_risks <- lyl_estimation[["competing_risks"]]
  censoring_label <- lyl_estimation[["censoring_label"]]
  death_labels <- lyl_estimation[["death_labels"]]

  pb <- progress::progress_bar$new(total = niter + 1, clear = F, show_after = 0, width = floor(0.4 * getOption("width")))
  pb$tick(0)

  for(r in 1:niter) {
    pb$tick()

    # Sample bootstrap population
    tmp <- dplyr::sample_n(lyl_estimation[["data"]], size = size_population, replace = TRUE)

    # We loop over range of ages
    for (i in 1:(age_end - age_begin + 1)) {
      age_specific <- age_begin + i - 1

      # Prepare poopulation at risk at that age
      pop <- tmp[tmp$t1 > age_specific, ]
      pop$age_begin <- pmax(pop$t0, age_specific)

      # Estimation of LYL at specific age
      if (nrow(pop) == 0) {
        LYL_age <- data.frame(age = age_specific)
        for(lab in c(death_labels, "life_exp")) {
          LYL_age[1, lab] <- NA
        }
      } else {
        LYL_age <- estimate_lyl(pop, age_specific, tau, competing_risks, censoring_label, death_labels)$LYL
      }

      if (i == 1) {
        LYL <- LYL_age
      } else {
        LYL <- rbind(LYL, LYL_age)
      }
    }

    LYL$iteration <- r
    if (r == 1) {
      LYL_ci <- LYL
    } else {
      LYL_ci <- rbind(LYL_ci, LYL)
    }
  }
  pb$tick()

  message("Done!")

  if (methods::is(lyl_estimation, "lyl")) {
  #if (class(lyl_estimation) == "lyl") {
    output <- list(
      LYL = lyl_estimation[["LYL"]],
      LYL_ci = LYL_ci,
      tau = tau,
      age_specific = age_specific,
      death_labels = death_labels,
      competing_risks = competing_risks,
      type = "age_specific",
      niter = niter
    )

  }

  if (methods::is(lyl_estimation, "lyl_range")) {
  #if (class(lyl_estimation) == "lyl_range") {
    output <- list(
      LYL = lyl_estimation[["LYL"]],
      LYL_ci = LYL_ci,
      tau = tau,
      age_begin = age_begin,
      age_end = age_end,
      death_labels = death_labels,
      competing_risks = competing_risks,
      type = "age_range",
      niter = niter
    )

  }

  class(output) <- "lyl_ci"

  return(output)

}
