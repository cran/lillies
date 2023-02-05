#' Life Years Lost at a range of different ages.
#'
#' \code{lyl} estimates remaining life expectancy and Life Years Lost for a given population
#' after a range of specific ages (\code{age_begin} to \code{age_end}) and restrictied to
#' a maximum theoretical age \eqn{\tau}.
#'
#' @export
#'
#' @param data A dataframe, where each raw represents a person. The dataframe will
#' have a time-to-event format with at least two variables: age at end of follow-up (\code{t})
#' and status indicator with death/censoring (\code{status}). Note that this package is not developed
#' to be used with tibbles.
#' @param t0 Age at start of the follow-up time. Default is \code{NULL}, which
#' means all subjects are followed from birth. For delayed entry, \code{t0} indicates
#' beginning of follow-up.
#' @param t Age at the end of the follow-up time (death or censoring).
#' @param status Status indicator, normally 0=alive, 1=dead. Other choices are
#' TRUE/FALSE (TRUE = death) or 1/2 (2=death). For multiple causes of death (competing risks
#' analysis), the status variable will be a factor, whose first level is treated as censoring; or
#' a numeric variable, whose lowest level is treated as censoring. In the latter case,
#' the label for censoring is \code{censoring_label} (\code{"Alive"} by default).
#' @param age_begin Specific starting age at which the Life Years Lost have to be estimated.
#' @param age_end Specific ending age at which the Life Years Lost have to be estimated.
#' @param censoring_label Label for censoring status. If \code{status} is not a factor, \code{"Alive"} by default. If
#' \code{status} is a factor, the first level will be treated as censoring label.
#' @param death_labels Label for event status. For only one cause of death, \code{"Dead"} is the default.
#' For multiple causes, the default are the values given in variable \code{status}.
#' @param tau Remaining life expectancy and Life Years Lost are estimated restrictied to a maximum
#' theoretical age \eqn{\tau} (\eqn{\tau}=100 years by default).
#'
#' @return A list with class \code{"lyl_range"} containing the following components:
#' \itemize{
#'     \item{\code{data}: Data frame with 3 variables and as many observations as the original
#'     data provided to estimate Life Years Lost: \code{t0}, \code{t}, and \code{status}}
#'     \item{\code{LYL}: Data frame with \code{(age_end - age_begin + 1)} observations and at least 3 variables: \code{age} which corresponds
#'     to each specific age from \code{age_begin} to \code{age_end}; \code{life_exp} which is the estimated remaining life expectancy at age specific age
#'     and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
#'     cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
#'     the total overall Life Years Lost}
#'     \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
#'     \item{\code{age_begin}: Specific starting age at which the Life Years Lost have been estimated}
#'     \item{\code{age_end}: Specific ending age at which the Life Years Lost have been estimated}
#'     \item{\code{censoring_label}: Label for censoring status}
#'     \item{\code{death_labels}: Label(s) for death status}
#'     \item{\code{competing_risks}: Logical value (\code{TRUE} = more than one cause of death (competing risks))}
#'     \item{\code{numbers_at_risk}: Data frame with \code{(tau - age_begin + 1)} observations and 2 variables: \code{age} which corresponds
#'     to each specific age from \code{age_begin} to \code{tau}; and \code{number} which is the number of persons at risk of dying at each
#'     specific age}
#'     \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}.}
#' }
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_diff}} to compare average Life Years Lost for two populations.}
#'     \item{\code{\link{lyl_checkplot}} to check whether small numbers could compromise the estimation.}
#'     \item{\code{\link{lyl_ci}} to estimate bootstrapped confidence intervals.}
#'     \item{\code{\link{summary.lyl_range}} to summarize objects obtained with function \code{lyl_range}.}
#'     \item{\code{\link{plot.lyl_range}} to plot objects obtained with function \code{lyl_range}.}
#' }
#'
#' @references \itemize{
#'     \item{Andersen PK. Life years lost among patients with a given disease. \emph{Statistics in Medicine}. 2017;36(22):3573- 3582.}
#'     \item{Andersen PK. Decomposition of number of life years lost according to causes of death. \emph{Statistics in Medicine}. 2013;32(30):5278-5285.}
#'     \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
#'     \emph{PLoS ONE}. 2020;15(3):e0228073.}
#' }
#'
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' \donttest{
#' # Estimate remaining life expectancy and Life Years
#' # Lost after each age from 0 to 94 years and before age 95 years
#' lyl_estimation <- lyl_range(data = simu_data, t = age_death, status = death,
#'                             age_begin = 0, age_end = 94, tau = 95)
#'
#' # Visualize data at each different specific age
#' summary(lyl_estimation)
#' plot(lyl_estimation)
#'
#' # Summarize data over an age distribution
#' summary(lyl_estimation, weights = simu_data$age_disease)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after each age
#' # from 0 to 94 years and before age 95 years
#' lyl_estimation2 <- lyl_range(data = simu_data, t = age_death, status = cause_death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#'
#' # Visualize data at each different specific age
#' summary(lyl_estimation2)
#' plot(lyl_estimation2)
#'
#' # Summarize data over an age distribution
#' summary(lyl_estimation2, weights = simu_data$age_disease)
#' }

lyl_range <- function(data,  t0 = NULL, t, status, age_begin, age_end, censoring_label = "Alive",
                      death_labels = "Dead", tau = 100)  {

  parameters <- as.list(match.call.defaults()[-1])

  message("1. Preparing and checking data...")

  if (missing(age_begin) | missing(age_end)) {
    stop("Parameters 'age_begin' and 'age_end' must be supplied.",
         call. = FALSE)
  }

  if (age_begin > age_end) {
    stop("Age 'age_begin' must be equal or smaller than 'age_end'.",
         call. = FALSE)
  }

  if ((age_begin < 0) | (tau < 0)) {
    stop("Ages 'age_begin', 'age_end', and 'tau' must be equal or larger than 0.",
         call. = FALSE)
  }
  if (age_end >= tau) {
    stop("Age 'tau' is the theoretical maximum age. Ages 'age_begin' and 'age_end' must be smaller than 'tau'.",
         call. = FALSE)
  }

  check <- lyl_check_errors(data, t0, t, status, censoring_label, death_labels, parameters)

  tmp <- check$tmp
  values <- check$values
  competing_risks <- check$competing_risks
  censoring_value <- check$censoring_value
  death_labels <- check$death_labels
  censoring_label <- check$censoring_label

  iterations <- age_end - age_begin + 1
  pb <- progress::progress_bar$new(total = iterations + 1, clear = F, show_after = 0, width = floor(0.4 * getOption("width")))

  message(paste0("3. Estimate LYL for ages ", age_begin, "-", age_end, " years and numbers at risk for ages ", age_begin, "-", tau, " years..."))
  pb$tick(0)

  # We keep numbers at risk
  numbers_at_risk <- data.frame(cbind(age = NA, number = NA))

  for (i in 1:iterations) {
    pb$tick()
    age_specific <- age_begin + i - 1

    # Prepare poopulation at risk at that age
    pop <- tmp[tmp$t1 > age_specific, ]
    pop$age_begin <- pmax(pop$t0, age_specific)

    # Keep numbers at risk
    numbers_at_risk[i, "age"] <- age_specific
    pop_risk <- pop[pop$t0 <= age_specific, ]
    numbers_at_risk[i, "number"] <- nrow(pop_risk)

    # Estimation of LYL at specific age
    if (nrow(pop) == 0) {
      LYL_age <- data.frame(age = age_specific)
      for(lab in c(gsub(" ", "", death_labels), "life_exp")) {
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

  # Numbers at risk after age_end
  if (age_end < (tau - 1)) {
    for (i in 1:(tau - age_end - 1)) {
      age_specific <- age_end + i
      numbers_at_risk[iterations + i, "age"] <- age_specific
      pop_risk <- tmp[tmp$t0 <= age_specific & tmp$t1 > age_specific, ]
      numbers_at_risk[iterations + i, "number"] <- nrow(pop_risk)
    }
  }

  pb$tick()

  message("Done!")

  output <- list(
    data = tmp,
    LYL = LYL,
    tau = tau,
    age_begin = age_begin,
    age_end = age_end,
    censoring_label = censoring_label,
    death_labels = death_labels,
    competing_risks = competing_risks,
    numbers_at_risk = numbers_at_risk,
    type = "age_range"
  )

  class(output) <- "lyl_range"

  return(output)

}

