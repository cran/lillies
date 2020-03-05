#' Life Years Lost at one specific age.
#'
#' \code{lyl} estimates remaining life expectancy and Life Years Lost for a given population
#' after a specific age \code{age_speficic} and restrictied to a maximum theoretical age \eqn{\tau}.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param data A dataframe, where each raw represents a person. The dataframe will
#' have a time-to-event format with at least two variables: age at end of follow-up (\code{t})
#' and status indicator with death/censoring (\code{status}).
#' @param t0 Age at start of the follow-up time. Default is \code{NULL}, which
#' means all subjects are followed from birth. For delayed entry, \code{t0} indicates
#' age at beginning of follow-up.
#' @param t Age at the end of the follow-up time (death or censoring).
#' @param status Status indicator, normally 0=alive, 1=dead. Other choices are
#' TRUE/FALSE (TRUE = death) or 1/2 (2=death). For multiple causes of death (competing risks
#' analysis), the status variable will be a factor, whose first level is treated as censoring; or
#' a numeric variable, whose lowest level is treated as censoring. In the latter case,
#' the label for censoring is \code{censoring_label} (\code{"Alive"} by default).
#' @param age_specific Specific age at which the Life Years Lost have to be estimated.
#' @param censoring_label Label for censoring status (\code{"Alive"} by default).
#' @param death_labels Label for event status. For only one cause of death, \code{"Dead"} is the default.
#' For multiple causes, the default are the values given in variable \code{status}.
#' @param tau Remaining life expectancy and Life Years Lost are estimated restrictied to a maximum
#' theoretical age \eqn{\tau} (\eqn{\tau}=100 years by default).
#'
#' @return A list with class \code{"lyl"} containing the following components:
#' \itemize{
#'     \item{\code{data}: Data frame with 3 variables and as many observations as the original
#'     data provided to estimate Life Years Lost: \code{t0}, \code{t}, and \code{status}}
#'     \item{\code{LYL}: Data frame with 1 observation and at least 3 variables: \code{age} which corresponds
#'     to \code{age_spefific}; \code{life_exp} which is the estimated remaining life expectancy at age \code{age_specific} years
#'     and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
#'     cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
#'     the total overall Life Years Lost}
#'     \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
#'     \item{\code{age_specific}: Specific age at which the Life Years Lost have been estimated}
#'     \item{\code{data_plot}: A data frame in long format with 3 variables \code{time}, \code{cause}, and \code{cip} used
#'     to create a Figure of Life Years Lost with function \code{\link{plot}}.}
#'     \item{\code{censoring_label}: Label for censoring status}
#'     \item{\code{death_labels}: Label(s) for death status}
#'     \item{\code{competing_risks}: Logical value (\code{TRUE} = more than one cause of death (competing risks))}
#'     \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}.}
#' }
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#'     \item{\code{\link{lyl_ci}} to estimate bootstrapped confidence intervals.}
#'     \item{\code{\link{lyl_diff}} to compare Life Years Lost for two populations.}
#'     \item{\code{\link{summary.lyl}} to summarize objects obtained with function \code{lyl}.}
#'     \item{\code{\link{plot.lyl}} to plot objects obtained with function \code{lyl}.}
#' }
#'
#' @references \itemize{
#'     \item{Andersen PK. Life years lost among patients with a given disease. \emph{Statistics in Medicine}. 2017;36(22):3573- 3582.}
#'     \item{Andersen PK. Decomposition of number of life years lost according to causes of death. \emph{Statistics in Medicine}. 2013;32(30):5278-5285.}
#'     \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
#'     \emph{PLoS ONE}. 2020;15(3):e0228073.}
#' }
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 45 years and before age 95 years
#' lyl_estimation <- lyl(data = simu_data, t = age_death, status = death,
#'                       age_specific = 45, tau = 95)
#'
#' # Summarize and plot the data
#' summary(lyl_estimation)
#' plot(lyl_estimation)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after age 45
#' # years and before age 95 years
#' \donttest{
#' lyl_estimation2 <- lyl(data = simu_data, t = age_death, status = cause_death,
#'                        age_specific = 45, tau = 95)
#'
#' # Summarize and plot the data
#' summary(lyl_estimation2)
#' plot(lyl_estimation2)
#' }

lyl <- function(data, t0 = NULL, t, status, age_specific, censoring_label = "Alive",
                death_labels = "Dead", tau = 100)  {

  parameters <- as.list(match.call.defaults()[-1])

  message("1. Preparing and checking data...")

  if (missing(age_specific)) {
    stop("Parameter 'age_specific' must be supplied.",
         call. = FALSE)
  }
  if ((age_specific < 0) | (tau < 0)) {
    stop("Ages 'age_specific' and 'tau' must be equal or larger than 0.",
         call. = FALSE)
  }
  if (age_specific >= tau) {
    stop("Age 'tau' is the theoretical maximum age. Age 'age_specific' must be smaller than 'tau'.",
         call. = FALSE)
  }

  check <- lyl_check_errors(data, t0, t, status, censoring_label, death_labels, parameters)

  tmp <- check$tmp
  values <- check$values
  competing_risks <- check$competing_risks
  censoring_value <- check$censoring_value
  death_labels <- check$death_labels

  # Prepare poopulation at risk at that age
  pop <- tmp[tmp$t1 > age_specific, ]
  if(nrow(pop) == 0) {
    stop(paste0("There are no persons at risk at age ", age_specific, " years."),
         call. = FALSE)
  }
  pop$age_begin <- pmax(pop$t0, age_specific)

  message("3. Estimating...")
  x <- estimate_lyl(pop, age_specific, tau, competing_risks, censoring_label, death_labels)

  message("4. Almost there...")
  # Plot
  cr_df <- x$cr_df
  cr_df$time <- ceiling(cr_df$time * 100) / 100
  cr_df <- dplyr::ungroup(dplyr::slice(dplyr::group_by(cr_df, .data$time), dplyr::n()))

  cr_df2 <- unique(tidyr::gather(cr_df, "cause", "cip", -.data$time))
  cr_df2$cause <- factor(
    cr_df2$cause,
    levels=c(censoring_label, gsub(" ", "", death_labels)),
    labels=c(censoring_label, death_labels)
  )

  message("Done!")

  output <- list(
    data = tmp,
    LYL = x$LYL,
    tau = tau,
    age_specific = age_specific,
    data_plot = cr_df2,
    censoring_label = censoring_label,
    death_labels = death_labels,
    competing_risks = competing_risks,
    type = "age_specific"
  )

  class(output) <- "lyl"

  return(output)

}
