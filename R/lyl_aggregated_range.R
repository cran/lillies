#'
#' Life Years Lost at a range of different ages using aggregated data.
#'
#' \code{lyl} estimates differences in remaining life expectancy and Life Years Lost
#' for two given life tables \code{data} and \code{data0} after a range of specific ages
#' (\code{age_begin} to \code{age_end}) and restrictied to a maximum theoretical age \eqn{\tau}.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param data A dataframe, where each raw represents an age, for the population of
#' interest. The dataframe will contain information on age-specific mortality rates
#' or survivial probability (if both parameters are provided, rates will be used).
#' @param age Variable in \code{data} containing information on age.
#' @param rates Variable in \code{data} containing information on age-specific mortality rates.
#' @param surv Variable in \code{data} containing information on age-specific survival probability.
#' @param weights Variable in \code{data} containing information on number of new cases per age. A
#' weighted average is provided using these weights.
#' @param data0 A dataframe, where each raw represents an age, for the population of
#' reference The dataframe will contain information on age-specific mortality rates
#' or survivial probability (if both parameters are provided, rates will be used).
#' @param age0 Variable in \code{data0} containing information on age.
#' @param rates0 Variable in \code{data0} containing information on age-specific mortality rates.
#' @param surv0 Variable in \code{data0} containing information on age-specific survival probability.
#' @param age_begin Specific starting age at which the Life Years Lost have to be estimated.
#' @param age_end Specific ending age at which the Life Years Lost have to be estimated.
#' @param censoring_label Label for censoring status (\code{"Alive"} by default).
#' @param death_labels Label for event status (\code{"Dead"} by default).
#' @param tau Remaining life expectancy and Life Years Lost are estimated restrictied to a maximum
#' theoretical age \eqn{\tau} (\eqn{\tau}=100 years by default).
#'
#' @return A list with class \code{"lyl_aggregated"} containing the following components:
#' \itemize{
#'     \item{\code{data}: Name of the dataset preovided in parameter \code{data}}
#'     \item{\code{data0}: Name of the dataset preovided in parameter \code{data0}}
#'     \item{\code{LYL}: Data frame with 1 observation and 2 variables: \code{life_exp} and \code{life_exp0} which are the
#'     estimated remaining life expectancies averaged over the age range
#'     and before age \code{tau} years for the population provided in \code{data} and \code{data0}, respectively}
#'     \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
#'     \item{\code{age_begin}: Specific starting age at which the Life Years Lost have been estimated}
#'     \item{\code{age_end}: Specific ending age at which the Life Years Lost have been estimated}
#'     \item{\code{data_plot}: A data frame in long format with 3 variables \code{time}, \code{cause}, and \code{cip} used
#'     to create a Figure of Life Years Lost with function \code{\link{plot}}.}
#'     \item{\code{censoring_label}: Label for censoring status}
#'     \item{\code{death_labels}: Label(s) for death status}
#'     \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}.}
#' }
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_aggregated}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{summary.lyl_aggregated}} to summarize objects obtained with function \code{lyl_aggregated_range}.}
#' }
#'
#' @references \itemize{
#'     \item{Andersen PK. Life years lost among patients with a given disease. \emph{Statistics in Medicine}. 2017;36(22):3573- 3582.}
#'     \item{Andersen PK. Decomposition of number of life years lost according to causes of death. \emph{Statistics in Medicine}. 2013;32(30):5278-5285.}
#'     \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
#'     \emph{PLoS ONE}. 2020;15(3):e0228073.}#' }
#'
#'
#' @examples
#' # Load simulated data as example
#' data(aggreg_data)
#' data(pop_ref)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after each age from 40 to 89 years and before age 90 years
#' lyl_summary_data <- lyl_aggregated_range(data = aggreg_data, age = age,
#'                                          rates = rate, weights = new_cases,
#'                                          data0 = pop_ref, age0 = age, surv0 = survival,
#'                                          age_begin = 40, age_end = 89, tau = 90)
#'
#' # Summarize the data
#' summary(lyl_summary_data)
#'

lyl_aggregated_range <- function(data, age, rates, surv, weights, data0, age0, rates0, surv0,
                           age_begin, age_end, censoring_label = "Alive",
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
  if (missing(weights)) {
    stop("Parameter 'weights' must be supplied.",
         call. = FALSE)
  }

  tmp <- aggregate_prepare_data(data, age, surv, rates, tau, parameters, names = c("data", "age", "surv", "rates"))
  if (sum(!(age_begin:(tau-1) %in% tmp$time)) > 0) {
    stop(paste0("Mortality rates or survival for each age from ", age_begin,
                " until ", (tau - 1), " must be supplied for the diseased population ('data')."),
         call. = FALSE)
  }

  tmp0 <- aggregate_prepare_data(data0, age0, surv0, rates0, tau, parameters, names = c("data0", "age0", "surv0", "rates0"))
  if (sum(!(age_begin:(tau-1) %in% tmp0$time)) > 0) {
    stop(paste0("Mortality rates or survival for each age from ", age_begin,
                " until ", (tau - 1), " must be supplied for the reference population ('data0')."),
         call. = FALSE)
  }


  # Estimate LYL
  message(paste0("2. Estimate LYL for ages ", age_begin, "-", age_end, " years..."))

  for (age_specific in age_begin:age_end) {
    LYL_age <- data.frame(age = age_specific, life_exp = NA, life_exp0 = NA)

    tmp_age <- tmp[tmp$time >= age_specific, ]
    tmp_age$S <- tmp_age$S / (tmp_age$S[tmp_age$time == age_specific])

    tmp_age0 <- tmp0[tmp0$time >= age_specific, ]
    tmp_age0$S <- tmp_age0$S / (tmp_age0$S[tmp_age0$time == age_specific])

    LYL_age[1, "life_exp"] <- pracma::trapz(tmp_age$time, tmp_age$S)
    LYL_age[1, "life_exp0"] <- pracma::trapz(tmp_age0$time, tmp_age0$S)

    if (age_specific == age_begin) {
      LYL <- LYL_age
    } else {
      LYL <- rbind(LYL, LYL_age)
    }

  }

  LYL$TotalLYL <- tau - LYL$age - LYL$life_exp
  LYL$TotalLYL0 <- tau - LYL$age - LYL$life_exp0

  message("3. Almost there...")
  ages_onset <- data[, c(as.character(parameters$age), as.character(parameters$weights))]
  colnames(ages_onset) <- c("age", "n")
  LYL_ages <- dplyr::mutate(dplyr::left_join(LYL, ages_onset, by = "age", all.x = T),
                            n=ifelse(is.na(.data$n), 0, .data$n))

  LYL_avg <- dplyr::select(dplyr::summarise_all(LYL_ages, list(~ stats::weighted.mean(., w = .data$n))), -.data$age, -.data$n)

  message("Done!")

  output <- list(
    data = as.character(parameters$data),
    data0 = as.character(parameters$data0),
    LYL = LYL_avg,
    tau = tau,
    age_begin = age_begin,
    age_end = age_end,
    censoring_label = censoring_label,
    death_labels = death_labels,
    type = "age_range"
  )

  class(output) <- "lyl_aggregated"

  return(output)

}










