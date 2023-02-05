#' Simulated population for Life Years Lost estimation.
#'
#' A dataset containing age and cause of death, as well as
#' age at disease diagnosis (or start of a condition) for
#' 100,000 simulated persons.
#'
#' @format A data frame with 100000 rows and 6 variables:
#' \describe{
#'   \item{id}{unique identifier of each person}
#'   \item{age_start}{age at start of follow-up (0 for all individuals)}
#'   \item{age_death}{age at end of follow-up (death or censoring)}
#'   \item{death}{logical variable (\code{TRUE} = death / \code{FALSE} = censoring)}
#'   \item{cause_death}{factor variable with 3 levels: \code{"Alive"} (for those
#'   censored) and \code{"Natural"} and \code{"Unnatural"} (for those dying of natural
#'   and unnatural causes of death, respectively)}
#'   \item{age_disease}{age at developing a specific disease or condition for those
#'   32,391 individuals that develop the disease (missing for the remaining 67,609)}
#' }
#' @source Simulated data
"simu_data"

#' Simulated aggregated data for Life Years Lost estimation.
#'
#' A dataset containing age-specific number of new cases, number
#' of deaths and mortality rates for a simulated disease with onset
#' after age 40 years. Data is available for ages 40-90 years.
#'
#' @format A data frame with 50 rows and 4 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{new_cases}{number of new cases diagnosed at that specific age}
#'   \item{deaths}{number of deaths among the diagnosed at that specific age}
#'   \item{rate}{age-specific mortality rates among the diagnosed}
#' }
#' @source Simulated data
"aggreg_data"

#' Aggregated data for Life Years Lost estimation.
#'
#' A dataset containing age-specific survival probability and mortality
#' rates for Danish women in years 2017-2018.
#'
#' @format A data frame with 100 rows and 3 variables:
#' \describe{
#'   \item{age}{age}
#'   \item{survival}{survival probability at that specific age}
#'   \item{mortality_rates}{age-specific mortality rates}
#' }
#' @source Statistics Danmark (https://www.dst.dk/en/Statistik/emner/befolkning-og-valg/)
"pop_ref"
