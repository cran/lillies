% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lyl_aggregated.R
\name{lyl_aggregated}
\alias{lyl_aggregated}
\title{Life Years Lost at one specific age using aggregated data.}
\usage{
lyl_aggregated(
  data,
  age,
  rates,
  surv,
  data0,
  age0,
  rates0,
  surv0,
  age_specific,
  censoring_label = "Alive",
  death_labels = "Dead",
  tau = 100
)
}
\arguments{
\item{data}{A dataframe, where each raw represents an age, for the population of
interest. The dataframe will contain information on age-specific mortality rates
or survivial probability (if both parameters are provided, rates will be used).}

\item{age}{Variable in \code{data} containing information on age.}

\item{rates}{Variable in \code{data} containing information on age-specific mortality rates.}

\item{surv}{Variable in \code{data} containing information on age-specific survival probability.}

\item{data0}{A dataframe, where each raw represents an age, for the population of
reference The dataframe will contain information on age-specific mortality rates
or survivial probability (if both parameters are provided, rates will be used).}

\item{age0}{Variable in \code{data0} containing information on age.}

\item{rates0}{Variable in \code{data0} containing information on age-specific mortality rates.}

\item{surv0}{Variable in \code{data0} containing information on age-specific survival probability.}

\item{age_specific}{Specific age at which the Life Years Lost have to be estimated.}

\item{censoring_label}{Label for censoring status (\code{"Alive"} by default).}

\item{death_labels}{Label for event status (\code{"Dead"} by default).}

\item{tau}{Remaining life expectancy and Life Years Lost are estimated restrictied to a maximum
theoretical age \eqn{\tau} (\eqn{\tau}=100 years by default).}
}
\value{
A list with class \code{"lyl_aggregated"} containing the following components:
\itemize{
    \item{\code{data}: Name of the dataset preovided in parameter \code{data}}
    \item{\code{data0}: Name of the dataset preovided in parameter \code{data0}}
    \item{\code{LYL}: Data frame with 1 observation and 3 variables: \code{age} which corresponds
    to \code{age_spefific}; and \code{life_exp} and \code{life_exp0} which are the estimated remaining life expectancies at age \code{age_specific} years
    and before age \code{tau} years for the population provided in \code{data} and \code{data0}, respectively}
    \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
    \item{\code{age_specific}: Specific age at which the Life Years Lost have been estimated}
    \item{\code{data_plot}: A data frame in long format with 3 variables \code{time}, \code{cause}, and \code{cip} used
    to create a Figure of Life Years Lost with function \code{\link{plot}}.}
    \item{\code{censoring_label}: Label for censoring status}
    \item{\code{death_labels}: Label(s) for death status}
    \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}}
}
}
\description{
\code{lyl} estimates differences in remaining life expectancy and Life Years Lost
for two given life tables \code{data} and \code{data0} after a specific age \code{age_speficic}
and restrictied to a maximum theoretical age \eqn{\tau}.
}
\examples{
# Load simulated data as example
data(aggreg_data)
data(pop_ref)

# Estimate remaining life expectancy and Life Years
# Lost after age 70 years and before age 90 years
lyl_summary_data70 <- lyl_aggregated(data = aggreg_data, age = age, rates = rate,
                                     data0 = pop_ref, age0 = age, surv0 = survival,
                                     age_specific = 70, tau = 90)

# Summarize and plot the data
summary(lyl_summary_data70)
plot(lyl_summary_data70)

}
\references{
\itemize{
    \item{Andersen PK. Life years lost among patients with a given disease. \emph{Statistics in Medicine}. 2017;36(22):3573- 3582.}
    \item{Andersen PK. Decomposition of number of life years lost according to causes of death. \emph{Statistics in Medicine}. 2013;32(30):5278-5285.}
    \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
    \emph{PLoS ONE}. 2020;15(3):e0228073.}#' }
}
\seealso{
\itemize{
    \item{\code{\link{lyl_aggregated_range}} for estimation of Life Years Lost for a range of different ages.}
    \item{\code{\link{summary.lyl_aggregated}} to summarize objects obtained with function \code{lyl_aggregated}.}
    \item{\code{\link{plot.lyl_aggregated}} to plot objects obtained with function \code{lyl_aggregated}.}
}
}
