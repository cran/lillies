% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lyl.R
\name{lyl}
\alias{lyl}
\title{Life Years Lost at one specific age.}
\usage{
lyl(
  data,
  t0 = NULL,
  t,
  status,
  age_specific,
  censoring_label = "Alive",
  death_labels = "Dead",
  tau = 100
)
}
\arguments{
\item{data}{A dataframe, where each raw represents a person. The dataframe will
have a time-to-event format with at least two variables: age at end of follow-up (\code{t})
and status indicator with death/censoring (\code{status}). Note that this package is not developed
to be used with tibbles.}

\item{t0}{Age at start of the follow-up time. Default is \code{NULL}, which
means all subjects are followed from birth. For delayed entry, \code{t0} indicates
age at beginning of follow-up.}

\item{t}{Age at the end of the follow-up time (death or censoring).}

\item{status}{Status indicator, normally 0=alive, 1=dead. Other choices are
TRUE/FALSE (TRUE = death) or 1/2 (2=death). For multiple causes of death (competing risks
analysis), the status variable will be a factor, whose first level is treated as censoring; or
a numeric variable, whose lowest level is treated as censoring. In the latter case,
the label for censoring is \code{censoring_label} (\code{"Alive"} by default).}

\item{age_specific}{Specific age at which the Life Years Lost have to be estimated.}

\item{censoring_label}{Label for censoring status. If \code{status} is not a factor, \code{"Alive"} by default. If
\code{status} is a factor, the first level will be treated as censoring label.}

\item{death_labels}{Label for event status. For only one cause of death, \code{"Dead"} is the default.
For multiple causes, the default are the values given in variable \code{status}.}

\item{tau}{Remaining life expectancy and Life Years Lost are estimated restrictied to a maximum
theoretical age \eqn{\tau} (\eqn{\tau}=100 years by default).}
}
\value{
A list with class \code{"lyl"} containing the following components:
\itemize{
    \item{\code{data}: Data frame with 3 variables and as many observations as the original
    data provided to estimate Life Years Lost: \code{t0}, \code{t}, and \code{status}}
    \item{\code{LYL}: Data frame with 1 observation and at least 3 variables: \code{age} which corresponds
    to \code{age_spefific}; \code{life_exp} which is the estimated remaining life expectancy at age \code{age_specific} years
    and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
    cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
    the total overall Life Years Lost}
    \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
    \item{\code{age_specific}: Specific age at which the Life Years Lost have been estimated}
    \item{\code{data_plot}: A data frame in long format with 3 variables \code{time}, \code{cause}, and \code{cip} used
    to create a Figure of Life Years Lost with function \code{\link{plot}}.}
    \item{\code{censoring_label}: Label for censoring status}
    \item{\code{death_labels}: Label(s) for death status}
    \item{\code{competing_risks}: Logical value (\code{TRUE} = more than one cause of death (competing risks))}
    \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}.}
}
}
\description{
\code{lyl} estimates remaining life expectancy and Life Years Lost for a given population
after a specific age \code{age_speficic} and restrictied to a maximum theoretical age \eqn{\tau}.
}
\examples{
# Load simulated data as example
data(simu_data)

# Estimate remaining life expectancy and Life Years
# Lost after age 45 years and before age 95 years
lyl_estimation <- lyl(data = simu_data, t = age_death, status = death,
                      age_specific = 45, tau = 95)

# Summarize and plot the data
summary(lyl_estimation)
plot(lyl_estimation)

# Estimate remaining life expectancy and Life Years
# Lost due to specific causes of death after age 45
# years and before age 95 years
\donttest{
lyl_estimation2 <- lyl(data = simu_data, t = age_death, status = cause_death,
                       age_specific = 45, tau = 95)

# Summarize and plot the data
summary(lyl_estimation2)
plot(lyl_estimation2)
}
}
\references{
\itemize{
    \item{Andersen PK. Life years lost among patients with a given disease. \emph{Statistics in Medicine}. 2017;36(22):3573- 3582.}
    \item{Andersen PK. Decomposition of number of life years lost according to causes of death. \emph{Statistics in Medicine}. 2013;32(30):5278-5285.}
    \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
    \emph{PLoS ONE}. 2020;15(3):e0228073.}
}
}
\seealso{
\itemize{
    \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
    \item{\code{\link{lyl_ci}} to estimate bootstrapped confidence intervals.}
    \item{\code{\link{lyl_diff}} to compare Life Years Lost for two populations.}
    \item{\code{\link{summary.lyl}} to summarize objects obtained with function \code{lyl}.}
    \item{\code{\link{plot.lyl}} to plot objects obtained with function \code{lyl}.}
}
}
