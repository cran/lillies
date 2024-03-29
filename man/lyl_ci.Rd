% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lyl_ci.R
\name{lyl_ci}
\alias{lyl_ci}
\title{Confidence intervals for Life Years Lost.}
\usage{
lyl_ci(lyl_estimation, niter = 1000)
}
\arguments{
\item{lyl_estimation}{An object of class \code{lyl} or \code{lyl_range}.}

\item{niter}{Number of iterations for the bootstrap (default is 1,000).}
}
\value{
A list with class \code{"lyl_ci"} containing the following components:
\itemize{
    \item{\code{LYL}: Data frame with one observation per age and at least 3 variables: \code{age}; \code{life_exp}
    which is the estimated remaining life expectancy at age \code{age_specific} years
    and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
    cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
    the total overall Life Years Lost}
    \item{\code{LYL_ci}: Data frame with one observation per age-iteration and at least 4 variables: \code{age}; \code{iteration}, which
    correspond to each specific iteration; \code{life_exp} which is the estimated remaining life expectancy at age \code{age_specific} years
    and before age \code{tau} years; and one variable corresponding to the estimated Life Years Lost for each specific
    cause of death. If only one cause of death is considered (no competing risks), this variable is \code{Dead} and includes
    the total overall Life Years Lost}
    \item{\code{tau}: Maximum theoretical age \eqn{\tau}}
    \item{\code{age_specific}: Specific age at which the Life Years Lost have been estimated}
    \item{\code{age_begin}: Specific starting age at which the Life Years Lost have been estimated}
    \item{\code{age_end}: Specific ending age at which the Life Years Lost have been estimated}
    \item{\code{death_labels}: Label(s) for death status}
    \item{\code{competing_risks}: Logical value (\code{TRUE} = more than one cause of death (competing risks))}
    \item{\code{type}: Whether the estimation is at \code{"age_specific"} or \code{"age_range"}.}
    \item{\code{niter}: Number of iterations used to estimate the confidence intervals}
}
}
\description{
\code{lyl_ci} estimates confidence intervals for Life Years Lost using non-parametric
bootstrap. The confidence level can be specified when summarizing the results with
the function \code{\link{summary.lyl_ci}}.
}
\examples{
# Load simulated data as example
data(simu_data)

# Estimate remaining life expectancy and Life Years
# Lost after age 45 years and before age 95 years
lyl_estimation <- lyl(data = simu_data, t = age_death, status = death,
                      age_specific = 45, tau = 95)

\donttest{
# Calculate bootstrapped confidence interval (3 iterations to test; more are necessary)
lyl_estimation_ci <- lyl_ci(lyl_estimation, niter = 3)
summary(lyl_estimation_ci)
plot(lyl_estimation_ci)

# Estimate remaining life expectancy and Life Years
# Lost after each age from 0 to 94 years and before age 95 years
lyl_estimation2 <- lyl_range(data = simu_data, t = age_death, status = death,
                             age_begin = 0, age_end = 94, tau = 95)

# Calculate bootstrapped confidence interval (3 iterations to test; more are necessary)
lyl_estimation_ci2 <- lyl_ci(lyl_estimation2, niter = 3)
summary(lyl_estimation_ci2, weights = simu_data$age_disease)
plot(lyl_estimation_ci2, weights = simu_data$age_disease)
}
}
\references{
\itemize{
    \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition.
    \emph{PLoS ONE}. 2020;15(3):e0228073.}
}
}
\seealso{
\itemize{
    \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
    \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
    \item{\code{\link{lyl_diff}} to compare Life Years Lost for two populations.}
    \item{\code{\link{summary.lyl_ci}} to summarize objects obtained with function \code{lyl_ci}.}
    \item{\code{\link{plot.lyl_ci}} to plot objects obtained with function \code{lyl_ci}.}
}

\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.
}
