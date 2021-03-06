% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lyl_plot.R
\name{plot.lyl}
\alias{plot.lyl}
\title{Plot Life Years Lost at one specific age}
\usage{
\method{plot}{lyl}(x, color_alive = NA, colors = NA, reverse_legend = FALSE, ...)
}
\arguments{
\item{x}{An object of class \code{lyl} (obtained with function \code{lyl}).}

\item{color_alive}{Color to be used for the censoring category. Default is NA, and
default color is "white".}

\item{colors}{Vector with one color for each cause of death. Default is NA, and
default colors are used.}

\item{reverse_legend}{Reverse the order of elements in the legend. Ddefault is FALSE, indicating that first is the censoring label and then all causes of death.}

\item{...}{Additional arguments affecting the plot produced.}
}
\value{
A plot with survival function and stacked cause-specific cumulative incidences.
}
\description{
\code{plot} for objects of class \code{lyl} creates a figure of Life Years Lost
at one specific age.
}
\examples{
# Load simulated data as example
data(simu_data)

# Estimate remaining life expectancy and Life Years
# Lost after age 45 years and before age 95 years
lyl_estimation <- lyl(data = simu_data, t = age_death, status = cause_death,
                      age_specific = 45, tau = 95)

# Summarize and plot the data
plot(lyl_estimation)
plot(lyl_estimation, colors = c("chocolate", "cornflowerblue"))

# The plot can be modified with a usual ggplot2 format
plot(lyl_estimation) +
  ggplot2::xlab("Age [in years]") +
  ggplot2::ggtitle("Life Years Lost at age 45 years")
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
}
}
