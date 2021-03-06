% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lyl_plot.R
\name{lyl_compare_plot}
\alias{lyl_compare_plot}
\title{Plot Life Years Lost at one specific age for two or more different populations}
\usage{
lyl_compare_plot(
  x,
  color_alive = NA,
  colors = NA,
  nrow = NULL,
  ncol = NULL,
  dir = "h",
  reverse_legend = FALSE,
  labels = NA,
  ...
)
}
\arguments{
\item{x}{A list of objects of class \code{lyl} (obtained with function \code{lyl}).}

\item{color_alive}{Color to be used for the censoring category. Default is NA, and
default color is "white".}

\item{colors}{Vector with one color for each cause of death. Default is NA, and
default colors are used.}

\item{nrow}{Number of rows to be passed to \code{facet_wrap}.}

\item{ncol}{Number of columns to be passed to \code{facet_wrap}.}

\item{dir}{Direction to be passed to facet_wrap: either "h" for horizontal, the default, or "v", for vertical.}

\item{reverse_legend}{Reverse the order of elements in the legend. Ddefault is FALSE, indicating that first is the censoring label and then all causes of death.}

\item{labels}{Vector with labels for the two populations (default are "Population of
interest" for \code{x}, and "Reference population" for \code{y})}

\item{...}{Additional arguments affecting the plot produced.}
}
\value{
A plot with survival function and stacked cause-specific cumulative incidences for two
populations side by side.
}
\description{
\code{lyl_compare_plot} creates a figure of Life Years Lost
at one specific age for two or more different populations.
}
\examples{
# Load simulated data as example
data(simu_data)

# Estimate remaining life expectancy and Life Years
# Lost after age 45 years and before age 95 years
lyl_estimation <- lyl(data = simu_data, t = age_death, status = cause_death,
                      age_specific = 45, tau = 95)

# Same estimate for those with a specific disease
diseased <- simu_data[!is.na(simu_data$age_disease), ]

lyl_estimation1 <- lyl(data = diseased, t0 = age_disease,
                       t = age_death, status = cause_death,
                      age_specific = 45, tau = 95)

# Plot the data
lyl_compare_plot(list(lyl_estimation1, lyl_estimation))
lyl_compare_plot(list(lyl_estimation1, lyl_estimation),
          labels = c("Population with a disease", "General population"))

# The plot can be modified with a usual ggplot2 format
lyl_compare_plot(list(lyl_estimation1, lyl_estimation)) +
  ggplot2::xlab("Age [in years]") +
  ggplot2::ggtitle("Differences in Life Years Lost at age 45 years")

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
    \item{\code{\link{lyl_diff}} to compare Life Years Lost for two populations.}
}
}
