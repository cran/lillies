#' Plot Life Years Lost at one specific age
#'
#' \code{plot} for objects of class \code{lyl} creates a figure of Life Years Lost
#' at one specific age.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param x An object of class \code{lyl} (obtained with function \code{lyl}).
#'
#' @param colors Vector with one color for each cause of death. Default is NA, and
#' default colors are used.
#'
#' @param ... Additional arguments affecting the plot produced.
#'
#' @return A plot with survival function and stacked cause-specific cumulative incidences.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#'
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 45 years and before age 95 years
#' lyl_estimation <- lyl(data = simu_data, t = age_death, status = cause_death,
#'                       age_specific = 45, tau = 95)
#'
#' # Summarize and plot the data
#' plot(lyl_estimation)
#' plot(lyl_estimation, colors = c("chocolate", "cornflowerblue"))
#'
#' # The plot can be modified with a usual ggplot2 format
#' plot(lyl_estimation) +
#'   ggplot2::xlab("Age [in years]") +
#'   ggplot2::ggtitle("Life Years Lost at age 45 years")

plot.lyl <- function(x, colors = NA, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.na(colors[1])) {
    colors <- lyl_colors(length(x[["death_labels"]]))
  }

  g <- ggplot2::ggplot(data=x[["data_plot"]],
                       ggplot2::aes(x = .data$time, y = 100 * .data$cip, group = .data$cause, fill = .data$cause)) +
    ggplot2::geom_area(alpha = 0.6, size = 0.3, color = "black", position = ggplot2::position_stack(rev = T)) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = seq(x[["age_specific"]], x[["tau"]], 5)) +
    ggplot2::xlab("Age in years") +
    ggplot2::ylab(paste0("Percentage of persons alive")) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("white", colors))

  return(g)
}

#' Plot Life Years Lost at a range of different ages
#'
#' \code{plot} for objects of class \code{lyl_range} creates a figure of Life Years Lost
#' at a range of different ages.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param x An object of class \code{lyl_range} (obtained with function \code{lyl_range}).
#'
#' @param colors Vector with one color for each cause of death. Default is NA, and
#' default colors are used.
#'
#' @param ... Additional arguments affecting the plot produced.
#'
#' @return A plot with age-specific life expectancy and life years lost.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#'
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' \donttest{
#' # Estimate remaining life expectancy and Life Years
#' # Lost due to specific causes of death after each age
#' # from 30 to 40 years and before age 95 years
#' lyl_estimation <- lyl_range(data = simu_data, t = age_death, status = cause_death,
#'                              age_begin = 30, age_end = 40, tau = 95)
#'
#' # Summarize and plot the data
#' plot(lyl_estimation)
#' plot(lyl_estimation, colors = c("chocolate", "cornflowerblue"))
#'
#' # The plot can be modified with a usual ggplot2 format
#' plot(lyl_estimation) +
#'   ggplot2::xlab("Age [in years]") +
#'   ggplot2::ggtitle("Life Years Lost at ages 30-40 years")
#'
#' }

plot.lyl_range <- function(x, colors = NA, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  cr_df_long <- unique(tidyr::gather(x[["LYL"]], "cause", "LYL", -.data$age, -.data$life_exp))
  cr_df_long$cause <- factor(
    cr_df_long$cause,
    levels=gsub(" ", "", x[["death_labels"]]),
    labels=x[["death_labels"]]
  )

  if (is.na(colors[1])) {
    colors <- lyl_colors(length(x[["death_labels"]]))
  }

  g <- ggplot2::ggplot(data = cr_df_long, ggplot2::aes(x = .data$age, y = .data$LYL, fill = .data$cause)) +
    ggplot2::geom_col(color = "black") +
    ggplot2::geom_point(ggplot2::aes(y = .data$life_exp), color = "black", show.legend = F) +
    ggplot2::geom_line(ggplot2::aes(y = .data$life_exp), color = "black") +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = seq(0, 200, 5)) +
    ggplot2::scale_y_continuous(breaks = seq(0, x[["tau"]], 5)) +
    ggplot2::xlab("Age in years") +
    ggplot2::ylab(paste0("Remaining life expectancy and Life Years Lost before age ", x[["tau"]], " years")) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c(colors)) +
    ggplot2::guides(color = FALSE)

  return(g)
}


#' Plot number of persons at risk at each specific age.
#'
#' Given a \code{lyl_range}-class object, \code{lyl_checkplot} draws numbers of persons at risk of
#' dying at each specific age from \code{age_begin} until age \eqn{\tau}.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param x An object of class \code{lyl_range} obtained with the \code{lyl_range} function.
#'
#' @return A plot with the number of persons at risk at each specific age.
#'
#' @seealso \code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#'
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after each age from 0 to 94 years and before age 95 years
#' lyl_estimation <- lyl_range(data = simu_data, t = age_death, status = death,
#'                             age_begin = 45, age_end = 50, tau = 95)
#'
#' # Check whether small numbers could compromise the results
#' lyl_checkplot(lyl_estimation)
#'
#' # The plot can be modified with a usual ggplot2 format
#' lyl_checkplot(lyl_estimation) +
#'   ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red") +
#'   ggplot2::xlab("Age [in years]") +
#'   ggplot2::ggtitle("Persons at risk of dying at each age 0-94 years")
#'


lyl_checkplot <- function(x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (class(x) != "lyl_range") {
    stop("'lyl_checkplot' works only with objects obtained with function 'lyl_range'.",
         call. = FALSE)
  }

  plot_data <- x[["numbers_at_risk"]]
  plot_data$number[plot_data$number == 0] <- 0.5

  plot_labels <- function(y) {
    y[y == 0.5] <- 0
    format(y, scientific = FALSE)
  }

  g <- ggplot2::ggplot(data = plot_data, ggplot2::aes(x = .data$age, y = .data$number)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = seq(0, x[["tau"]], 5)) +
    ggplot2:: scale_y_log10(limits = c(0.45, NA),
                            breaks = c(0.5, 1, 2, 3, 5, 10, 20, 50, 100, 500, 1000, 10000, 100000, 1000000, 10000000),
                            labels = function(y) plot_labels(y)) +
    ggplot2::xlab("Age in years") +
    ggplot2::ylab("Number of persons at risk at each age")

  return(g)

}

#' Plot evolution of bootstrapped parameters for Life Years Lost
#'
#' \code{plot} for objects of class \code{lyl_ci} creates a figure of the bootstrapped Life Years Lost
#' to examine if the number of iterations is enough.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param x An object of class \code{lyl_ci} (obtained with function \code{lyl_ci}).
#'
#' @param level Confidence level (default is 0.95 for 95\% confidence intervals)
#'
#' @param weights Vector with age distribution of disease/condition onset to be used when Life Years Lost are estimated
#' over a range of ages (with \code{lyl_range} function).
#'
#' @param ... Additional arguments affecting the plot produced.
#'
#' @return A plot with the evolution of bootstrapped parameters.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_ci}} to estimate bootstrapped confidence intervals.}
#'  }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#'
#' @examples
#'
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 45 years and before age 95 years
#' lyl_estimation <- lyl(data = simu_data, t = age_death, status = death,
#'                       age_specific = 45, tau = 95)
#'
#' \donttest{
#' # Calculate bootstrapped confidence interval (10 iterations to test)
#' lyl_estimation_ci <- lyl_ci(lyl_estimation, niter = 10)
#' plot(lyl_estimation_ci)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after each age from 0 to 94 years and before age 95 years
#' lyl_estimation2 <- lyl_range(data = simu_data, t = age_death, status = death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#'
#' # Calculate bootstrapped confidence interval
#' lyl_estimation_ci2 <- lyl_ci(lyl_estimation2)
#' plot(lyl_estimation_ci2, weights = simu_data$age_disease)
#' }

plot.lyl_ci <- function(x, level = 0.95, weights, ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }

  LYL_ci <- x[["LYL_ci"]]

  if (x[["competing_risks"]]) {
    LYL_ci$TotalLYL <- x[["tau"]] - LYL_ci$life_exp - LYL_ci$age
  }

  LYL_ci <- LYL_ci[, colnames(LYL_ci) != "life_exp"]

  if(x[["type"]] == "age_range") {
    if (missing(weights)) {
      stop("Argument 'weights' must be provided.",
           call. = FALSE)
    }

    ages_onset <- dplyr::mutate(data.frame(t0=weights), age = floor(.data$t0))
    ages_onset <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(ages_onset, .data$age), n = dplyr::n()))

    LYL_ci_ages <- dplyr::mutate(dplyr::left_join(LYL_ci, ages_onset, by = "age", all.x = T),
                                 n=ifelse(is.na(.data$n), 0, .data$n))
    LYL_ci <- dplyr::ungroup(dplyr::select(dplyr::summarise_all(dplyr::group_by(LYL_ci_ages, .data$iteration), list(~ stats::weighted.mean(., w = .data$n))), -.data$age, -.data$n))

  } else {
    LYL_ci <- LYL_ci[, colnames(LYL_ci) != "age"]
    LYL_ci <- dplyr::select(LYL_ci, .data$iteration, dplyr::everything())
  }

  LYL_med <- LYL_left <- LYL_right <- data.frame(iteration = numeric())

  for (i in 1:nrow(LYL_ci)) {
    LYL_med[i, "iteration"] <- LYL_left[i, "iteration"] <- LYL_right[i, "iteration"] <- LYL_ci[i, "iteration"]
    for (j in 2:ncol(LYL_ci)) {
      LYL_med[i, colnames(LYL_ci)[j]] <- stats::quantile(as.data.frame(LYL_ci[(1:i), colnames(LYL_ci)[j]])[, 1], probs = 0.5)
      LYL_left[i, colnames(LYL_ci)[j]] <- stats::quantile(as.data.frame(LYL_ci[(1:i), colnames(LYL_ci)[j]])[, 1], probs = ((1 - level) / 2))
      LYL_right[i, colnames(LYL_ci)[j]] <- stats::quantile(as.data.frame(LYL_ci[(1:i), colnames(LYL_ci)[j]])[, 1], probs = (level + (1 - level) / 2))
    }
  }

  LYL_med$param <- "Median"
  LYL_left$param <- "Low"
  LYL_right$param <- "High"

  LYL_ci_plot <- rbind(LYL_med, rbind(LYL_left, LYL_right))

  LYL_ci_plot <- tidyr::gather(LYL_ci_plot, "cause", "estimate", -.data$iteration, -.data$param)

  if (x[["competing_risks"]]) {
    LYL_ci_plot$cause <- factor(LYL_ci_plot$cause, levels = c("TotalLYL", gsub(" ", "", x[["death_labels"]])),
                                labels = c("Total LYL", x[["death_labels"]]))
  } else {
    LYL_ci_plot$cause <- "LYL"
  }

  g <- ggplot2::ggplot(data = LYL_ci_plot[LYL_ci_plot$param == "Median", ], ggplot2::aes(x = .data$iteration, y = .data$estimate)) +
    ggplot2::geom_line() +
    ggplot2::geom_line(data = LYL_ci_plot[LYL_ci_plot$param == "Low", ], linetype = "dashed") +
    ggplot2::geom_line(data = LYL_ci_plot[LYL_ci_plot$param == "High", ], linetype = "dashed") +
    ggplot2::facet_wrap(~ .data$cause, scales = "free") +
    ggplot2::theme_bw() +
    ggplot2::xlab("Number of bootstrap iterations") +
    ggplot2::ylab(paste0("Median and ", 100 * level, "% bootstraped confidence interval"))

  return(g)
}


#' Plot Life Years Lost at one specific age for two different populations
#'
#' \code{lyl_2plot} creates a figure of Life Years Lost
#' at one specific age for two different populations.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param x An object of class \code{lyl} (obtained with function \code{lyl}).
#'
#' @param y An object of class \code{lyl} (obtained with function \code{lyl}).
#'
#' @param colors Vector with one color for each cause of death. Default is NA, and
#' default colors are used.
#'
#' @param labels Vector with labels for the two populations (default are "Population of
#' interest" for \code{x}, and "Reference population" for \code{y})
#'
#' @param ... Additional arguments affecting the plot produced.
#'
#' @return A plot with survival function and stacked cause-specific cumulative incidences for two
#' populations side by side.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_diff}} to compare Life Years Lost for two populations.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#'
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 45 years and before age 95 years
#' lyl_estimation <- lyl(data = simu_data, t = age_death, status = cause_death,
#'                       age_specific = 45, tau = 95)
#'
#' # Same estimate for those with a specific disease
#' diseased <- simu_data[!is.na(simu_data$age_disease), ]
#'
#' lyl_estimation1 <- lyl(data = diseased, t0 = age_disease,
#'                        t = age_death, status = cause_death,
#'                       age_specific = 45, tau = 95)
#'
#' # Plot the data
#' lyl_2plot(lyl_estimation1, lyl_estimation)
#' lyl_2plot(lyl_estimation1, lyl_estimation,
#'           labels = c("Population with a disease", "General population"))
#'
#' # The plot can be modified with a usual ggplot2 format
#' lyl_2plot(lyl_estimation1, lyl_estimation) +
#'   ggplot2::xlab("Age [in years]") +
#'   ggplot2::ggtitle("Differences in Life Years Lost at age 45 years")

lyl_2plot <- function(x, y, colors = NA, labels = c("Population of interest", "Reference population"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if ((class(x) != "lyl") | (class(y) != "lyl")) {
    stop("'lyl_2plot' works only with objects 'x' and 'y' obtained with function 'lyl'.",
         call. = FALSE)
  }
  if ((x[["age_specific"]] != y[["age_specific"]])) {
    stop("The two objects must have the same 'age_specific' argument'.",
         call. = FALSE)
  }

  if ((x[["tau"]] != y[["tau"]])) {
    stop("The two objects must have the same 'tau'.",
         call. = FALSE)
  }

  if (!identical(x[["death_labels"]], y[["death_labels"]])) {
    stop("The two objects must have the same causes of death.",
         call. = FALSE)
  }

  if (!identical(x[["censoring_label"]], y[["censoring_label"]])) {
    stop("The two objects must have the same censoring label.",
         call. = FALSE)
   }

  if (is.na(colors[1])) {
    colors <- lyl_colors(length(x[["death_labels"]]))
  }

  data_plot1 <- x[["data_plot"]]
  data_plot1$pop <- labels[1]

  data_plot2 <- y[["data_plot"]]
  data_plot2$pop <- labels[2]

  data_plot <- rbind(data_plot1, data_plot2)
  data_plot$pop <- factor(data_plot$pop, levels = labels, labels = labels)

  g <- ggplot2::ggplot(data=data_plot,
                       ggplot2::aes(x = .data$time, y = 100 * .data$cip, group = .data$cause, fill = .data$cause)) +
    ggplot2::geom_area(alpha = 0.6, size = 0.3, color = "black", position = ggplot2::position_stack(rev = T)) +
    ggplot2::facet_wrap( ~ .data$pop) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = seq(x[["age_specific"]], x[["tau"]], 5)) +
    ggplot2::xlab("Age in years") +
    ggplot2::ylab(paste0("Percentage of persons alive")) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("white", colors))

  return(g)
}

#' Plot Life Years Lost at one specific age for two different populations obtained from
#' aggregated data
#'
#' \code{plot} for objects of class \code{lyl_aggregated} creates a figure of Life Years Lost
#' at one specific age for two different populations.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param x An object of class \code{lyl_aggregated} (obtained with function \code{lyl_aggregated}).
#'
#' @param colors Vector with one color for each cause of death. Default is NA, and
#' default colors are used.
#'
#' @param labels Vector with labels for the two populations (default are "Population of
#' interest" for \code{data}, and "Reference population" for \code{data0}; which are provided
#' to function \code{lyl_aggregated}.)
#'
#' @param ... Additional arguments affecting the plot produced.
#'
#' @return A plot with survival function and stacked cause-specific cumulative incidences for two
#' populations side by side.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_aggregated}} for estimation of Life Years Lost at one specific age.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#'
#' @examples
#' # Load simulated data as example
#' data(aggreg_data)
#' data(pop_ref)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 70 years and before age 90 years
#' lyl_summary_data70 <- lyl_aggregated(data = aggreg_data, age = age, rates = rate,
#'                                      data0 = pop_ref, age0 = age, surv0 = survival,
#'                                      age_specific = 70, tau = 90)
#'
#' # Plot the data
#' plot(lyl_summary_data70)
#'
plot.lyl_aggregated <- function(x, colors = NA, labels = c("Population of interest", "Reference population"), ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (is.na(colors[1])) {
    colors <- lyl_colors(1)
  }

  data_plot <- x[["data_plot"]]
  data_plot$pop <- factor(data_plot$pop, levels = c("disease", "reference"), labels = labels)

  g <- ggplot2::ggplot(data = data_plot,
                       ggplot2::aes(x = .data$time, y = 100 * .data$cip, group = .data$cause, fill = .data$cause)) +
    ggplot2::geom_area(alpha = 0.6, size = 0.3, color = "black", position = ggplot2::position_stack(rev = T)) +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap( ~ .data$pop) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_x_continuous(breaks = seq(x[["age_specific"]], x[["tau"]], 5)) +
    ggplot2::xlab("Age in years") +
    ggplot2::ylab(paste0("Percentage of persons alive")) +
    ggplot2::scale_fill_manual(
      name = "",
      values = c("white", colors))

  return(g)
}

