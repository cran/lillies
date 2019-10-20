#' Summarize Life Years Lost at one specific age
#'
#' \code{summary} for objects of class \code{lyl} summarizes Life Years Lost
#' at one specific age.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param object An object of class \code{lyl} (obtained with function \code{lyl}).
#'
#' @param decimals Number of decimals to be reported (default is 2).
#'
#' @param difference Parameter automatically created by the package.
#'
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return A table with the summary of the results.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
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
#'

summary.lyl <- function(object, decimals = 2, difference = FALSE, ...) {
  cat(paste0("Estimates at age ", object[["age_specific"]], " years [maximum age tau = ", object[["tau"]], " years]"))

  LYL <- object[["LYL"]]

  if (!object[["competing_risks"]]) {
    colnames(LYL)[colnames(LYL) == object[["death_labels"]]] <- "TotalLYL"
  } else {
    if(difference) {
      LYL$TotalLYL <- -LYL$life_exp
    } else {
      LYL$TotalLYL <- object[["tau"]] - LYL$life_exp - LYL$age
    }
  }

  LYL <- dplyr::select(LYL, -.data$age)
  LYL <- dplyr::select(LYL, .data$life_exp, .data$TotalLYL, dplyr::everything())
  LYL_print <- tidyr::gather(LYL, "category", "estimate")
  LYL_print$CI_left <- "-"
  LYL_print$CI_right <- "-"

  LYL_print$category[!(LYL_print$category %in% c("life_exp", "TotalLYL"))] <-
    paste0("- Due to ", LYL_print$category[!(LYL_print$category %in% c("life_exp", "TotalLYL"))])
  LYL_print$category[LYL_print$category == "life_exp"] <- "Remaining life expectancy"
  LYL_print$category[LYL_print$category == "TotalLYL"] <- "Total Life Years Lost"


  print(knitr::kable(LYL_print, digits = decimals, col.names = c("", "estimate", "CI_left", "CI_right"),
                     format = "rst", align = "lrrr"))
  cat("*Confidence intervals can be estimated with function 'lyl_ci'\n")

  return(invisible(LYL[1, ]))
}


#' Summarize Life Years Lost over a range of differents ages
#'
#' \code{summary} for objects of class \code{lyl_range} summarizes Life Years Lost
#' over a range of different ages.
#'
#' @export
#' @importFrom rlang .data
#'
#' @param object An object of class \code{lyl_range} (obtained with function \code{lyl_range}).
#'
#' @param decimals Number of decimals to be reported (default is 2).
#'
#' @param weights Vector with age distribution of disease/condition onset. If weights are not provided (dafault is \code{weights = NA}),
#' then the differences in Life Years Lost at each age are summarized. If weights are provided, then a weighted average is provided.
#'
#' @param difference Parameter automatically created by the package.
#'
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return A table with the summary of the results.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
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
#'
#' # Summarize data over an age distribution
#' summary(lyl_estimation, weights = simu_data$age_disease)
#' }

summary.lyl_range <- function(object, decimals = 2, weights = NA, difference = FALSE, ...) {

  LYL <- object[["LYL"]]

  if (!object[["competing_risks"]]) {
    colnames(LYL)[colnames(LYL) == object[["death_labels"]]] <- "TotalLYL"
  } else {
    if(difference) {
      LYL$TotalLYL <- -LYL$life_exp
    } else {
      LYL$TotalLYL <- object[["tau"]] - LYL$life_exp - LYL$age
    }
  }

  LYL <- dplyr::select(LYL, .data$age, .data$life_exp, .data$TotalLYL, dplyr::everything())

  if (is.na(weights[1])) {
    message("The object summarized contains Life Years Lost for a range of ages.\nFor a weighted average of all these ages, provide the distribution\nof ages in the 'weights' parameter.")
    cat(paste0("Estimates at ages ", object[["age_begin"]], "-", object[["age_end"]], " years [maximum age tau = ", object[["tau"]], " years]"))
    print(knitr::kable(LYL, digits = decimals, format = "rst"))
    cat("*Confidence intervals can be estimated with function 'lyl_ci'\n")
    return(invisible(LYL))
  }

  ages_onset <- dplyr::mutate(data.frame(t0=weights), age = floor(.data$t0))
  ages_onset <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(ages_onset, .data$age), n = dplyr::n()))

  LYL_ages <- dplyr::mutate(dplyr::left_join(LYL, ages_onset, by = "age", all.x = T),
                            n=ifelse(is.na(.data$n), 0, .data$n))

  LYL_avg <- dplyr::select(dplyr::summarise_all(LYL_ages, list(~ stats::weighted.mean(., w = .data$n))), -.data$age, -.data$n)


  cat(paste0("Estimates at ages ", object[["age_begin"]], "-", object[["age_end"]], " years [maximum age tau = ", object[["tau"]], " years]"))
  LYL_print <- tidyr::gather(LYL_avg, "category", "estimate")
  LYL_print$CI_left <- "-"
  LYL_print$CI_right <- "-"

  LYL_print$category[!(LYL_print$category %in% c("life_exp", "TotalLYL"))] <-
    paste0("- Due to ", LYL_print$category[!(LYL_print$category %in% c("life_exp", "TotalLYL"))])
  LYL_print$category[LYL_print$category == "life_exp"] <- "Remaining life expectancy"
  LYL_print$category[LYL_print$category == "TotalLYL"] <- "Total Life Years Lost"


  print(knitr::kable(LYL_print, digits = decimals, col.names = c("", "estimate", "CI_left", "CI_right"),
                     format = "rst", align = "lrrr"))
  cat("*Confidence intervals can be estimated with function 'lyl_ci'\n")

  return(invisible(LYL_avg[1, ]))

}



#' Summarize Life Years Lost with confidence intervals
#'
#' \code{summary} for objects of class \code{lyl_ci} summarizes Life Years Lost
#' at one specific age or over a range of different ages, including bootstrapped
#' confidence intervals
#'
#' @export
#' @importFrom rlang .data
#'
#' @param object An object of class \code{lyl_ci} (obtained with function \code{lyl_ci}).
#'
#' @param decimals Number of decimals to be reported (default is 2).
#'
#' @param level Confidence level (default is 0.95 for 95\% confidence intervals)
#'
#' @param weights Vector with age distribution of disease/condition onset to be used when Life Years Lost are estimated
#' over a range of ages (with \code{lyl_range} function). If weights are not provided (dafault is \code{weights = NA}),
#' then the differences in Life Years Lost at each age is provided. If weights are provided, then a weighted average is provided.
#'
#' @param difference Parameter automatically created by the package.
#'
#' @param ... Additional arguments affecting the summary produced.
#'
#' @return A table with the summary of the results.
#'
#' @seealso \itemize{
#'     \item{\code{\link{lyl}} for estimation of Life Years Lost at one specific age.}
#'     \item{\code{\link{lyl_range}} for estimation of Life Years Lost for a range of different ages.}
#'     \item{\code{\link{lyl_ci}} to estimate bootstrapped confidence intervals.}
#' }
#'
#' % @references \itemize{
#'     % \item{Plana-Ripoll et al. lillies – An R package for the estimation of excess Life Years Lost among patients with a given disease or condition. \emph{In preparation}.}
#' % }
#' @examples
#' # Load simulated data as example
#' data(simu_data)
#'
#' # Estimate remaining life expectancy and Life Years
#' # Lost after age 45 years and before age 95 years
#' lyl_estimation <- lyl(data = simu_data, t = age_death, status = death,
#'                       age_specific = 45, tau = 95)
#'
#' # Calculate bootstrapped confidence interval (3 iterations to test)
#' lyl_estimation_ci <- lyl_ci(lyl_estimation, niter = 3)
#' summary(lyl_estimation_ci)
#'
#' \donttest{
#' # Estimate remaining life expectancy and Life Years
#' # Lost after each age from 0 to 94 years and before age 95 years
#' lyl_estimation2 <- lyl_range(data = simu_data, t = age_death, status = death,
#'                              age_begin = 0, age_end = 94, tau = 95)
#'
#' # Calculate bootstrapped confidence interval
#' lyl_estimation_ci2 <- lyl_ci(lyl_estimation2)
#' summary(lyl_estimation_ci2, weights = simu_data$age_disease)
#' }

summary.lyl_ci <- function(object, decimals = 2, level = 0.95, weights = NA, difference = FALSE, ...) {

  if (level < 0 | level > 1) {
    stop("Confidence level must be between 0 and 1.",
         call. = FALSE)
  }
  low <- function(x) return(stats::quantile(x, p = ((1 - level) / 2)))
  high <- function(x) return(stats::quantile(x, p = (level + (1 - level) / 2)))


  LYL <- object[["LYL"]]
  LYL_ci <- object[["LYL_ci"]]

  if (!object[["competing_risks"]]) {
    colnames(LYL)[colnames(LYL) == object[["death_labels"]]] <- "TotalLYL"
    colnames(LYL_ci)[colnames(LYL_ci) == object[["death_labels"]]] <- "TotalLYL"
  } else {
    if(difference) {
      LYL$TotalLYL <- -LYL$life_exp
      LYL_ci$TotalLYL <- -LYL_ci$life_exp
    } else {
      LYL$TotalLYL <- object[["tau"]] - LYL$life_exp - LYL$age
      LYL_ci$TotalLYL <- object[["tau"]] - LYL_ci$life_exp - LYL_ci$age
    }
  }

  LYL <- dplyr::select(LYL, .data$age, .data$life_exp, .data$TotalLYL, dplyr::everything())
  LYL_ci <- dplyr::select(LYL_ci, .data$age, .data$life_exp, .data$TotalLYL, dplyr::everything())

  if(object[["type"]] == "age_range") {

    if (is.na(weights[1])) {
      message("The object summarized contains Life Years Lost for a range of ages.\nFor a weighted average of all these ages, provide the distribution\nof ages in the 'weights' parameter.")
      cat(paste0("Estimates at ages ", object[["age_begin"]], "-", object[["age_end"]], " years [maximum age tau = ", object[["tau"]], " years]"))

      LYL_left <- data.frame(dplyr::ungroup(dplyr::summarise_all(dplyr::group_by(dplyr::select(LYL_ci, -.data$iteration), .data$age), list(low))))
      LYL_right <- data.frame(dplyr::ungroup(dplyr::summarise_all(dplyr::group_by(dplyr::select(LYL_ci, -.data$iteration), .data$age), list(high))))

      LYL_ci <- data.frame(age = LYL$age)

      for(labs in colnames(LYL)[colnames(LYL) != "age"]) {
        LYL_ci[, labs] <- paste0(
          format(round(LYL[, labs], decimals), nsmall = decimals),
          " [",
          format(round(LYL_left[, labs], decimals), nsmall = decimals),
          " ; ",
          format(round(LYL_right[, labs], decimals), nsmall = decimals),
          "]")
      }

      print(knitr::kable(LYL_ci))

      output <- list(
        lyl_estimate = LYL,
        lyl_ci_left = LYL_left,
        lyl_ci_right = LYL_right
      )

      return(invisible(output))

    } else {

      ages_onset <- dplyr::mutate(data.frame(t0=weights), age = floor(.data$t0))
      ages_onset <- dplyr::ungroup(dplyr::summarise(dplyr::group_by(ages_onset, .data$age), n = dplyr::n()))

      LYL_ages <- dplyr::mutate(dplyr::left_join(LYL, ages_onset, by = "age", all.x = T),
                                n=ifelse(is.na(.data$n), 0, .data$n))
      LYL <- dplyr::select(dplyr::summarise_all(LYL_ages, list(~ stats::weighted.mean(., w = .data$n))), -.data$age, -.data$n)

      LYL_ci_ages <- dplyr::mutate(dplyr::left_join(LYL_ci, ages_onset, by = "age", all.x = T),
                                   n=ifelse(is.na(.data$n), 0, .data$n))
      LYL_ci <- dplyr::ungroup(dplyr::select(dplyr::summarise_all(dplyr::group_by(LYL_ci_ages, .data$iteration), list(~ stats::weighted.mean(., w = .data$n))), -.data$age, -.data$n))
      LYL_left <- data.frame(dplyr::summarise_all(dplyr::select(LYL_ci, -.data$iteration), list(low)))
      LYL_right <- data.frame(dplyr::summarise_all(dplyr::select(LYL_ci, -.data$iteration), list(high)))


      cat(paste0("Estimates at ages ", object[["age_begin"]], "-", object[["age_end"]], " years [maximum age tau = ", object[["tau"]], " years]"))

    }

  }

  if(object[["type"]] == "age_specific") {

    LYL <- dplyr::select(LYL, -.data$age)
    LYL <- dplyr::select(LYL, .data$life_exp, .data$TotalLYL, dplyr::everything())

    LYL_ci <- dplyr::select(LYL_ci, .data$life_exp, .data$TotalLYL, dplyr::everything())
    LYL_left <- data.frame(dplyr::select(dplyr::summarise_all(dplyr::select(LYL_ci, -.data$iteration), list(low)), -.data$age))
    LYL_right <- data.frame(dplyr::select(dplyr::summarise_all(dplyr::select(LYL_ci, -.data$iteration), list(high)),-.data$age ))

    cat(paste0("Estimates at age ", object[["age_specific"]], " years [maximum age tau = ", object[["tau"]], " years]"))

  }

  LYL_print <- tidyr::gather(LYL, "category", "estimate")
  LYL_print_left <- tidyr::gather(LYL_left, "category", "CI_left")
  LYL_print_right <- tidyr::gather(LYL_right, "category", "CI_right")

  LYL_print <- dplyr::left_join(LYL_print, dplyr::left_join(LYL_print_left, LYL_print_right, by = "category"), by = "category")

  LYL_print$category[!(LYL_print$category %in% c("life_exp", "TotalLYL"))] <-
    paste0("- Due to ", LYL_print$category[!(LYL_print$category %in% c("life_exp", "TotalLYL"))])
  LYL_print$category[LYL_print$category == "life_exp"] <- "Remaining life expectancy"
  LYL_print$category[LYL_print$category == "TotalLYL"] <- "Total Life Years Lost"


  print(knitr::kable(LYL_print, digits = decimals, col.names = c("", "estimate", "CI_left", "CI_right"),
                     format = "rst", align = "lrrr"))
  cat(paste0("*", 100*level, "% confidence intervals based on ", object[["niter"]], " bootstrap iterations\n"))

  output <- list(
    lyl_estimate = LYL,
    lyl_ci_left = LYL_left,
    lyl_ci_right = LYL_right
  )

  return(invisible(output))

}
