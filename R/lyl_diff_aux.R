#' @importFrom rlang .data

lyl_diff_lyl <- function(lyl_population1, lyl_population0, decimals, weights = NA, age_specific, age_begin, age_end, parameters) {

  reference <- lyl_population0[["LYL"]]
  colnames(reference)[colnames(reference) != "age"] <- paste0(colnames(reference)[colnames(reference) != "age"], "_ref")
  LYL_combined <- dplyr::full_join(lyl_population1[["LYL"]], reference, by = "age")

  for (colu in colnames(lyl_population1[["LYL"]])[colnames(lyl_population1[["LYL"]]) != "age"]) {
    LYL_combined[, colu] <- LYL_combined[, colu] - LYL_combined[, paste0(colu, "_ref")]
    LYL_combined <- LYL_combined[, colnames(LYL_combined)[colnames(LYL_combined)!=paste0(colu, "_ref")]]
  }

  output <- list(
    LYL = LYL_combined,
    tau = lyl_population1[["tau"]],
    age_specific = age_specific,
    age_begin = age_begin,
    age_end = age_end,
    death_labels = lyl_population1[["death_labels"]],
    competing_risks = lyl_population1[["competing_risks"]]
  )

  cat(paste0("Differences in estimates comparing '", as.character(parameters$lyl_population1),
             "' with '", as.character(parameters$lyl_population0), "'.\n"))

  if (methods::is(lyl_population1, "lyl")) {
  #if (class(lyl_population1) == "lyl") {
      class(output) <- "lyl"
      return(summary(output, decimals = decimals, difference = TRUE))
  }
  if (methods::is(lyl_population1, "lyl_range")) {
  #if (class(lyl_population1) == "lyl_range") {
    class(output) <- "lyl_range"
    return(summary(output, decimals = decimals, weights = weights, difference = TRUE))
  }
}



#' @importFrom rlang .data

lyl_diff_lyl_ci <- function(lyl_population1, lyl_population0, decimals, level,
                            weights = NA, type, age_specific, age_begin, age_end, niter, parameters) {

  reference <- lyl_population0[["LYL"]]
  colnames(reference)[colnames(reference) != "age"] <- paste0(colnames(reference)[colnames(reference) != "age"], "_ref")
  LYL_combined <- dplyr::full_join(lyl_population1[["LYL"]], reference, by = "age")

  reference_ci <- lyl_population0[["LYL_ci"]]
  colnames(reference_ci)[!(colnames(reference_ci) %in% c("age", "iteration"))] <-
    paste0(colnames(reference_ci)[!(colnames(reference_ci) %in% c("age", "iteration"))], "_ref")
  LYL_combined_ci <- dplyr::full_join(lyl_population1[["LYL_ci"]], reference_ci, by = c("age", "iteration"))

  for (colu in colnames(lyl_population1[["LYL"]])[colnames(lyl_population1[["LYL"]]) != "age"]) {
    LYL_combined[, colu] <- LYL_combined[, colu] - LYL_combined[, paste0(colu, "_ref")]
    LYL_combined <- LYL_combined[, colnames(LYL_combined)[colnames(LYL_combined)!=paste0(colu, "_ref")]]

    LYL_combined_ci[, colu] <- LYL_combined_ci[, colu] - LYL_combined_ci[, paste0(colu, "_ref")]
    LYL_combined_ci <- LYL_combined_ci[, colnames(LYL_combined_ci)[colnames(LYL_combined_ci)!=paste0(colu, "_ref")]]
  }

  cat(paste0("Differences in estimates comparing '", as.character(parameters$lyl_population1),
             "' with '", as.character(parameters$lyl_population0), "'.\n"))

  if (type == "age_specific") {
    output <- list(
      LYL = LYL_combined,
      LYL_ci = LYL_combined_ci,
      tau = lyl_population1[["tau"]],
      age_specific = age_specific,
      death_labels = lyl_population1[["death_labels"]],
      competing_risks = lyl_population1[["competing_risks"]],
      type = "age_specific",
      niter = niter
    )
  } else {
    output <- list(
      LYL = LYL_combined,
      LYL_ci = LYL_combined_ci,
      tau = lyl_population1[["tau"]],
      age_begin = age_begin,
      age_end = age_end,
      death_labels = lyl_population1[["death_labels"]],
      competing_risks = lyl_population1[["competing_risks"]],
      type = "age_range",
      niter = niter
    )
  }

  class(output) <- "lyl_ci"
  return(summary(output, decimals = decimals, level = level, weights = weights, difference = TRUE))

}
