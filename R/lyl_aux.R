match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))

  for (i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )

  match.call(sys.function(sys.parent()), call)
}


lyl_check_errors <- function(data, t0, t, status, censoring_label, death_labels, parameters) {
  if (missing(data)) {
    stop("Parameter 'data' must be supplied.",
    call. = FALSE)
  }
  if (missing(t)) {
    stop("Parameter 't' must be supplied.",
         call. = FALSE)
    }
  if (missing(status)) {
    stop("Parameter 'status' must be supplied.",
         call. = FALSE)
    }

  tmp <- data

  if (is.null(parameters$t0)) {
    tmp$t0 <- 0
  } else {
    tmp$t0 <- tmp[, as.character(parameters$t0)]
  }
  tmp$t1 <- tmp[, as.character(parameters$t)]
  tmp$status <- tmp[, as.character(parameters$status)]
  tmp <- tmp[, c("t0", "t1", "status")]

  # Check for variables t0 and t1
  if (class(tmp$t0) != "numeric") {
    stop(paste0("Variable '", parameters$t0, "' must be numeric."),
         call. = FALSE)
  }

  if (class(tmp$t1) != "numeric") {
    stop(paste0("Variable '", parameters$t, "' must be numeric."),
         call. = FALSE)
  }

  # Check for variable status
  if (!(class(tmp$status) %in% c("numeric", "factor", "logical"))) {
    stop(paste0("Variable '", parameters$status, "' must be a numeric, logical or factor."),
         call. = FALSE)
  }

  # If it's logical, no competing risks
  if (class(tmp$status) == "logical") {
    message("2. Type of outcome data: 'logical' (TRUE = death / FALSE = alive) [no competing risks - one cause of death]")
    competing_risks <- FALSE
    values <- c(FALSE, TRUE)
    censoring_value <- FALSE
    death_labels <- death_labels[1]
  }

  # If it's a factor
  if (class(tmp$status) == "factor") {
    values <- levels(tmp$status)
    message(paste0("2. Type of outcome data: 'factor' (censoring = '", values[1], "' / different causes of death = '", paste(values[-1], collapse="', '"), "') [competing risks]"))
    competing_risks <- TRUE
    censoring_value <- values[1]
    death_labels <- values[-1]
  }

  # If it's numerical, no competing risks if only two values
  if (class(tmp$status) == "numeric") {
    values <- unique(tmp$status)[order(unique(tmp$status))]
    if (length(values) == 1) {
      tmp$status <- TRUE
      message("2. Type of outcome data: 'numeric' with one value (all subjects assumed to have an event) [no competing risks - one cause of death]")
      censoring_value <- FALSE
      competing_risks <- FALSE
      death_labels <- death_labels[1]
    }
    if (length(values) == 2) {
      tmp$status <- (tmp$status == values[2])
      message(paste0("2. Type of outcome data: 'numeric' with two values ('", values[2], "' = death / '", values[1], "' = alive)[no competing risks - one cause of death]"))
      censoring_value <- values[1]
      competing_risks <- FALSE
      death_labels <- death_labels[1]
    }
    if (length(values) > 2) {
      tmp$status <- factor(tmp$status, levels = values, labels = c(censoring_label, paste0("Cause", values[-1])))
      message(paste0("2. Type of outcome data: 'numeric' with 3+ values (alive = '", values[1], "' / different causes of death = '", paste(values[-1], collapse="', '"), "') [competing risks]"))
      competing_risks <- TRUE
      censoring_value <- values[1]
      death_labels <- paste0("Cause", values[-1])
    }
  }
  return(
    list(
      tmp = tmp,
      values = values,
      competing_risks = competing_risks,
      censoring_value = censoring_value,
      death_labels = death_labels
    )
  )
}



lyl_colors <- function(num_deaths) {

  if (num_deaths==1) { colors <- c("maroon") }
  if (num_deaths==2) { colors <- c("#7fc97f", "#beaed4") }
  if (num_deaths==3) { colors <- c("#7fc97f", "#beaed4", "#fdc086") }
  if (num_deaths==4) { colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99") }
  if (num_deaths==5) { colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0") }
  if (num_deaths==6) { colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f") }
  if (num_deaths==7) { colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17") }
  if (num_deaths==8) { colors <- c("#7fc97f", "#beaed4", "#fdc086", "#ffff99", "#386cb0", "#f0027f", "#bf5b17", "#666666") }
  if (num_deaths==9) { colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fbd462", "#b3de69", "#fccde5", "#d9d9d9") }
  if (num_deaths==10) { colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fbd462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd") }
  if (num_deaths==11) { colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fbd462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5") }
  if (num_deaths==12) { colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fbd462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f") }
  if (num_deaths>12) {
    colors <- c("#8dd3c7", "#ffffb3", "#bebada", "#fb8072", "#80b1d3", "#fbd462", "#b3de69", "#fccde5", "#d9d9d9", "#bc80bd", "#ccebc5", "#ffed6f")
    colors <- rep(colors, num_deaths)[1:num_deaths]
  }
  return(colors)
}


#' @importFrom rlang .data

estimate_lyl <- function(pop, age_specific, tau, competing_risks, censoring_label, death_labels) {

  km <- survival::survfit(survival::Surv(age_begin, t1, status, origin = age_specific) ~ 1, data = pop, id = 1:nrow(pop), se.fit = FALSE)

  if (!competing_risks) {
    cr_df <- data.frame(time = km$time, prob = km$surv)
    colnames(cr_df) <- c("time", censoring_label)
    cr_df[, gsub(" ", "", death_labels)] <- 1 - cr_df[, censoring_label]
    cr_df <- cr_df[, c("time", gsub(" ", "", death_labels), censoring_label)]
  }
  if (competing_risks) {
    cr_df <- data.frame(time = km$time, prob = km$pstate)

    #Change for survival 3.0
    if(utils::packageVersion("survival") >= 3) {
      colnames(cr_df) <- c("time", gsub(" ", "", km$states))
      colnames(cr_df)[colnames(cr_df) == "(s0)"] <- censoring_label
    } else {
      states <- c(km$states[-length(km$states)], censoring_label)
      colnames(cr_df) <- c("time", gsub(" ", "", states))
    }

  }

  cr_df <- dplyr::add_row(cr_df, time = 0, .before = 1)
  cr_df[is.na(cr_df[, censoring_label]), censoring_label] <- 1
  cr_df[is.na(cr_df)] <- 0
  cr_df <- unique(cr_df)
  cr_df$time <- cr_df$time + age_specific
  cr_df <- dplyr::filter(cr_df, .data$time <= tau)
  cr_df$time[length(cr_df$time)] <- tau

  LYL <- data.frame(cbind(age = age_specific))
  for (j in 2:ncol(cr_df)) {
    LYL[1, colnames(cr_df)[j]] <- pracma::trapz(cr_df$time, cr_df[, colnames(cr_df)[j]])
  }

  colnames(LYL)[colnames(LYL) == censoring_label] <- "life_exp"

  output <- list(
    LYL = LYL,
    cr_df = cr_df
  )

  return(output)
}
