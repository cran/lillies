aggregate_prepare_data <- function(data, age, surv, rates, tau, parameters, names) {
  if (missing(data)) {
    stop(paste0("Dataset '", names[1], "' must be provided."),
         call. = FALSE)
  }

  if (missing(age)) {
    stop(paste0("Parameter '", names[2], "' must be provided."),
         call. = FALSE)
  }

  if (missing(surv) & missing(rates)) {
    stop(paste0("Parameter '", names[3], "' or '", names[4], "' must be provided."),
         call. = FALSE)
  }

  if (!missing(surv)) {
    if (!missing(rates)) {
      message(paste0("Parameter '", names[3], "' ignored because '", names[4], "' also provided."))
    }

    # If survival provided
    tmp0 <- data[, c(as.character(parameters[[names[2]]]), as.character(parameters[[names[3]]]))]
    colnames(tmp0) <- c("time", "S")
    tmp0 <- tmp0[tmp0$time <= tau, ]

  } else {

    # If mortality rates provided: transform to survival
    tmp0 <- data[, c(as.character(parameters[[names[2]]]), as.character(parameters[[names[4]]]))]
    colnames(tmp0) <- c("time", "r")
    tmp0 <- tmp0[tmp0$time <= tau, ]

    tmp0 <- tmp0[order(tmp0$time), ]
    tmp0$to <- c(tmp0$time[-1], tau)

    tmp0$S <- 1
    S <- 1
    for(i in 1:nrow(tmp0)) {
      tmp0$S[i] <- S * (1 - tmp0$r[i] * (tmp0$to[i] - tmp0$time[i]))
      S <- tmp0$S[i]
    }
    tmp0 <- data.frame(time = c(tmp0$time[1], tmp0$to), S = c(1, tmp0$S))
  }

  return(tmp0)

}
