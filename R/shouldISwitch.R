#' Take the decission  of whether to set-up a contingency reversal based on the
#' history of key presses.
#'
#' @keywords operant conditioning, reversal
#' @param data Dataframe to analyze.
#' @param alpha Alpha level for stablishing a significant preference.
#' @param consecutive_days Number of consecutive days when preference for a song
#'   must be significant.
#' @param ... Arguments passed to filterByLastPb.
#'
#' @details the function extracts the last `consecutive_days` rows of `data` and
#'   for each day runs a permutation test to test whether the number of presses
#'   for one key was significantly larger than for the other. If the number of
#'   presses was significantly higher for one of the keys during the
#'   `consecutive_days`, then a reversal is recommended.
#'
#'   In some reinforcement schedules, reversals are recommended only once. Check
#'   with your reinforcement schedule to do a reversal before accepting the
#'   recommendation given by this function.
#' @export

shouldISwitch <- function(data, alpha= 0.05, consecutive_days = 5,
                          ...){
  # Filter data---
  data <- filterByLastPb(data= data, ...)

  # Extract the last days----
  uniq_dates <- unique(data$dates)
  date_range <- (length(uniq_dates) - consecutive_days + 1):length(uniq_dates)
  recent_dates <- uniq_dates[date_range]
  data_filtered <- data[is.element(data$dates, recent_dates),]

  # Check whether there is preference for one key----
  press_matrix <- getKeyCount(data= data_filtered)
  most_pressed_key <- apply(X = press_matrix, MARGIN = 1, FUN = which.max)

  # Check whether preference for a key has been maintained for all days----
  if (all(most_pressed_key == most_pressed_key[1])){
    pvals <- apply(X = press_matrix, MARGIN = 1, FUN = function(x){
      prop.test(x= x[1], n = sum(x))$p.value
    })

    if(all(pvals <= alpha)){
      switch <- T
    }else{
      switch <- F
    }
  }else{
    switch <- F
  }
  # Print output----
  print(cbind(press_matrix, p= pvals))

  if (switch){
    print(paste("Do reversal. If you have done a reversal in the past, check",
          "whether your reinforcement schedule allows more than one reversal"))
  } else{
    print("Do not switch contingencies.")
  }

  return(switch)
}
