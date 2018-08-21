#' Filter SingSparrow OC according to last playbacks
#'
#' @keywords operant conditioning, SingSparrow
#' @param data Dataframe containing SingSparrow data.
#' @param filtype Type of filter. Can the take the values 'onesong',
#'   'bothsongs', or 'bothsongs_plus100'.
#' @param ... Other arguments passed to getLastPb().
#' @details This function filters SingSparrow data according to the last time a
#'   song was played back. The type of filter is specified via the argument
#'   filtype. A value of 'onesong' means that the dataframe will be filtered to
#'   contain all records per day until playback of one of the songs was
#'   exhausted. A value of 'bothsongs', until playback of both songs were
#'   exahusted. A value of 'bothsongs_plus100', 100 presses after playback of
#'   both songs were exhausted.
#' @export

filterByLastPb <- function(data, filtype= NA, ...){
  if (is.na(filtype)){
    return(data)
  }

  # Find last playbacks of song----
  lastSongs <- getLastPb(data= data, ...)

  # Filters based on lastSongs----
  onesong <- apply(X = lastSongs[,-1], MARGIN = 1, FUN = min)
  bothsongs <- apply(X = lastSongs[,-1], MARGIN = 1, FUN = max)
  bothsongs_plus100 <- apply(X = lastSongs[,-1], MARGIN = 1, FUN = max) + 100


  # Filter according to last playbacks-----
  data.split <- split(x = data, f = data$dates)

  data.filtered <- list(1:length(data.split))
  y <- as.integer(eval(parse(text = filtype)))
  for (i in 1:length(data.split)){
    rowind <- ifelse(is.finite(y[i]), 1, 0):ifelse(is.finite(y[i]), y[i], 0)
    data.filtered[[i]] <- data.split[[i]][rowind,]

    if (any(is.na(data.filtered[[i]]$Event))){
      data.filtered[[i]] <- data.filtered[[i]][0:0,]
    }
  }
  datao <- do.call("rbind", data.filtered)
  return(datao)
}
