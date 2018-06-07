#' Get record indexes when playback of a song or more were exhausted
#'
#' @keywords operant conditioning, quota exhausted.
#' @param data Dataframe containing SingSparrow data.
#' @param quota Quota for daily playback of songs.
#' @export

getLastPb <- function(data, quota = 30){

  # Auxilliary functions----
  lastByDay <- function(x, song){
    played <- cumsum(x$Sound == song)
    ind <- min(which(played == quota))
    return(ind)
  }

  # Get dates----
  data$dates <- as.Date(data$Time)

  # Find when were the two songs exhausted
  lastSong_1 <- plyr::ddply(.data = data, .variables = plyr::.(dates),
                          .fun = function(x) lastByDay(x = x, song = 1))
  colnames(lastSong_1)[colnames(lastSong_1) == "V1"] <- "lastsong1"

  lastSong_2 <- plyr::ddply(.data = data, .variables = plyr::.(dates),
                          .fun = function(x) lastByDay(x = x, song = 2))
  colnames(lastSong_2)[colnames(lastSong_2) == "V1"] <- "lastsong2"

  lastSongs <- merge(lastSong_1, lastSong_2)

  return(lastSongs)
}
