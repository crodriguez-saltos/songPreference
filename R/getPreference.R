#' Estimate preference for a song
#'
#' @keywords preference,operant conditioning, songbirds, SingSparrow
#' @param data Data frame containing output data from SingSparrow for a single
#'   bird
#' @param sound Song for which preference is measured.
#' @param excludeRev Logical. Exlude data from some days after a contingency
#'   reversal.
#' @param nexlude If excludeRev is TRUE, the number of days after a contingency
#'   reversal to exclude.
#' @details All the measures based on presses for a key associated with playback
#'   of a given song.
#' @export

getPreference <- function(data, sound){
  getPref <- function(x, sound){
    length(which(as.numeric(x$Key) == sound)) / nrow(x)
  }

  datapref <- plyr::ddply(.data = data, .variables = plyr::.(dates),
                    .fun = function(x, y= sound) getPref(x, sound = y))
  colnames(datapref)[colnames(datapref) == "V1"] <- "pref"
  datapref$id <- unique(data$id)
  return(datapref)
}
