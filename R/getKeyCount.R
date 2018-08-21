#' Get daily count of presses for each key.
#'
#' @keywords preference,operant conditioning, songbirds, SingSparrow
#' @param data Data frame containing output data from SingSparrow for a single
#'   bird
#' @param excludeRev Logical. Exlude data from some days after a contingency
#'   reversal.
#' @param nexlude If excludeRev is TRUE, the number of days after a contingency
#'   reversal to exclude.
#' @details All the measures based on presses for a key associated with playback
#'   of a given song.
#' @export

getKeyCount <- function(data){
  press_matrix <- plyr::daply(.data = data, .variables = "dates",
                             .fun = function(x){
                               c(summary(x$Key))
                             })
  return(press_matrix)
}
