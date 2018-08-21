#' Remove days after a reversal
#'
#' Remove data from a certain number of days after a contingency reversal. The
#' reversal must be recorded on a database containing information on
#' experimental treatment applied to the bird.
#' @keywords reversal, operant conditioning
#' @param keypress Dataframe containing keypress data from SingSparrow
#' @param bird Name of the bird.
#' @param birdinfo Optional. Database on which to check accuracy of reversal
#'   record
#' @param dpostrev Number of days after a reversal that data will be excluded
#' @param verbose Logical. Should verbose mode be activated?
#' @return Dataframe containing info on keypresses, in same format as in
#'   keypress, excluding data some days after a reversal was performed.
#' @export

rmRev <- function(keypress, bird, birdinfo, dpostrev, verbose = F){
  # Test whether a reversal was applied----
  brecord <- birdinfo[birdinfo$Id == bird | birdinfo$Id == toupper(bird),]

  reversed <- as.character(brecord$Reversal)
  reversed <- ifelse(reversed == "Y", T, F)

  # Find reversals in dataframe----
  if (reversed){
    if (verbose) print("Reversals found")
    to <- as.difftime(dpostrev, units= "days")
    revmat <- seekRev(keypress)
    if (verbose) print(head(revmat))
    if (is.null(dim(revmat))){
      if (verbose) print (paste(
        "Warning: There are no reversals in the data;",
        "for caution, check accuracy of bird info database."
      )
      )
    }else{
      revtime <- revmat[2,]$Time
      until <- revtime + to
      keypress <- keypress[!(keypress$Time > revtime & keypress$Time < until),]
      keypress$Time <- as.POSIXct(keypress$Time)
    }
  }
  return(keypress)
}
