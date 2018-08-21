#' Search contingency reversals in dataframe
#'
#' @keywords reversal, operant conditioning
#' @param x Dataframe containing records from SingSparrow.
#' @return A dataframe with the reversal time. This time is the midpoint between
#'   two consecutive events found in x before and after the reversal. In
#'   addition, the dataframe contains the song which became associated withn key
#'   A after the reversal, and that which became associated with key B.
#' @export

seekRev <- function(x){
  vars <- c("Time", "keyA", "keyB")

  revs <-  which(x[1:(nrow(x) - 1),]$sound1 != x[2:nrow(x),]$sound1)
  if (!length(revs)) return(0)
  revs.time <- mean(c(x[revs,]$Time, x[revs + 1,]$Time))
  revs.keyA <- x[revs + 1,]$sound1
  revs.keyB <- x[revs + 1,]$sound2
  revs <- data.frame(revs.time, revs.keyA, revs.keyB)
  colnames(revs) <- vars

  event0 <- x[1,]
  song0 <- data.frame(event0$Time, event0$sound1, event0$sound2)
  colnames(song0) <- vars

  return(rbind(song0, revs))
}
