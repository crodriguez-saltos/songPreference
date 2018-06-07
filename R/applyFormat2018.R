#' Apply the 2018 format to database
#'
#' @keywords operant conditioning, SingSparrow, format
#' @param data Dataframe containing SingSparrow data

applyFormat2018 <- function(data){
  colnames(data)[colnames(data) == "soundA"] <- "sound1"
  colnames(data)[colnames(data) == "soundB"] <- "sound2"
  data$Time <- as.POSIXct(data$Time)
  data$dates <- as.Date(data$Time)
  return(data)
}
