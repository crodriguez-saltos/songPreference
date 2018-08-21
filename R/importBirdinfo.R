#' Import information on birds
#'
#' @keywords database, birds
#' @param conn Connection from which to import the data.
#' @export

importBirdinfo <- function(conn){
  birdinfo <- read.csv(conn)
  birdinfo$Hatching.day <- as.Date(x= birdinfo$Hatching.day, format= "%m/%d/%Y")
  return(birdinfo)
}
