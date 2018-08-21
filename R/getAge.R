#' Get age of a bird
#'
#' Get the age of a bird for any date.
#' @keywords age, bird
#' @param bird The id of the bird.
#' @param dates Dates for which the age of the bird is wished to be known.
#' @param birdinfo Dataframe contaning hatching records of the bird.
#' @export

getAge <- function(bird, dates, birdinfo){
  bird.hatched <- birdinfo$Hatching.day[birdinfo$Id == toupper(bird)]
  age <- dates - bird.hatched
  return(age)
}
