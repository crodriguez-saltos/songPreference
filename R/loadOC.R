#' Load output files from SingSparrow

#' @keywords operant conditioning,singsparrow
#' @param bird Id of the bird.
#' @param birDir Folder containing output files.
#' @param datalim Number of records from each day to be kept
#' @param exlude Indexes of records to exclude.
#' @param col_names Names of the columns in SingSparrow output.
#' @param fmt Format of keypress data file.
#' @param zerokeyrm Logical. Should rows with values of zero for key be removed?

#' @details If bird is not provided, then it is assumed to be the basename of
#'   birDir.
#'
#'   The exclude indexes apply to the dataframe output by the plotting function.
#'   The indexes should be retrieved after preliminary exploration of the
#'   database.
#'
#'   Possible formats of the keypress data file are "2015" or "2019".
#'
#' @return A database containing values from SingSparrow! output in
#'   human-readable format.
#' @export

loadOC <- function(birDir, bird= NULL, datalim= NA, exclude= NA,
                   col_names = c('Event',
                                 'Key',
                                 'Sound',
                                 'File',
                                 'Year', 'Month', 'Day','Hour', 'Min', 'Sec'
                   ), fmt= "2015", zerokeyrm= F){

  # Import files----
  if (is.na(any(exclude))){
    fileNames <- dir(birDir)
  }else{
    fileNames <- dir(birDir)[-exclude]
  }

  # Exclude mockStr files----
  mockstr <- grep(pattern = "mockStr.txt$", x = fileNames)
  if (length(mockstr)){
    fileNames <- fileNames[-mockstr]
  }

  fileContent <- as.list(fileNames)
  for (i in 1:length(fileNames)){
    if (fmt == "2015"){
      fileContent[[i]] <- read.table(file.path(birDir, fileNames[[i]]),
                                     header = TRUE, sep = ',')
    }else if (fmt == "2019"){
      fileContent[[i]] <- read.table(file.path(birDir, fileNames[[i]]),
                                     header = FALSE, sep = ',')
    }

    fileContent[[i]] <- fileContent[[i]][-1,]  # Eliminates the first row,
    # which contains only zeros.
    if (!is.na(datalim)){
      if (nrow(fileContent[[i]]) >= datalim) {
        fileContent[[i]] <- fileContent[[i]][1:datalim,]
      }
    }
  }

  # Extract information from file names----
  spaces <- lapply(fileNames, function(x) gregexpr('_',x)[[1]])
  Id <- sapply(fileNames, function(x) regexpr('Id-', x))
  File1Ind <- sapply(fileNames, function(x) regexpr('File1-', x)[[1]]) + 6
  File2Ind <- sapply(fileNames, function(x) regexpr('File2-', x)[[1]]) + 6
  File1IndEnd <- mapply(function(x,y) x[which(x > y)[1]], spaces, File1Ind)
  File2IndEnd <- mapply(function(x,y) x[which(x > y)[1]], spaces, File2Ind)

  File1 <- fileNames
  for (i in 1:length(fileNames)){
    File1[i] <- substr(fileNames[i], File1Ind[i],File1IndEnd[i] - 1)
  }

  File2 <- fileNames
  for (i in 1:length(fileNames)){
    File2[i] <- substr(fileNames[i], File2Ind[i],File2IndEnd[i] - 1)
  }

  for (i in 1:length(fileContent)) {
    fileContent[[i]]$soundA <- rep(File1[i], nrow(fileContent[[i]]))
  }
  for (i in 1:length(fileContent)) {
    fileContent[[i]]$soundB <- rep(File2[i], nrow(fileContent[[i]]))
  }
  rm(File1Ind, File1IndEnd, File2Ind, File2IndEnd, File1, File2)

  # Merge all files in one dataframe----
  cntFull <- do.call('rbind', fileContent)
  cntFull <- as.data.frame(cntFull)

  # Format data frame----
  col_names <- c(col_names, 'soundA', 'soundB')
  colnames(cntFull) <- col_names

  if (zerokeyrm){
    cntFull <- cntFull[cntFull$Key != 0,]
  }

  cntFull$Key <- factor(c('A','B')[cntFull$Key], levels= c('A', 'B'))

  dates <- paste(cntFull$Year, cntFull$Month, cntFull$Day, sep = '/')
  times <- paste(cntFull$Hour, cntFull$Min,
                 round(cntFull$Sec, digits = 3), sep= ':')
  z <- paste(dates, times)

  cntFull <- subset(cntFull, select= -c(Year, Month, Day, Hour, Min, Sec))
  cntFull$Time <- strptime(z, '%Y/%m/%d %H:%M:%S')
  cntFull <- cntFull[!is.na(cntFull$Time),]

  rm(dates, times, z)

  cntFull <- cntFull[order(cntFull$Time),]

  # We will add a column that shows the corresponding label to the key that was
  # pressed
  labels <- cntFull[,c("soundA", "soundB")]
  label_index <- sapply(cntFull$Key, function(x) which(c("A", "B") == x))
  label_selected <- rep("", nrow(cntFull))
  for (i in 1:length(label_selected)){
    label_selected[i] <- labels[i, label_index[i]]
  }
  cntFull$Key_label <- as.factor(label_selected)
  rm(labels, label_index)

  # Add name of the bird----
  if (is.null(bird)){
    bird <- basename(birDir)
  }
  cntFull$id <- bird

  # Output----
  rownames(cntFull) <- NULL
  cntFull <- applyFormat2018(data= cntFull)
  return(cntFull)
}
