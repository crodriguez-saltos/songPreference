# Clear environment----
rm(list= ls())

# Libraries----
library(devtools)
library(songPreference)

# Data----
birDir <- file.path(path.package(package = "songPreference"), "ZF1536")

# Load data----
data <- loadOC(birDir = birDir)
head(data)

# Filter by last time that one song was played back----
datafilt <- filterByLastPb(data = data, filtype = "onesong")
