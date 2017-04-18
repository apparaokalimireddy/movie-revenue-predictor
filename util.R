#
# This file contains utility functions for the project.
#

# Function: checkForHoliday
# Returns TRUE if the Date passed is a defined Holiday. 
# We will use "dates" and "is.holiday" functions from chron package
checkForHoliday <- function(x) {
  #List of holidays, we want to consider for this project. This list can be changed handle other holidays also.
  hlist <- c("USNewYearsDay","USMemorialDay","USIndependenceDay","USLaborDay","USThanksgivingDay","USChristmasDay") # We can list all possible holidays here.
  # We will use chron package's dates function to create a object to handle the dates
  holidays <- dates(sapply(sapply(hlist,holiday,year=as.integer(format(x, "%Y"))),as.Date))
  #is.holiday function from chron package allows us check if the given date x, is in the list of defined holidays
  return(is.holiday(x,holidays))
}

# Function: checkDayBeforeHoliday
# Function to set day before a holiday, set by a previous call to checkForHoliday
# function, to holiday also. We do this to include day before actual holiday also
# in our regression
checkDayBeforeHoliday<-function(df) {
  for (i in 2:nrow(df)) {
    if (df$Holiday[i] == TRUE)
      df$Holiday[i-1] = TRUE
  }
  df
}

# Function: simpleCap
# Capitalizes first letter of a word separated by space
simpleCap <- function(x) {
  x<-tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

# Function: numberMovieId
# Convert the movie name to the-numbers.com movie name id format
numbersMovieId<-function(movie) {
  movie<-simpleCap(movie)
  movieId<-gsub(" ", "-", gsub("[\\(\\)]", "", movie))
  if (startsWith(tolower(movieId), "the-")) {
    movieId<-sub('The-', '', movieId)
    movieId<-paste(movieId, "-The", sep="")
  }
  movieId
}

# Function: movieDisplayText
# Movie display name from the-numbers movieId
movieDisplayText<-function(movieId) {
  if (endsWith(tolower(movieId), "-the")) {
    movieId<-sub('-The', '', movieId)
    movieId<-paste("The-", movieId, sep="")
  }
  movie<-gsub("-", " ", movieId)
  movie
}

