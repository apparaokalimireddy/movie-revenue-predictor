# Returns TRUE if the Date passed is a defined HOliday. 
# TODO: Need to add more comments
checkForHoliday <- function(x) {
  hlist <- c("USNewYearsDay","USMemorialDay","USIndependenceDay","USLaborDay","USThanksgivingDay","USChristmasDay") # We can list all possible holidays here.
  holidays <- dates(sapply(sapply(hlist,holiday,year=as.integer(format(x, "%Y"))),as.Date))
  return(is.holiday(x,holidays))
}
checkDayBeforeHoliday<-function(df) {
  for (i in 2:nrow(df)) {
    if (df$Holiday[i] == TRUE)
      df$Holiday[i-1] = TRUE
  }
  df
}

#
# Capitalizes first letter of a word separated by space
#
simpleCap <- function(x) {
  x<-tolower(x)
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

#
# Convert the movie name to the-numbers.com movie name id format
#
numbersMovieId<-function(movie) {
  movie<-simpleCap(movie)
  movieId<-gsub(" ", "-", gsub("[\\(\\)]", "", movie))
  if (startsWith(tolower(movieId), "the-")) {
    movieId<-sub('The-', '', movieId)
    movieId<-paste(movieId, "-The", sep="")
  }
  movieId
}

#
# Movie display name from the-numbers movieId
movieDisplayText<-function(movieId) {
  if (endsWith(tolower(movieId), "-the")) {
    movieId<-sub('-The', '', movieId)
    movieId<-paste("The-", movieId, sep="")
  }
  movie<-gsub("-", " ", movieId)
  movie
}

