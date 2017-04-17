# Load and Prepare the data.
#
# Load the data from a file
#
loadAndPrepare <- function(file) {
  df = read.csv(file, stringsAsFactors = FALSE)
  df$Revenues <- as.numeric(gsub("\\$|,","",df$Revenues))
  df$Date <- as.Date(df$Date, "%m/%d/%Y")
  df$Weekday <- factor(substr(weekdays(df$Date), 1, 3))
  return(df)
}

#
# Load the data from the-numbers.com
#
loadFromNumbers <- function(movieId="World-is-Not-Enough-The", samples=21) {
  url= paste("http://www.the-numbers.com/movie/", movieId, "#tab=box-office", sep = "")
  pg <- tryCatch(read_html(url), error=function(e){sprintf("Unable to retreive movie data for %s", movieId)}) # Download webpage
  tb <- html_table(html_children(html_nodes(pg, "#box_office_chart")[2])[1], fill=TRUE)
  ddf <- tb[[1]]
  if (nrow(ddf) < samples) {
    stop(sprintf("No of samples is less than %s", samples))
  }
  df <- head(data.frame(Date=as.Date(ddf$Date, "%Y/%m/%d"), Revenues=as.numeric(gsub("\\$|,","",ddf$Gross))), samples)
  df$Day <- as.integer(seq(1, samples, by = 1))
  df$Weekday <- factor(substr(weekdays(df$Date), 1, 3))
  return(df)
}

#
# Search movies on the-numbers.com
#
searchOnNumbers <- function(searchterm) {
  searchterm<-URLencode(searchterm) # Encode the search term to it include in URL
  url<-paste("http://www.the-numbers.com/search?searchterm=", searchterm, "&searchtype=allmatches", sep="")
  pg <- read_html(url) # Download webpage
  tb <- html_table(pg, fill=TRUE)
  result<-tb[[2]]$Movie
  result<-result[!is.na(result)] # Remove NAs
#  ddf<-data.frame(MovieId=gsub(" ", "-", gsub("[\\(\\)]", "", regmatches(result, gregexpr("\\(.*?\\)", result)))))
  y<-strsplit(result, "(", fixed = TRUE)
  ddf <- data.frame(matrix(unlist(y), nrow=length(y), byrow=T),stringsAsFactors=FALSE)
  ddf$X2<-gsub(")", "", ddf$X2, fixed = TRUE)
  ddf$X2<-gsub(" ", "-", ddf$X2, fixed = TRUE)
  ddf
}
#
# Prepare df with Dummy variables for hoildays and weekends
#
setDummyVariables <- function(df) {
  df$Friday=ifelse(weekdays(df$Date)=="Friday",TRUE,FALSE)
  df$Saturday=ifelse(weekdays(df$Date)=="Saturday",TRUE,FALSE)
  df$Sunday=ifelse(weekdays(df$Date)=="Sunday",TRUE,FALSE)
  df$Holiday<-checkForHoliday(df$Date)
  df = checkDayBeforeHoliday(df)
  df
}