rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  if (outcome == "heart attack") {
    data <- data[order(as.numeric(data[,11]),data[,2], na.last = NA),]
  } else if (outcome == "heart failure") {
    data <- data[order(as.numeric(data[,17]),data[,2], na.last = NA),]
  } else if (outcome == "pneumonia") {
    data <- data[order(as.numeric(data[,23]),data[,2], na.last = NA),]
  }
  data <- split(data, data$State)
  if (num == "best") {
      head(data[,2],1)
  } else if (num == "worst") {
    data <- lapply(data,tail,1) 
  } else {
    data <- lapply(data, "[",num,c(2,7)) 
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  do.call("rbind", data)
}