best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- subset(data, State == state)
  if (outcome == "heart attack") {
  data <- data[order(as.numeric(data[,11]),data[,2], na.last = NA),]
  } else if (outcome == "heart failure") {
  data <- data[order(as.numeric(data[,17]),data[,2], na.last = NA),]
  } else if (outcome == "pneumonia") {
    data <- data[order(as.numeric(data[,23]),data[,2], na.last = NA),]
  }
  data[1,2]
}