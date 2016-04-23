rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome 
  data <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE)
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data <- subset(data, State == state)
  if (outcome == "heart attack") {
    data <- data[order(as.numeric(data[,11]),data[,2], na.last = NA),]
  } else if (outcome == "heart failure") {
    data <- data[order(as.numeric(data[,17]),data[,2], na.last = NA),]
  } else if (outcome == "pneumonia") {
    data <- data[order(as.numeric(data[,23]),data[,2], na.last = NA),]
  }
  if (num == "best") {
    head(data[,2],1)
  } else if (num == "worst") {
    tail(data[,2],1)
  } else {
    data[num,2] 
  }
}