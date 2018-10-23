rankhospital <- function(state, outcome, num = "best") {

  ## Read outcome data
  
  data_outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  states <- data_outcome[, 7]
  outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if ((state %in% states) == FALSE) {
    stop(print("Invalid State"))
  }
  else if ((outcome %in% outcomes) == FALSE) {
    stop(print("Invalid Outcome"))
  }
  
  ## Return hospital name in that state with the given rank 30-day death rate
  
  state_outcome <- subset(data_outcome, State == state)
  
  if (outcome == "heart attack") {
    outcome_col <- 11
  }
  else if (outcome == "heart failure") {
    outcome_col <- 17
  }
  else if (outcome == "pneumonia") {
    outcome_col <- 23
  }
  
  if (is.numeric(num) == TRUE) {
    if (length(data_outcome[,2]) < num) {
      return(NA)
    }
  }
  
  outcome_columns <- as.numeric(state_outcome[, outcome_col])
  na_hosp <- is.na(outcome_columns)
  outcome_data <- state_outcome[!na_hosp, ]
  
  outcome_column_name <- names(outcome_data)[outcome_col]
  hospital_column_name <- names(outcome_data)[2]
  index <- with(outcome_data, order(outcome_data[outcome_column_name], outcome_data[hospital_column_name]))
  ordered_required_hosp <- outcome_data[index, ]
  
  if (is.character(num) == TRUE) {
    if (num == "best") {
      num = 1
    }
    else if (num == "worst") {
      num = length(ordered_required_hosp[, outcome_col])
    }
  }
  
  ordered_required_hosp[num, 2]
  
}