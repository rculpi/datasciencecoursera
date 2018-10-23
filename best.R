best <- function(state, outcome) {
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
  
  ## Return hospital name in that state with lowest 30-day death rate
  
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
  
  outcome_columns <- as.numeric(state_outcome[, outcome_col])
  na_hosp <- is.na(outcome_columns)
  outcome_data <- state_outcome[!na_hosp, ]
  
  hosp_considered <- as.numeric(outcome_data[, outcome_col])
  desired_hosp <- which(hosp_considered == min(hosp_considered))
  best_hosp <- outcome_data[desired_hosp, 2]
  
  if (length(best_hosp) > 1) {
    sorted_hosp <- sort(best_hosp)
    sorted_hosp
  }
  else {
    best_hosp
  }
}
