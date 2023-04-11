best <- function(state, outcome){
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df[, 11] <- as.numeric(df[, 11])
  df[, 17] <- as.numeric(df[, 17])
  df[, 23] <- as.numeric(df[, 23])
  
  valid_states <- unique(df$State)
  valid_outcome <- c('heart attack','heart failure', 'pneumonia')
  
  if (!is.element(state, valid_states)) { #check for invalid state
    stop("invalid state")
  }
  else if (!is.element(outcome, valid_outcome)) {#check for valid outcome
    stop("invalid outcome")
  }
  else{ #At this point only valid cases are going
    cases_by_state = subset(df, State == state) #Filter df by the specific given state
    if (outcome == 'heart attack') {
      minval <- min(cases_by_state[[11]][complete.cases(cases_by_state[[11]])])
      cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == minval)
      hospital <- cases$Hospital.Name
      hospital <- sort(hospital)
      return(hospital)
    }
    else if (outcome == 'heart failure') {
      minval <- min(cases_by_state[[17]][complete.cases(cases_by_state[[17]])])
      cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == minval)
      hospital <- cases$Hospital.Name
      hospital <- sort(hospital)
      return(hospital)
    }
    else if (outcome == 'pneumonia') {
      minval <- min(cases_by_state[[23]][complete.cases(cases_by_state[[23]])])
      cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == minval)
      hospital <- cases$Hospital.Name
      hospital <- sort(hospital)
      return(hospital)
    }
  }
}