rankhospital <- function(state, outcome, rank = "best"){
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  df[, 11] <- as.numeric(df[, 11])
  df[, 17] <- as.numeric(df[, 17])
  df[, 23] <- as.numeric(df[, 23])
  
  if (is.numeric(rank)) { #rank treatment
    if (rank>length(unique(df$Hospital.Name))) {
      return(NA)
    }
  }
  else{
    if (rank == 'best') {
      rank <- 1 #first position in sorted vector
    }
  }
  
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
      if (rank=='worst') {
        rankval <- tail(sort(cases_by_state[[11]][complete.cases(cases_by_state[[11]])]), 1)
        rank <- length(cases_by_state[[11]][complete.cases(cases_by_state[[11]])])[1]
        
      }
      else{
        rankval <- sort(cases_by_state[[11]][complete.cases(cases_by_state[[11]])])[rank]
      }
      cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <= rankval)
      cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,Hospital.Name)), ]
      hospital <- cases$Hospital.Name[rank]
      return(hospital)
    }
    else if (outcome == 'heart failure') {
      if (rank=='worst') {
        rankval <- tail(sort(unique(cases_by_state[[17]][complete.cases(cases_by_state[[17]])])), 1)
        rank <- length(cases_by_state[[17]][complete.cases(cases_by_state[[17]])])[1]
      }
      else{
        rankval <- sort(cases_by_state[[17]][complete.cases(cases_by_state[[17]])])[rank]
      }
      cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <= rankval)
      cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,Hospital.Name)), ]
      hospital <- cases$Hospital.Name[rank]
      return(hospital)
    }
    else if (outcome == 'pneumonia') {
      if (rank=='worst') {
        rankval <- tail(sort(unique(cases_by_state[[23]][complete.cases(cases_by_state[[23]])])), 1)
        rank <- length(cases_by_state[[23]][complete.cases(cases_by_state[[23]])])[1]
      }
      else{
        rankval <- sort(cases_by_state[[23]][complete.cases(cases_by_state[[23]])])[rank]
      }
      cases <- subset(cases_by_state, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <= rankval)
      cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,Hospital.Name)), ]
      hospital <- cases$Hospital.Name[rank]
      return(hospital)
    }
  }
}
