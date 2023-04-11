
##Read the outcome data into R via the read.csv function and look at the first few rows.
outcome <- read.csv("outcome-of-care-measures.csv", header = TRUE, sep = ",", colClasses = "character")
head(outcome)

##To make a simple histogram of the 30-day death rates from heart attack (column 11 in the outcome dataset)
outcome[, 11] <- as.numeric(outcome[, 11])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, 11])

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

best("SC", "heart attack")


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

rankall <- function(outcome, rank = "best"){
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
  
  valid_states <- sort(unique(df$State))
  valid_outcome <- c('heart attack','heart failure', 'pneumonia')
  
  if (!is.element(outcome, valid_outcome)) {#check for valid outcome
    stop("invalid outcome")
  }
  else{ #At this point only valid cases are going
    for (i in valid_states) { #For each state
      cases_by_state = subset(df, State == i) #Filter df by the specific given state
      
      if (outcome == 'heart attack') {
        #Filter the Only two columns we need
        cases <- cases_by_state[,c("Hospital.Name",
                                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")] 
        cases <- cases[complete.cases(cases),] #Filter Non-NaN rows
        #Sort cases by DeathRate, than by HospitalName
        cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                                         Hospital.Name)), ]
        
        if (rank=='worst') { #take the last element
          hospital <- tail(cases$Hospital.Name,1)
        }
        else{ #take the rank index element
          hospital <- cases$Hospital.Name[rank]
        }
      }
      
      else if (outcome == 'heart failure') {
        #Filter the Only two columns we need
        cases <- cases_by_state[,c("Hospital.Name",
                                   "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")] 
        cases <- cases[complete.cases(cases),] #Filter Non-NaN rows
        #Sort cases by DeathRate, than by HospitalName
        cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                                         Hospital.Name)), ]
        
        if (rank=='worst') { #take the last element
          hospital <- tail(cases$Hospital.Name,1)
        }
        else{ #take the rank index element
          hospital <- cases$Hospital.Name[rank]
        }
      }
      
      else if (outcome == 'pneumonia') {
        #Filter the Only two columns we need
        cases <- cases_by_state[,c("Hospital.Name",
                                   "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")] 
        cases <- cases[complete.cases(cases),] #Filter Non-NaN rows
        #Sort cases by DeathRate, than by HospitalName
        cases <- cases[with(cases, order(Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                                         Hospital.Name)), ]
        
        if (rank=='worst') { #take the last element
          hospital <- tail(cases$Hospital.Name,1)
        }
        else{ #take the rank index element
          hospital <- cases$Hospital.Name[rank]
        }
      }
      
      if (i==valid_states[1]) {
        hospital_stack <- hospital
        states_stack <- i
      }
      else{
        hospital_stack <- c(hospital_stack,hospital)
        states_stack <- c(states_stack,i)
      }
    }#close for
    df_answer <- data.frame("hospital" = hospital_stack, "state" = states_stack)
    return(df_answer)
  }#close outer else
}#close function
