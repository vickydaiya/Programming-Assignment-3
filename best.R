best <- function(state, outcome) {
  
  ## Read outcome data
  csv_data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  tot_states <- unique(csv_data$State)
  tot_outcomes <- c('heart attack','heart failure','pneumonia')
  
  if(is.na(match(state,tot_states)))
  {
    stop("invalid state")
  }
  if(is.na(match(outcome,tot_outcomes)))
  {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  csv_data_for_state <- csv_data[csv_data$State == state, ]
  if(outcome == 'heart attack'){
    needed_data <- as.numeric(csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    needed_data_without_na <- needed_data[!(is.na(needed_data))]
    min_val <- format(round(min(needed_data_without_na),1), nsmall = 1)
    final <- csv_data_for_state[csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min_val, ]
    head(final[order(final$Hospital.Name), c('Hospital.Name')],n=1)
  }
  else if(outcome == 'heart failure'){
    needed_data <- as.numeric(csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    needed_data_without_na <- needed_data[!(is.na(needed_data))]
    min_val <- format(round(min(needed_data_without_na),1), nsmall = 1)
    final <- csv_data_for_state[csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min_val, ]
    head(final[order(final$Hospital.Name), c('Hospital.Name')],n=1)
  }
  else
  {
    needed_data <- as.numeric(csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    needed_data_without_na <- needed_data[!(is.na(needed_data))]
    min_val <- format(round(min(needed_data_without_na),1), nsmall = 1)
    final <- csv_data_for_state[csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min_val, ]
    head(final[order(final$Hospital.Name), c('Hospital.Name')],n=1)
  }

  
}



