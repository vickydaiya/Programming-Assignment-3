rankall <- function(outcome, num = "best") {
  
  ## Read outcome data
  csv_data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that state and outcome are valid
  tot_outcomes <- c('heart attack','heart failure','pneumonia')
  if(is.na(match(outcome,tot_outcomes)))
  {
    stop("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  final_dataframe <- data.frame(hospital = character(0), state = character(0))
  tot_states <- unique(csv_data$State)
  
  for(state in tot_states)
  {
    csv_data_for_state <- csv_data[csv_data$State == state, ]
    
    if(outcome == 'heart attack'){
      na_removed <- csv_data_for_state[!is.na(as.numeric(csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), ]
      ordered <- na_removed[order(as.numeric(na_removed$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), na_removed$Hospital.Name), ]
      max_rank <- nrow(ordered)
    }
    else if(outcome == 'heart failure'){
      na_removed <- csv_data_for_state[!is.na(as.numeric(csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), ]
      ordered <- na_removed[order(as.numeric(na_removed$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), na_removed$Hospital.Name), ]
      max_rank <- nrow(ordered)
    }
    else
    {
      na_removed <- csv_data_for_state[!is.na(as.numeric(csv_data_for_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), ]
      ordered <- na_removed[order(as.numeric(na_removed$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), na_removed$Hospital.Name), ]
      max_rank <- nrow(ordered)
    }
    
    if(num == "best")
    {
      rank <- 1
    }
    else if(num == "worst")
    {
      rank <- max_rank  
    }
    else if(num > max_rank)
    {
      hospital_name <- NA
    }
    else
    {
      rank <- num
    }
    hospital_name <- ordered[rank, c('Hospital.Name')]
    final_dataframe <- rbind(final_dataframe, c(hospital_name,state))
  }
  colnames(final_dataframe) <- c('hospital','state')
  rownames(final_dataframe) <- tot_states
  final_dataframe[order(final_dataframe$state), ]
  
  
}