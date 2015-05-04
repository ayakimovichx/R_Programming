rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData<-read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available")[, c(2,7,11,17,23)]
  cols<-c(3,4,5)
  outcomeData[,cols]<-apply(outcomeData[,cols], 2, function(x) (as.numeric (x)))
  
  ## Check that state and outcome are valid
  validStates<-c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
  if (!(state %in% validStates)) {stop("invalid state")}
  validOutcomes<-c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% validOutcomes)) {stop("invalid outcome")}
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  outcomeData<-outcomeData[outcomeData$State==state,]
  outcome_name<-NULL
  if (outcome == "heart attack") {
    outcome_name<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
  }
  else if (outcome == "heart failure") {
    outcome_name<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  }
  else if (outcome == "pneumonia") {
    outcome_name<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  }  
  outcomeData<-outcomeData[order(outcomeData[,outcome_name],outcomeData$Hospital.Name, na.last=NA),]
  total_rows<-nrow(outcomeData)
  if (num=="best"){return (outcomeData[1,]$Hospital.Name)}
  else if (num=="worst"){return (outcomeData[total_rows,]$Hospital.Name)}
  else if (as.numeric(num)>=1 && as.numeric(num)<=total_rows){return (outcomeData[num,]$Hospital.Name)}
  else if (as.numeric(num)>total_rows){return (NA)}
}
