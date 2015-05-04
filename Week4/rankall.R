rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData<-read.csv("outcome-of-care-measures.csv", colClasses="character", na.strings="Not Available")[, c(2,7,11,17,23)]
  cols<-c(3,4,5)
  outcomeData[,cols]<-apply(outcomeData[,cols], 2, function(x) (as.numeric (x)))
  
  ## Check that state and outcome are valid
  validStates<-unique(outcomeData$State)
  validStates<-validStates[order(validStates)]
  if (!(outcome %in% c("heart attack", "heart failure", "pneumonia"))) {stop("invalid outcome")}
  
  ## For each state, find the hospital of the given rank
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
  
  hospitals<-c()
  states<-c()
  for (state in validStates){
   ranked_hospitals<-c()
   filteredData<-outcomeData[outcomeData$State==state,]
   filteredData<-filteredData[order(filteredData[,outcome_name],filteredData$Hospital.Name, na.last=NA),]
   total_rows<-nrow(filteredData)
   if (num=="best"){
     ranked_hospitals<-filteredData[1,]$Hospital.Name
   }
   else if (num=="worst"){
     ranked_hospitals<-filteredData[total_rows,]$Hospital.Name
   }
   else {
     ranked_hospitals<-as.character(filteredData[num,]$Hospital.Name)
   }
  hospitals<-c(hospitals, ranked_hospitals)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result<-data.frame(hospital=hospitals, state=validStates)
  return(result)
}
