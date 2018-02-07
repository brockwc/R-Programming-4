## This is a function called best which takes two arguments, a state abbreviation and 
## an outcome, and returns the hospital with the lowest 30-day mortality rate when treating
## the given outcome. 

best <- function(state, outcome) {
    ## Read in data 
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid using entries in the State column and a vector of appropriate outcomes.
    valid_states <- outcome_data$State
    valid_outcomes <- c("pneumonia", "heart attack", "heart failure")
    
    if (!state %in% valid_states) {
        stop("invalid state")
    } else if (!outcome %in% valid_outcomes) {
        stop("invalid outcome")
    }

    ## Create subset of data by state
    state_sub <- subset(outcome_data, outcome_data$State == state)
    
    ## Return hospital name in that state which has the lowest 30-day death rate
    if (outcome == "heart attack") {
        state_sub$Hospital.Name[which.min(state_sub[, 11])]
    } else if (outcome == "heart failure") {
        state_sub$Hospital.Name[which.min(state_sub[, 17])]
    } else if (outcome == "pneumonia") {
        state_sub$Hospital.Name[which.min(state_sub[, 23])]
    }
}
