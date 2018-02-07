rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!state %in% unique(dat[, 7])) {
        stop("invalid state")
    }
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
    outcome_data[, col] = as.numeric(dat[, col])
    
    ## Create subset of data by state using the user input abbreviation
    state_sub <- subset(outcome_data, outcome_data$State == state)
    
    ## Decipher what rank user wants returned (best, worst, or some number)
    hospital_num = nrow(outcome_data)
    if (num == "best") {
        num = 1
    } else if (num == "worst") {
        num = hospital_num
    }
    
    if (num > hospital_num) {
        return(NA)
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
}