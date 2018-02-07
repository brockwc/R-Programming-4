rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    if (!state %in% unique(outcome_data[, 7])) {
        stop("invalid state")
    }
    switch(outcome, `heart attack` = {
        col = 11
    }, `heart failure` = {
        col = 17
    }, pneumonia = {
        col = 23
    }, stop("invalid outcome"))
    outcome_data[, col] = as.numeric(outcome_data[, col])
    
    ## Create subset of data by state using the user input abbreviation
    state_sub = outcome_data[outcome_data$State == state, c(2, col)]
    state_sub = na.omit(state_sub)

    ## Decipher what rank user wants returned (best, worst, or some number)
    hospital_num = nrow(state_sub)
    if (num == "best") {
        num = 1
    } else if (num == "worst") {
        num = hospital_num
    }
    
    if (num > hospital_num) {
        return(NA)
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    ranked_order = order(state_sub[, 2], state_sub[, 1])
    state_sub[ranked_order, ][num, 1]
}