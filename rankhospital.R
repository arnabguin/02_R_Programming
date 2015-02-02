
source("best.R")
rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    if (num == "best") {
        best(state,outcome)
    }
    if (num != "worst") { 
        if (!is.numeric(num)) {
            stop("Wrong number - must be best or worst or number greater zero")
        } 
        else {
            if (as.integer(num) <= 0) {
                stop("Wrong number - must be integer greater than zero")
            }
        }
    }

    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    states <- unique(outcome_data[,"State"])
    if (!(state %in% states)) {
        stop("Invalid state")
    }
    validoutcomes <- c("heartattack","heartfailure","pneumonia")
    outcome <- gsub(" ","",outcome)
    if (!(outcome %in% validoutcomes)) {
        stop("Invalid outcome")
    }
    ## The results are sorted and this works because the valid outcomes are in sorted order as well
    # Find out column names
    searchCols <- grep ("^Hospital.30.Day.Death",names(outcome_data),value=TRUE)
    # Create data frame with outcome -> column name mapping
    searchDataFrame <- data.frame("heartattack"=as.character(searchCols[1]),
                                  "heartfailure"=as.character(searchCols[2]),
                                  "pneumonia"=as.character(searchCols[3]))
    outcomeForState <- subset(outcome_data,outcome_data$State == state)
    mortality <- as.character(searchDataFrame[,outcome])
    col <- as.numeric(outcomeForState[[mortality]])

    # Worst - Find all rows containing maximum values for columns
    if (num == "worst") {
        maxRows <- which (col == max(col,na.rm=TRUE),arr.ind=TRUE)
        ## Return hospital name in that state with highest 30-day death
        ## rate
        sort(outcomeForState[as.vector(maxRows),"Hospital.Name"],decreasing=TRUE)[1]
    }
    else {
        ## Return hospital name in that state with n-th 30-day death rate
        if (num > length(col)) {
            NA
        }
        # create a data frame with hospital name and ordering
        nth_mortality_index <- order(col,na.last=NA)[num]
        nth_mortality_value <- col[nth_mortality_index]

        nth_mortality_ties <- grep(TRUE,col == nth_mortality_value)
        sort(outcomeForState[nth_mortality_ties,"Hospital.Name"],decreasing=TRUE)[1]
    }
}
