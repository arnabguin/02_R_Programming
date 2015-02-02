
best <- function(state,outcome) {
    ## Read outcome data
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
    # Find all rows containing minimum values for columns
    minRows <- which (col == min(col,na.rm=TRUE),arr.ind=TRUE)
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    sort(outcomeForState[as.vector(minRows),"Hospital.Name"])[1]
}

