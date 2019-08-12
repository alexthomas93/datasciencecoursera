rankall <- function(outcome, num = "best") {
    if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        if (num == "best" || num == "worst" || is.numeric(num)) {
            outcome_col <- if (outcome == "heart attack") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
            } else if (outcome == "heart failure") {
                "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
            } else {
                "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
            }
            # Load outcome-of-care-measures.csv
            df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
            # Convert outcome_col to numeric
            df[[outcome_col]] <- suppressWarnings(as.numeric(df[[outcome_col]]))
            # Drop rows with NA values in outcome_col
            df <- df[!is.na(df[[outcome_col]]),]
            # Create an sapply function
            rankhospital <- function(x) {
                # Order by outcome and hospital name
                ordered = x[order(x[outcome_col], x$Hospital.Name),]
                index = if (num == "best") 1 else if (num == "worst") nrow(ordered) else num
                # Return the hospital name of the first row of ordered
                ordered$Hospital.Name[index]
            }
            # Apply the rankhospital function to get the num-ranked hospital for each state
            hospitals = sapply(split(df, df$State), rankhospital)
            states = names(hospitals)
            # Return the results as a data frame
            data.frame(hospital=hospitals, state=states, row.names=states)
        } else {
            stop("invalid num")
        }
    } else {
        stop("invalid outcome")
    }
}