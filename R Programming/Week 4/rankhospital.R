rankhospital <- function(state, outcome, num = "best") {
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    if (state %in% df$State) {
        if (outcome %in% c("heart attack", "heart failure", "pneumonia")) {
            if (num == "best" || num == "worst" || is.numeric(num)) {
                outcome_col <- if (outcome == "heart attack") {
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
                } else if (outcome == "heart failure") {
                    "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
                } else {
                    "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
                }
                # Convert outcome_col to numeric
                df[[outcome_col]] <- suppressWarnings(as.numeric(df[[outcome_col]]))
                # Drop rows with NA values in outcome_col
                dropped <- df[!is.na(df[[outcome_col]]),]
                # Filter by state
                state_df = dropped[dropped$State == state,]
                # Order by outcome and hospital name
                ordered_df = state_df[order(state_df[outcome_col], state_df$Hospital.Name),]
                index = if (num == "best") 1 else if (num == "worst") nrow(ordered_df) else num
                # Return the hospital name of the first row of ordered_df
                ordered_df$Hospital.Name[index]
            } else {
                stop("invalid num")
            }
        } else {
            stop("invalid outcome")
        }
    } else {
        stop("invalid state")
    }
}