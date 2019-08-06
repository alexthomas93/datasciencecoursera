pollutantmean <- function(directory, pollutant, id = 1:332) {
    pollutants <- c()
    for (i in id) {
        file = paste(directory, sprintf("/%03d.csv", i), sep = "")
        df <- read.csv(file)
        new_pollutants <- na.omit(df[[pollutant]])
        pollutants <- append(pollutants, new_pollutants)
    }
    mean(pollutants)
}