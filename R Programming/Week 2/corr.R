corr <- function(directory, threshold = 0) {
    complete_cases <- complete(directory)
    filtered = complete_cases[complete_cases["nobs"] > threshold,]
    id = filtered[["id"]]
    corrs <- c()
    for (i in id) {
        file = paste(directory, sprintf("/%03d.csv", i), sep = "")
        df <- read.csv(file)
        cr <- cor(df[["nitrate"]], df[["sulfate"]], use = "complete.obs")
        corrs <- append(corrs, cr)
    }
    corrs
}