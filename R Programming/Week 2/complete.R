complete <- function(directory, id = 1:332) {
  nobs <- c()
  for (i in id) {
    file = paste(directory, sprintf("/%03d.csv", i), sep = "")
    df <- read.csv(file)
    nob <- sum(complete.cases(df))
    nobs <- append(nobs, nob)
  }
  data.frame(id=id, nobs=nobs)
}