## For objekter som skal være tilgjengelig i både ui.R og server.R

raw.data <- read.csv("./data/annonym2017jan.csv",
                 header = TRUE,
                 sep = ";",
                 fileEncoding = "latin1",
                 stringsAsFactor = FALSE,
                 strip.white = TRUE)

source("./codes/clean-file.R")

data <- clean(data = raw.data)
