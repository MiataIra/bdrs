## lage antall og prosent

prosent <- function(dataIN, var) {

    library(dplyr)
    dataIN %>%
        group_by_(.dots = var) %>%
        tally %>%
        mutate(yAksen = (100*n/sum(n)))
}
