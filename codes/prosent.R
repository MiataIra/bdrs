## lage antall og prosent

prosent <- function(dataIN, ...) {

    library(dplyr)
    dataIN %>%
        group_by_(...) %>%
        tally %>%
        mutate(yAksen = (100*n/sum(n)))
}
