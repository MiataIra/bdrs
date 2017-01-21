## Lage antall og prosent

profun <- function(data, ...) {
    data %>%
        group_by_(...) %>%
        tally %>%
        mutate(pro = (100*n/sum(n)))
}
