
sumCI <- function(data, maalvar, gpvars, conf.Int=.95) {

    summ = data %>%
        filter_(.dots = lazyeval::interp(~!is.na(var), var = as.name(maalvar))) %>%
        group_by_(.dots = gpvars) %>%
        summarise_(.dots = list(
                       N = lazyeval::interp(~n()),
                       Mean =  lazyeval::interp(~mean(var, na.rm = TRUE), var = as.name(maalvar)),
                       SD =  lazyeval::interp(~sd(var, na.rm = T), var = as.name(maalvar))))
    ## For sample
    ## ciMult = qt(conf.Int/2 + .5, summ$N-1) #konvertere f.eks CI=95% blir .975

    ## For total populasjon
    ciMult = qt(conf.Int/2 + .5, summ$N) #konvertere f.eks CI=95% blir .975

    summ$SE = summ$SD/sqrt(summ$N)
    summ$CI = ciMult * summ$SE

    summ.na.zero <- data %>%
       tidyr::expand_(gpvars) %>%
       dplyr::left_join(summ)

    summ.na.zero[is.na(summ.na.zero)] <- 0

    return(invisible(summ.na.zero))

}
