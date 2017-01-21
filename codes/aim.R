aim <- function(data, title, size, pct1, pct2, col1, col2, col3) {

    if (missing(title)) {
        title <- " "
    }

    if (missing(size)) {
        size <- 14
    }

    if (missing(pct1)) {
        pct1 <- 50
    }

    if (missing(pct2)) {
        pct2 <- 80
    }

    if (missing(col2)) {
        col2 <- "#2171B5"
    }

    if (missing(col1)) {
        col1 <- "#99CCFF"
    }

    if (missing(col3)) {
        col3 <- "#000033"
    }

    names(data)[1] <- "aim"
    names(data)[2] <- "percent"
    levels(data$aim) <- gsub(" ", "\n", levels(data$aim))
    pct2a <- 2 + pct2

    p <- ggplot(data) +
        scale_x_discrete() +
        scale_y_reverse() +
        geom_rect(xmin=Inf, xmax = -Inf, ymin = 0 - pct2a, ymax = 0 - pct1, fill=col2) +
        geom_rect(xmin=Inf, xmax = -Inf, ymin = 0 - pct1, ymax = 0, fill=col1) +
        geom_rect(xmin=Inf, xmax = -Inf, ymin = -100, ymax = 0 - pct2, fill=col3) +
        geom_vline(xintercept=1:8, size=1.5, color="white") +
        geom_hline(yintercept=c(pct1, pct2, 100), size=0.1, color="white") +
        geom_point(aes(x=aim, y=percent, ymin=0, ymax=100),
                            shape=21, fill="#FF9933", size=size, position="identity") +
        ggtitle(title) +
        coord_polar() +
        theme(
            plot.title=element_text(face = "bold", color = "black", size = 17, ),
            panel.background=element_rect(fill = c("white")),
            panel.grid=element_blank(),
            panel.grid.major=element_line(size=2),
            panel.grid.minor.y=element_blank(),
            axis.text.x=element_text(vjust=5),
            axis.text=element_text(size = 12, color = "black", face = "bold"),
            axis.text.y=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.ticks=element_blank())

    return(p)
}
