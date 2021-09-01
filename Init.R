#install.packages("iplots",dep=TRUE)
#install.packages("ggplot2")
library(edf)
require(ggplot2)
#source("~/ASIC_Study/ASIC_vs_SphygmoCor/AdditionalR/1_Datenformatierung.R")

#library("PerformanceAnalytics")
#source("~/R-Skripte/DataAnalysis/CorrelationTools.R")

#source("~/R-Skripte/DataAnalysis/BasicPlotting.R")


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols, nrow = ceiling(numPlots / cols))
    }

    if (numPlots == 1) {
        print(plots[[1]])

    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
        }
    }
}

ggboxplot <- function(plotdf, xcol, ycol, jitter, title) {
    p <- ggplot(plotdf, aes(y = plotdf[, ycol], x = plotdf[, xcol], na.rm = T)) +
    stat_boxplot(geom = 'errorbar', linetype = 1, width = 0.5) + #whiskers
    geom_boxplot(outlier.shape = 1) +
    stat_summary(fun.y = mean, geom = "point", size = 2) +
    geom_jitter(width = 0.2, aes(colour = plotdf[, jitter])) +
    xlab(xcol) +
    ylab(ycol) +
    labs(geom_jitter = jitter) +
    ggtitle(title)

    return(p)
}

nametree <- function(X, prefix1 = "", prefix2 = "", prefix3 = "", prefix4 = "")
    if (is.list(X))
        for (i in seq_along(X)) {
            cat(if (i < length(X)) prefix1 else prefix3, names(X)[i], "\n", sep = "")
            prefix <- if (i < length(X)) prefix2 else prefix4
            nametree(
        X[[i]],
        paste0(prefix, "?????????"),
        paste0(prefix, "???  "),
        paste0(prefix, "?????????"),
        paste0(prefix, "   ")
      )
        }
#nametree(fulldata)

convertseconds <- function(time) {
    h <- time %/% 3600
    time <- time - h * 3600
    m <- time %/% 60
    time <- time - m * 60
    s <- time
    return(paste(h, ":", m, ":", s, sep = ""))
}

file <- "~/PulseWave/Files/TD02052000.rec"

fulldata <- read.edf(file)
