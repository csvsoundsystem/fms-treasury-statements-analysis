library(plyr)
library(aplpack)
library(reshape2)

# Load the data
if (!'table2.raw' %in% ls()) {
    table2.raw <- read.csv('table2-std.csv')

    # Fix types
    table2.raw$date <- as.Date(table2.raw$date)
    table2.raw$type <- factor(table2.raw$type)
    table2.raw$item <- factor(table2.raw$item)
    table2.raw$today <- as.numeric(table2.raw$today)

    source('data.r')
}
table2 <- table2.raw[!table2.raw$is_total,c('date', 'type', 'item', 'today')]

# Make faster
table2 <- table2[table2$date > as.Date('2012-11-02'),]

# Select only the items that are present on all days.
n.days <- length(unique(table2$date))
table2.pca <- ddply(table2, c('type', 'item'), function(df) {
    if (nrow(df) == n.days) {
        df
    }
})

# Run PCA
items <- dcast(table2.pca, date ~ type + item, value.var = 'today')[-1]
pca <- princomp(items, cor = T)
pca.stuff <- function() {
    summary(pca)
    plot(pca$sdev ~ I(1:length(pca$sdev)))
}
factored <- t(scale(items, center = pca$center, scale = pca$scale) %*% pca$loadings)

# Plot a Chernoff face for a day at an x, y
face <- function(day.or.days, ...) {
    # Day is a row index
    f <- faces(t(factored)[day.or.days,1:15], face.type = 0, plot = F)
    plot.faces(f, face.type = 0, ...,)
}

table2.tmp <- ddply(table2.pca, 'date', function(df) { c(error = sd(df$today)) })
table2.toplot <- join(table2.tmp, fms.day[c('date', 'balance')])

frame <- function(i) {
    if (i <= 2) {
        return
    }
    plot(
        table2.toplot[1:i,'balance'] ~ table2.toplot[1:i,'date'],
        type = 'l', lwd = table2.toplot[1:i,'error'] / 4000,
        xlim = range(table2.toplot$date),
        ylim = range(table2.toplot$balance)
    )
    face(i,
        x.pos = table2.toplot[i,'date'],
        y.pos = table2.toplot[i,'balance'],
        height = abs(diff(range(table2.toplot$balance))) / 5,
        width = abs(diff(range(table2.toplot$date))) / 5
    )
    print(table2.toplot[i,])
}

frame(30)
