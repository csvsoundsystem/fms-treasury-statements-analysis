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
face <- function(day.or.days, x.pos, y.pos) {
    # Day is a row index
    f <- faces(t(factored)[day.or.days,1:15], face.type = 2, plot = F)
    plot.faces(f, x.pos = x.pos, y.pos = y.pos, face.type = 2, width = 20, height = 20 * 1.6)
}

table2.tmp <- table2.pca
balance.at.start <- 8035
table2.tmp[table2.tmp$type == 'withdrawal','today'] <- -table2.tmp[table2.tmp$type == 'withdrawal','today']
table2.tmp <- table2.tmp[order(table2.tmp$date),]
table2.tmp$balance <- cumsum(table2.tmp$today) + balance.at.start
table2.toplot <- ddply(table2.tmp, 'date', function(df) { c(balance = mean(df$balance), error = sd(df$today)) })
# rownames(factored) <- rownames(table2.toplot) <- table2.toplot$date

frame <- function(i) {
    if (i == 1) {
        return
    }
    plot(
        table2.toplot[1:i,'balance'] ~ table2.toplot[1:i,'date'],
        type = 'l', cex = table2.toplot[1:i,'error'],
        xlim = range(table2.toplot$date),
        ylim = c(0, max(table2.toplot$balance))
    )
    face(i, table2.toplot[i,'date'], table2.toplot[i, 'balance'])
    print(table2.toplot[i,])
}
