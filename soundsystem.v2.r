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

# Make faces
f.all <- faces(t(factored)[,1:15], plot = F, print.info = F)

# Plot a Chernoff face for a day at an x, y
face <- function(day.or.days, x, y, ...) {
    # day.or.days is a row index

    f <- f.all
    f$xy <- f$faces <- NULL

    f$xy <- matrix(f.all$xy[,day.or.days])
    dimnames(f$xy) <- dimnames(f.all$xy)
    f$faces <-f.all$faces[day.or.days]

    plot.faces(f, face.type = 1, x.pos = x, y.pos = y, ...)
}

# Other plot stuff
table2.tmp <- ddply(table2.pca, 'date', function(df) { c(error = sd(df$today)) })
table2.toplot <- join(table2.tmp, fms.day[c('date', 'balance')])

# Video frame
colors.of.week <- c(
  Sunday = '#DDDDDD',
  Monday = '#FFDDDD',
  Tuesday = '#DDFFDD',
  Wednesday = '#DDDDFF',
  Thursday = '#DDFFFF',
  Friday = '#FFFFDD',
  Saturday = '#FFDDFF'
)
frame <- function(i) {
    if (i <= 2) {
        return
    }
    day.of.week.a <- strftime(table2.toplot[i,'date'], format = '%A')
    par(bg = colors.of.week[day.of.week.a][[1]])
    plot(
        table2.toplot[1:i,'balance'] ~ table2.toplot[1:i,'date'],
        type = 'n',
        xlim = range(table2.toplot$date),
        ylim = range(table2.toplot$balance),
        xlab = '', #Date
        ylab = 'Cash in the bank (millions)', main = 'FMS Soundsystem',
        axes = F, col = 2
    )
    polygon(
        c(table2.toplot[1:i,'date'], table2.toplot[i:1,'date']),
        c(table2.toplot[1:i,'balance'], table2.toplot[i:1,'balance']) + c(table2.toplot[1:i,'error'], - table2.toplot[i:1,'error']),
        col = 1
    )
    text(
        x = max(table2.toplot$date),
        y = c(3000, 0) + mean(range(table2.toplot$balance)),
        labels = c(
            strftime(table2.toplot[i,'date'], format = '%B %Y'),
            sub('\\$-', '-$', paste('$', as.character(table2.toplot[i,'balance']), ' million', sep = ''))
        ),
        pos = 2, font = 2:1

    )
  # axis(1, at = range(table2.toplot$date), labels = range(table2.toplot$date))
    axis(2)
    face(i,
        x = table2.toplot[i,'date'],
        y = table2.toplot[i,'balance'],
        height = abs(diff(range(table2.toplot$balance))) / 5,
        width = abs(diff(range(table2.toplot$date))) / 10,
        labels = ''
    )
  # print(table2.toplot[i,])
}

frame(30)
