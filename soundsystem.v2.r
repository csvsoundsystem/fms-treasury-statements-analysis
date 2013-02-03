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
    fed.rate <- read.csv('fed_rate.csv', stringsAsFactors = F)
    fed.rate$date <- strptime(fed.rate$date, format = '%m/%d/%y')
}
table2 <- table2.raw[!table2.raw$is_total,c('date', 'type', 'item', 'today')]

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

    x.pos <- x + abs(diff(range(table2.toplot$date)) / 20)

    plot.faces(f, face.type = 1, x.pos = x.pos, y.pos = y, ...)
}

# Other plot stuff
table2.tmp <- ddply(table2.pca, 'date', function(df) { c(error = sd(df$today)) })
table2.toplot <- join(join(table2.tmp, fms.day[c('date', 'balance')]), fed.rate)

# Remove NAs
table2.toplot[c(358, 833, 1022, 1393, 1398),] <- table2.toplot[c(358, 833, 1022, 1393, 1398) - 1,]

# Skip the top 40 for rolling.
table2.toplot <- table2.toplot[-(1:40),]

# Video frame
bg.of.week <- c(
  Sunday = '#DDDDDD',
  Monday = '#FFDDDD',
  Tuesday = '#DDFFDD',
  Wednesday = '#DDDDFF',
  Thursday = '#DDFFFF',
  Friday = '#FFFFDD',
  Saturday = '#FFDDFF'
)
fg.of.week <- c(
  Sunday = '#000000',
  Monday = '#00DDDD',
  Tuesday = '#DDDD00',
  Wednesday = '#DD00DD',
  Thursday = '#DD0000',
  Friday = '#00DD00',
  Saturday = '#0000DD'
)
frame <- function(i) {
    if (i <= 2) {
        return
    }
    day.of.week.a <- strftime(table2.toplot[i,'date'], format = '%A')
    bg <- bg.of.week[day.of.week.a][[1]]
    fg <- fg.of.week[day.of.week.a][[1]]
    par(
        bg = bg
    )
    plot(
        table2.toplot[1:i,'balance'] ~ table2.toplot[1:i,'date'],
        type = 'n',
        xlim = range(table2.toplot$date),
        ylim = c(-2e5, 7e5), #range(table2.toplot$balance),
        xlab = '', #Date
        ylab = 'Cash in the bank (billions)', main = '', #'FMS Soundsystem',
        axes = F, col = 4 # so we notice errors
    )
    abline(h = 0, col = fg)

    # Rate
    #range(table2.toplot$balance)

    # Balance
    polygon(
        c(table2.toplot[1:i,'date'], table2.toplot[i:1,'date']),
        c(table2.toplot[1:i,'balance'], table2.toplot[i:1,'balance']) + c(table2.toplot[1:i,'error'], - table2.toplot[i:1,'error']),
        col = 1
    )

    # Under balance
    rect(
        xright =  min(table2.toplot$date),
        ybottom = mean(range(table2.toplot$balance)) * 0.95,
        xleft =   weighted.mean(range(table2.toplot$date), c(18, 3)),
        ytop    = mean(range(table2.toplot$balance)) * 1.15,
        col = 1
    )
    text(
        x = min(table2.toplot$date),
        y = mean(range(table2.toplot$balance)) * c(1.1, 1),
        labels = c(
            'Balance',
            sub('\\$-', '-$', paste('$', as.character(round(table2.toplot[i,'balance'] / 1000)), ' billion', sep = ''))
        ),
        pos = 4, font = 2:1, col = fg
    )

    # Under interest rate
    rect(
        xleft =   weighted.mean(range(table2.toplot$date), c(3, 18)),
        ybottom = mean(range(table2.toplot$balance)) * 0.95,
        xright =  max(table2.toplot$date),
        ytop    = mean(range(table2.toplot$balance)) * 1.15,
        col = 1
    )
    text(
        x = max(table2.toplot$date),
        y = mean(range(table2.toplot$balance)) * c(1.1, 1),
        labels = c(
            'Interest rate',
            sub('\\$-', '-$', paste('$', as.character(round(table2.toplot[i,'balance'] / 1000)), ' billion', sep = ''))
        ),
        pos = 2, font = 2:1, col = fg
    )

    # Under main
    rect(
        xleft   = weighted.mean(range(table2.toplot$date), c(2, 9)),
        ybottom = weighted.mean(range(table2.toplot$balance), c(2, 15)),
        xright  = max(table2.toplot$date),
        ytop    = max(table2.toplot$balance),
        col = 1
    )
    text(
        x = weighted.mean(range(table2.toplot$date), c(1, 9)),
        y = c(
            weighted.mean(range(table2.toplot$balance), c(1, 15)),
            weighted.mean(range(table2.toplot$balance), c(2, 15))
        ),
        labels = c(
            'FMS Soundsystem',
            strftime(table2.toplot[i,'date'], format = '%B %Y')
        ),
        col = fg, pos = 3, font = 2:1
    )

    ticks <- seq(-2e5, 6e5, 1e5)
    axis(2, at = ticks, labels = round(ticks / 1000))
    face(i,
        x = table2.toplot[i,'date'],
        y = table2.toplot[i,'balance'],
        height = abs(diff(range(table2.toplot$balance))) / 5,
        width = abs(diff(range(table2.toplot$date))) / 10,
        labels = ''
    )

    par(new = T)
    plot(
         table2.toplot[1:i,'rate'] ~ table2.toplot[1:i,'date'],
         axes = F, xlab = '', ylab = '', type = 'l', lty = 2,
         xlim = range(table2.toplot$date), ylim = c(-2, max(table2.toplot$rate))
    )
    mtext("Federal interest rate", side=4, line=3)
    axis(4, at = 0:5, labels = paste(0:5, '%', sep = ''), lty = 2)
}

# frame(30)

main.plots <- function() {
    for (i in 1:nrow(table2.toplot)) {
         png(sprintf('slideshow/%d.png', i), width = 1200, height = 600) 
         frame(i)
         dev.off()
    }
}
