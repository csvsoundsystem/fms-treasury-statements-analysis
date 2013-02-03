library(plyr)

# Load the data
table2.raw <- read.csv('table2-std.csv')
table2 <- table2.raw[!table2.raw$is_total,c('date', 'type', 'item', 'today')]

# Fix types
table2$date <- as.Date(table2$date)
table2$type <- factor(table2$type)
table2$item <- factor(table2$item)
table2$today <- as.numeric(table2$today)

# Make faster
table2 <- table2[1:1000,]

# Select only the items that are present on all days.
n.days <- length(unique(table2$date))
table2 <- ddply(table2, c('type', 'item'), function(df) {
    if (nrow(df) == n.days) {
        df
    }
})

# Run PCA

items <- dcast(table2, date ~ type + item, value.var = 'today')[-1]
