# Load the data
table2.raw <- read.csv('table2.csv')
table2 <- table2.raw[!table2.raw$is_total,c('date', 'type', 'item', 'today')]

# Make faster
table2 <- table2[1:100,]

# Run PCA
items <- dcast(table2, date ~ type + item, value.var = 'today', fun.aggregate = mean)[-1]
