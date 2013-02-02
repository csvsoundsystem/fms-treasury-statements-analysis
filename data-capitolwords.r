library(sqldf)
library(plyr)

capitolwords <- sqldf('select * from spending', dbname = '/tmp/capitolwords.db')
capitolwords$date <- as.Date(capitolwords$day)
capitolwords$day <- NULL

fms.joined <- join(fms.day, capitolwords)
fms.rolling.joined <- join(fms.rolling, capitolwords)

names(fms.joined) <- c("date", "net.change", "deposits", "withdrawals", "count", "percentage", "total", "raw_count")
names(fms.rolling.joined) <- c("net.change", "deposits", "withdrawals", "date", "count", "percentage", "total", "raw_count")

fms.joined.molten <- melt(na.omit(fms.joined[c('date', 'net.change', 'withdrawals', 'deposits', 'percentage')]), c('date', 'percentage'))
fms.rolling.joined.molten <- melt(na.omit(fms.rolling.joined[c('date', 'net.change', 'withdrawals', 'deposits', 'percentage')]), c('date', 'percentage'))
