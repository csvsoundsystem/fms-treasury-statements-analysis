library(sqldf)
library(plyr)

capitolwords <- sqldf('select * from spending', dbname = '/tmp/capitolwords.db')
capitolwords$date <- as.Date(capitolwords$day)
capitolwords$day <- NULL

fms.joined <- join(fms.day, capitolwords)
names(fms.joined) <- c("date", "net.change", "deposits", "withdrawals", "count", "percentage", "total", "raw_count")

fms.joined.molten <- melt(na.omit(fms.joined[c('date', 'net.change', 'withdrawals', 'deposits', 'percentage')]), c('date', 'percentage'))
