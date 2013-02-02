library(reshape2)
library(stringr)
library(zoo)

strip <- function(string) {
    # Strip leading and trailing spaces
    str_replace(str_replace(string, '^ *', ''), ':? *$', '')
}

number <- function(string){
    # Remove the leading dollar sign, remove commas, and convert to numeric.
    as.numeric(str_replace_all(str_replace(strip(string), '^[$]?(?:[0-9]/)?', ''), ',', ''))
}

fms <- read.fwf('summary.fixie',
    c(9, 50, 13, 1, 13, 1, 13), skip = 1,
    col.names = c('file', 'item', 'today', 'space1', 'mtd', 'space2', 'ytd')
)[-c(4,6)]

# Remove na
fms <- na.omit(fms)

# Strip spaces
fms <- data.frame(lapply(fms, strip))

# Make numbers
fms[c('today', 'mtd', 'ytd')] <- data.frame(lapply(fms[c('today', 'mtd', 'ytd')], number))

# Make dates
fms$date <- as.Date(strptime(fms$file, format = '%Y%m%d'))
fms$file <- NULL

# Different slices
fms.item <- fms
fms.day <- dcast(fms, date ~ item, value.var = 'today')
rm(fms)

# Rolling mean
fms.rolling <- data.frame(lapply(fms.day[-1], rollmean, k = 7))
names(fms.rolling) <- c('net.change', 'deposits', 'withdrawals')
fms.rolling$date <- fms.day$date[4:(nrow(fms.day) - 3)]

balance.at.start <- 8035
fms.day$balance <- cumsum(fms.day[,'Net Change in Operating Cash Balance']) + balance.at.start
