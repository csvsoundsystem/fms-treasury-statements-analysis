library(reshape2)
library(stringr)

strip <- function(string) {
    # Strip leading and trailing spaces
    str_replace(str_replace(string, '^ *', ''), ':? *$', '')
}

number <- function(string){
    # Remove the leading dollar sign, remove commas, and convert to numeric.
    as.numeric(str_replace_all(str_replace(strip(string), '^[$]?(?:[0-9]/)?', ''), ',', ''))
}

fms <- read.fwf('/tmp/summary.fixie',
    c(9, 50, 13, 1, 13, 1, 13), skip = 1,
    col.names = c('file', 'item', 'today', 'space1', 'mtd', 'space2', 'ytd')
)[-c(4,6)]

fms <- data.frame(lapply(fms, strip))
fms[c('today', 'mtd', 'ytd')] <- data.frame(lapply(fms[c('today', 'mtd', 'ytd')], number))
