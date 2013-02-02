library(reshape2)
library(stringr)
library(zoo)
setwd("/Users/brian/Dropbox/code/fms-treasury-statements-analysis")
d <- read.csv("fms.day.csv", stringsAsFactors=F)

z <- function(x) {
    (x-mean(x))/sd(x)
}
d$w_d <- z(d$withdrawals)
d$w_d <- ifelse(abs(d$w_d)>4, 1, 0)
d$big <- ifelse(d$change>50000, 1, 0)
x <- as.Date(d$date)

for(i in 1:nrow(d)){jkk1
    if(d$big[i]==1) {
        abline(v=x[i], col="blue", lwd=0.1)
    }
}