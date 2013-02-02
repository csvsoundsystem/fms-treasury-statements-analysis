library(reshape2)
library(stringr)
library(zoo)
library(scales)
library(lubridate)
library(plyr)

setwd("/Users/brian/Dropbox/code/fms-treasury-statements-analysis")
d <- read.csv("fms.day.csv", stringsAsFactors=F)

z <- function(x) {
    (x-mean(x))/sd(x)
}

plot(density(d$withdrawals), type="n")
polygon(density(d$withdrawals), col=alpha("#ffcc00", 0.3))
polygon(density(d$deposits), col=alpha("#00ccff", 0.3))
legend(x=100000, y=2.0e-05, legend=c("withdrawals", "deposits"), fill=c("#ffcc00","#00ccff"))

d$weekday <- weekdays(as.Date(d$date))
wkdy <- ddply(d, .(weekday), function(x){
    median(x$change)
    })
wkdy <- wkdy[c(2,4,5,3,1),]
barplot(wkdy$V1)
(d$withdrawals)
d$zw <- ifelse(abs(d$w_d)>2, 1, 0)
d$cw <- z(d$change)
y <- d$withdrawals
plot(x,y, type="l", lwd=0.1)
for(i in 1:nrow(d)) {
    if(d$big[i]==1) {
        abline(v=x[i], col="blue", lwd=0.5)
    }
    if(d$zw[i]==1) {
        abline(v=x[i], col="red", lwd=0.5)
    }
}