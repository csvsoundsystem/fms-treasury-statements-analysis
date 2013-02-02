library(ggplot2)
library(scales)

p.day <- ggplot(fms.day) + aes(color = item, x = date, y = today, group = item) +
    scale_x_date('Date') +
    scale_y_continuous('Amount', label = dollar) +
    geom_line()

p.rolling <- ggplot(melt(fms.rolling, 'date', variable.name = 'Item', value.name = 'Amount')) +
    aes(color = Item, x = date, y = Amount, group = Item) +
    scale_x_date('Date') +
    scale_y_continuous('Amount (7-business-day moving average)', label = dollar) +
    labs(title = 'Daily federal cash flow') +
    geom_line()
