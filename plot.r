library(ggplot2)
library(scales)

p.day <- ggplot(fms.item) + aes(color = item, x = date, y = today, group = item) +
    scale_x_date('Date') +
    scale_y_continuous('Amount', label = dollar) +
    geom_line()

p.rolling <- ggplot(melt(fms.rolling, 'date', variable.name = 'Item', value.name = 'Amount')) +
    aes(color = Item, x = date, y = Amount, group = Item) +
    scale_x_date('Date') +
    scale_y_continuous('Amount (7-business-day moving average)', label = dollar) +
    labs(title = 'Daily federal cash flow') +
    geom_line()

p.joined <- ggplot(fms.joined.molten) + aes(x = value, y = percentage, color = variable) +
    scale_x_continuous('Cash flow', label = dollar) +
    scale_y_continuous('Mentions of "spending" per total words', label = percent) +
    labs(title = 'Discussion of spending is higher when net change is near zero') +
    geom_point(alpha = 0.3)

p.rolling.joined <- ggplot(fms.rolling.joined.molten) + aes(x = value, y = percentage, color = variable) +
    scale_x_continuous('Cash flow (7-business-day rolling mean)', label = dollar) +
    scale_y_continuous('Mentions of "spending" per total words', label = percent) +
    labs(title = 'Discussion of spending has nothing to do with daily spending') +
    geom_point(alpha = 0.3)


fms.item$day.of.month <- as.numeric(strftime(fms.item$date, format = '%d'))
p.day.of.month <- ggplot(fms.item) +
    aes(x = day.of.month, y = today, group = item, color = item) +
    scale_x_continuous('Day of month') +
    scale_y_continuous('Amount', label = dollar) +
    labs(table = 'Spending is above or below $50,000 unless it\'s the middle or end of the month') +
    geom_jitter()

p.day.of.month.withdraw <- ggplot(subset(fms.item, item == 'Total Withdrawals (excluding transfers)')) +
    aes(x = day.of.month, y = today, group = item, color = item) +
    scale_x_continuous('Day of month') +
    scale_y_continuous('Amount', label = dollar) +
    labs(table = 'Spending is above or below $50,000 unless it\'s the middle or end of the month') +
    geom_jitter()

p.balance <- ggplot(fms.day) + aes(x = date, y = balance) +
    scale_x_date('Date') +
    scale_y_continuous('Balance', label = dollar) +
    geom_line()

pdf('exploratory.plots.pdf', width = 11, height = 8.5)
print(p.day)
print(p.rolling)
print(p.joined)
print(p.rolling.joined)
print(p.day.of.month)
print(p.day.of.month.withdraw)
print(p.balance)
dev.off()
