library(sqldf)
capitolwords <- sqldf('select * from spending', dbname = '/tmp/capitolwords.db')
