# RStudio memo
# control 1: console
# control 2: editor
# control option right: next tab
# cmd shift n: new file

source('group.R')

df <- read.csv('data/members.csv', col.names=c('lat', 'long', 'addr', 'raw_addr'))
# df <- head(df)

# convert to SpatialPointsDataFrame, giving it @coords
coordinates(df) <- c('long', 'lat')

df <- subset(df, !duplicated(df$lat * df$long))

df <- group(df, 20)
