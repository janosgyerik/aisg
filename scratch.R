# RStudio memo
# control 1: console
# control 2: editor
# control option right: next tab
# cmd shift n: new file

df <- read.csv('data/members.csv', col.names=c('lat', 'long', 'addr', 'raw_addr'))
df <- head(df)

#   df$group[grep('Fourqueux', df$addr)] <- 'f'

source('group.R')
df <- group(df, 2)

