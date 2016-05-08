source('group.R')

df <- read.csv('data/members.csv', col.names=c('lat', 'long', 'addr', 'raw_addr'))

# convert to SpatialPointsDataFrame, giving it @coords
coordinates(df) <- c('long', 'lat')

spdf <- group(df, 20)
spdf <- group(df, 20, distribute.leftovers = T)

opar <- par(mfrow=c(1,1), mar = c(2,2,2,2), mgp = c(1.5,0.5,0), tck = -0.02)

# plot(sdf2, border="darkgreen", col=NULL, lwd=.5)
# plot(idf_sub_dep, border="#FFFFFF", col="#DDDDDD", lwd=1, add=TRUE)
# plot(sdf2, border="darkgreen", col="#FFFFCC", lwd=.3, add=TRUE)

plot(df, col=df$group, bg=df$group, pch=6, cex=.8)
