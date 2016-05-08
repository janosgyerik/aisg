require(spdep)

group <- function(spdf, num, distribute.leftovers=F) {
  # Group the nearest num points together
  #
  # spdf: a SpatialPointsDataFrame
  # num: the target number of points to put in a group
  #
  # return spdf with a $group vector added

  # save original
  spdf.orig <- spdf

  # add $group, set to all zeros, meaning "no group"
  spdf$group <- rep(0, nrow(spdf))
  
  # remove points with duplicate coordinates
  spdf <- subset(spdf, !duplicated(spdf@coords))
  
  # distance matrix
  dist <- spDists(spdf)
  
  ids <- c(1:nrow(spdf))
  
  group <- 1
  
  nogroup <- function() spdf$group == 0
  
  # repeat while there are unassigned group values
  while (T) {
    count <- sum(nogroup())
    if (count <= num) {
      if (distribute.leftovers) {
        # distribute the leftovers evenly to nearest groups
        find.nearest.group <- function (rowname) {
          # note: make sure the row has unique ID, rbind fails otherwise
          row <- spdf[rowname,]
          row.names(row) <- nrow(spdf.orig) + 1

          tempdf <- rbind(spdf[!nogroup(),], row)
          knn <- knearneigh(tempdf, k=1)
          tempdf$group[knn$nn[nrow(knn$nn), 1]]
        }
        groups <- sapply(row.names(spdf[nogroup(),]), find.nearest.group)
        spdf$group[nogroup()] <- groups
      } else {
        # assign leftovers to last group
        spdf$group[nogroup()] <- group
      }
      break
    }
    
    # find nearest neighbors
    knn <- knearneigh(spdf[nogroup(),], k=num-1)
    sub.ids <- ids[nogroup()]
    
    # calculate the sum of distances within each group
    sums <- calc.distances(dist, knn)
    
    # find the member with longest distance
    outlier <- head(which(sums == max(sums)), n=1)
    candidates <- ids[sub.ids[c(outlier, knn$nn[outlier,])]]
    
    # assign members and dupes up to num to current group
    #TODO consider dupes
    spdf$group[candidates] <- group

    group <- group + 1
  }

  # workaround for strange spatial merge
  df1 <- data.frame(long=spdf.orig@coords[,1], lat=spdf.orig@coords[,2])
  df2 <- data.frame(long=spdf@coords[,1], lat=spdf@coords[,2], group=spdf$group)
  m <- merge(df1, df2, by=c('lat', 'long'), sort=F)
  spdf.orig$group <- m$group
  spdf.orig
}

calc.distances <- function(dist, knn) {
  # Calculate the sum of distances within groups
  #
  # dist: distance matrix
  # knn: matrix of nearest neighbors returned by knearneigh
  #
  # return a vector of sum of distances between nearest neighbors
  
  sapply(1:nrow(knn$nn), function(index) {
    cols <- c(index, knn$nn[index,])
    sum(dist[cols, cols]) / 2
  })
}
