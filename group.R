require(spdep)

group <- function(spdf, num, distribute.leftovers=F, dense.first=F) {
  # Group the nearest num points together
  #
  # spdf: a SpatialPointsDataFrame
  # num: the target number of points to put in a group
  # distribute.leftovers: distribute "leftover" points in last group
  # dense.first: select dense groups first; otherwise sparse groups first
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
    #TODO use submatrix instead of recalculating, test carefully
    sums <- calc.distances(spDists(spdf[nogroup(),]), knn)
    
    # find the member with longest distance within neighbor group
    if (dense.first) {
      fun <- min
    } else {
      fun <- max
    }
    outlier <- head(which(sums == fun(sums)), n=1)
    candidates <- ids[sub.ids[c(outlier, knn$nn[outlier,])]]
    
    # assign members and dupes up to num to current group
    #TODO consider dupes
    spdf$group[candidates] <- group

    group <- group + 1
  }

  # merge group for duplicate coords
  # ... in an awkward way: workaround for strange spatial merge
  df1 <- data.frame(long=spdf.orig@coords[,1], lat=spdf.orig@coords[,2])
  df2 <- data.frame(long=spdf@coords[,1], lat=spdf@coords[,2], group=spdf$group)
  m <- cbind(df1, df2[match(df1$long * df1$lat, df2$long * df2$lat),])

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
