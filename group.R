require(spdep)

group <- function(spdf, num) {
  # Group the nearest num points together
  #
  # spdf: a SpatialPointsDataFrame
  # num: the target number of points to put in a group
  #
  # return spdf with a $group vector added
  
  # add $group, set to all zeros, meaning "no group"
  spdf$group <- rep(0, nrow(spdf))
  
  # subset with no duplicate coordinates
  nodups <- subset(spdf, !duplicated(spdf@coords))
  
  # distance matrix
  dist <- spDists(nodups, longlat = F)
  dist <- spDists(spdf, longlat = F)  #TODO delete
  
  ids <- c(1:nrow(spdf))
  
  group <- 1
  
  nogroup <- function() spdf$group == 0
  
  # repeat while there are unassigned group values
  while (T) {
    count <- sum(nogroup())
    if (count <= num) {
      #TODO maybe better distribute the leftovers evenly
      spdf$group[nogroup()] <- group
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
  spdf
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
