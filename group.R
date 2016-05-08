require(sp)
require(spdep)

group <- function(df, num) {
  # Group the nearest num points together
  #
  # df: data.frame with lat and long columns
  # num: the target number of points to put in a group
  #
  # return same data.frame as input + group column to indicate group
  
  # convert to SpatialPointsDataFrame, giving it @coords
  coordinates(df) <- c('long', 'lat')
  
  # add $group, set to all zeros, meaning "no group"
  df$group <- rep(0, nrow(df))
  
  # subset with no duplicate coordinates
  nodups <- subset(df, !duplicated(df@coords))
  
  # distance matrix
  dist <- spDists(nodups, longlat = F)
  dist <- spDists(df, longlat = F)  #TODO delete
  
  ids <- c(1:nrow(df))
  
  group <- 1
  
  nogroup <- function() df$group == 0
  
  # repeat while there are unassigned group values
  while (T) {
    count <- sum(nogroup())
    if (count <= num) {
      df$group[nogroup()] <- group
      break
    }
    
    # find nearest neighbors
    knn <- knearneigh(df[nogroup(),], k=num-1)
    sub.ids <- ids[nogroup()]
    
    # calculate the sum of distances within each group
    sums <- calc.distances(dist, knn)
    
    # find the member with longest distance
    outlier <- head(which(sums == max(sums)), n=1)
    candidates <- ids[sub.ids[c(outlier, knn$nn[outlier,])]]
    
    # assign members and dupes up to num to current group
    #TODO consider dupes
    df$group[candidates] <- group

    group <- group + 1
  }
  df
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
