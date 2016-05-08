source('group.R')

make.spdf <- function(df) {
  df$junk <- rep('junk', nrow(df))
  coordinates(df) <- c('long', 'lat')
  df  
}

test.all.points.to.group.1 <- function() {
  df <- make.spdf(data.frame(
    long=c(1:5),
    lat=c(1:5)
  ))
  df <- group(df, nrow(df))
  checkEquals(rep(1, nrow(df)), df$group)
}

test.3.points.to.2.groups <- function() {
  df <- make.spdf(data.frame(
    long=c(1, 2, 5),
    lat=c(1, 2, 5)
  ))
  df <- group(df, 2)
  checkEquals(c(2, 1, 1), df$group)
}

test.with.same.distance <- function() {
  df <- make.spdf(data.frame(
    long=c(1, 2, 3, 4, 5),
    lat=c(1, 2, 3, 4, 5)
  ))
  df <- group(df, 2)
  checkEquals(c(1, 1, 2, 2, 3), df$group)
}

test.with.duplicate.points <- function() {
  df <- make.spdf(data.frame(
    long=c(1, 2, 5, 5),
    lat=c(1, 2, 5, 5)
  ))
  df <- group(df, 2)
  checkEquals(c(2, 2, 1, 1), df$group)
}

test.calc.distances.with.3.points <- function() {
  df <- make.spdf(data.frame(
    long=c(1, 2, 5),
    lat=c(0, 0, 0)
  ))
  dist <- spDists(df, longlat = F)
  knn <- knearneigh(df, 1)
  sums <- calc.distances(dist, knn)
  checkEquals(c(1, 1, 3), sums)
}

test.calc.distances.with.5.points <- function() {
  df <- make.spdf(data.frame(
    long=c(1, 2, 3, 6, 7),
    lat=c(0, 0, 0, 0, 0)
  ))
  dist <- spDists(df, longlat = F)
  knn <- knearneigh(df, 2)
  sums <- calc.distances(dist, knn)
  checkEquals(c(4, 4, 4, 8, 8), sums)
}
