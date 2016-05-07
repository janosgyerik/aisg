source('group.R')

test.all.points.to.group.1 <- function() {
  df <- data.frame(
    lat=c(1:5),
    long=c(1:5),
    addr=rep('rue', 5)
  )
  df <- group(df, nrow(df))
  checkEquals(rep(1, nrow(df)), df$group)
}

test.3.points.to.2.groups <- function() {
  df <- data.frame(
    lat=c(1, 2, 5),
    long=c(1, 2, 5),
    addr=rep('rue', 3)
  )
  df <- group(df, 2)
  checkEquals(c(2, 1, 1), df$group)
}

test.with.same.distance <- function() {
  df <- data.frame(
    lat=c(1, 2, 3, 4, 5),
    long=c(1, 2, 3, 4, 5),
    addr=rep('rue', 5)
  )
  df <- group(df, 2)
  checkEquals(c(1, 1, 2, 2, 3), df$group)
}

test.with.duplicate.points <- function() {
  df <- data.frame(
    lat=c(1, 2, 5, 5),
    long=c(1, 2, 5, 5),
    addr=rep('rue', 4)
  )
  df <- group(df, 2)
  checkEquals(c(2, 2, 1, 1), df$group)
}

test.calc.distances.with.3.points <- function() {
  df <- data.frame(
    lat=c(0, 0, 0),
    long=c(1, 2, 5),
    addr=rep('rue', 3)
  )
  coordinates(df) <- c('long', 'lat')
  dist <- spDists(df, longlat = F)
  knn <- knearneigh(df, 1)
  sums <- calc.distances(dist, knn)
  checkEquals(c(1, 1, 3), sums)
}

test.calc.distances.with.5.points <- function() {
  df <- data.frame(
    lat=c(0, 0, 0, 0, 0),
    long=c(1, 2, 3, 6, 7),
    addr=rep('rue', 5)
  )
  coordinates(df) <- c('long', 'lat')
  dist <- spDists(df, longlat = F)
  knn <- knearneigh(df, 2)
  sums <- calc.distances(dist, knn)
  checkEquals(c(4, 4, 4, 8, 8), sums)
}
