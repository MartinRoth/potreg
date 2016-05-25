#' Select seasonal subset and set season index
#' @param dat data.table of the original data
#' @param season String with the full season name
#' @import data.table
#' @importFrom zoo rollapply
#' @export
SubsetSeasons <- function(dat, season) {
  stopifnot(is.data.table(dat))
  stopifnot(all(c("year", "month") %in% names(dat)))
  stopifnot(season %in% c("winter", "spring", "summer", "autumn"))
  indicator <- NULL
  switch (season,
          winter = season <- c(12,  1,  2),
          spring = season <- c( 3,  4,  5),
          summer = season <- c( 6,  7,  8),
          autumn = season <- c( 9, 10, 11)
  )
  newDat <- dat[month %in% season, ]
  setkey(newDat, year, month)

  indexDat <- newDat[, list(month = unique(month)), by = year]
  setkey(indexDat, year, month)

  indexDat[, indicator := FALSE]
  indexDat[-c(.N, .N-1),
           indicator := rollapply(indexDat[, month], 3, identical, season)]
  indexDat[indicator==FALSE, indicator := NA]
  indexDat[, season := 0L]
  indexDat[indicator==TRUE, season := cumsum(indicator)]
  indexDat[, season := (season + shift(season, fill = 0) +
                               shift(season, 2, fill = 0))]
  indexDat[, indicator := NULL]
  newDat <- merge(newDat, indexDat)
  return(newDat[season > 0])
}
