expect_identical(
  tile_observations(data.frame(day = 1:366, value = 1:366), "value"),
  data.frame(
    day = as.Date(paste(2000, c(1:73 * 5L - 2L, 366L)), format = "%Y %j"),
    value = c(1:73 * 5 - 2, 366)
  )
)
