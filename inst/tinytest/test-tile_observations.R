expect_identical(
  tile_observations(data.frame(day = 1:366, value = 1:366), "value"),
  data.frame(
    day = c(1:73 * 5L - 2L, 366L),
    value_avgs = c(1:73 * 5 - 2, 366),
    row.names = as.character(1:74)
  )
)
