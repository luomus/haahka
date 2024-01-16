records <- data.frame(
  date = as.Date("2000-01-01"),
  am = 1,
  am_ = TRUE,
  sm = 0,
  sm_ = TRUE
)

expect_identical(get_value("Autumn", "Migr", "Sum", records), "1")
expect_identical(
  get_value("Autumn", "Migr", "date_string", records), "2000-04-10"
)
expect_identical(get_value("Spring", "Migr", "Sum", records), "-")
