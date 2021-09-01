test_that("group time works", {
  times <- as.POSIXct(0:9, origin = '1970-01-01', tz = 'UTC')
  expect_equal(group_time(as.POSIXct(times), 10),
               rep(as.POSIXct('1970-01-01 00:00:05', tz = 'UTC'), 10))
})
