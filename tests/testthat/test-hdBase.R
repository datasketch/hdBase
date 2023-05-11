test_that("Create hdBase ", {

  # NULL case
  ts <- NULL
  hb <- hdBase(ts)

  expect_null(hb$hdTables)

  # From a list of hdTables
  t1 <- hdTable(cars)
  t2 <- hdTable(iris)
  ts <- list(t1,t2)
  hb <- hdBase(ts)
  hbts <- hb$hdTables
  expect_true(are_hdTables(hbts))
  slugs <- hdTables_slugs(hbts)
  expect_equal(slugs, c("cars", "iris"))



  # From a data.frame
  hb <- hdBase(iris, license = "MIT")
  expect_equal(hb$name, "iris")

  hb$hdTables_slugs()

  hdts <- hb$hdTables
  hdTables_slugs(hdts)

  hb$metadata()
  hb$

  expect_equal(hb$meta$license, "MIT")
  expect_equal(hb$metadata()$hdTables$d$name, "iris")

})
