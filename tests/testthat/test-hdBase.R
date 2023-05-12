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
  expect_equal(names(hb$metadata()$hdTables), c("cars", "iris"))


  # From a data.frame
  hb <- hdBase(iris, license = "MIT")
  expect_equal(hb$name, "iris")

  hb$hdTables_slugs()

  hdts <- hb$hdTables
  expect_equal(hdTables_slugs(hdts), "iris")

  hb$metadata()
  expect_equal(names(hb$metadata()$hdTables), "iris")
  expect_equal(hb$meta$license, "MIT")

  # From a list of data.frames

  l <- list(cars = head(cars), iris = head(iris))
  hb <- hdBase(l, license = "MIT")

  ht1 <- hb$hdTables[[1]]
  expect_equal(ht1$name, "cars")
  ht2 <- hb$hdTables[[2]]
  expect_equal(ht2$name, "iris")
  expect_equal(names(hb$metadata()$hdTables),c("cars","iris"))

  expect_equal(hb$hdBaseType, paste0(ht1$hdTableType,"__",ht2$hdTableType))
  expect_equal(hb$hdBaseTypeGroup,
               paste0(ht2$hdTableTypeGroup,"__",ht1$hdTableTypeGroup))

})
