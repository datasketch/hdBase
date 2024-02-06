test_that("hdbase table", {

  # update
  l <- list(cars = cars, mtcars = mtcars)
  hb <- hdbase(l)
  new_cars <- tibble::as_tibble(cars[1:2,])
  new_cars$n <- 1:2
  hb2 <- hdbase_table_update(hb, table_slug = "cars", new_cars)
  hb$hdtables
  hb2$hdtables_slugs()
  x <- hb2$hdtables$cars$data
  expect_equal(x, new_cars)

  # update new

  l <- list(cars = cars, mtcars = mtcars)
  hb <- hdbase(l)
  new_cars <- tibble::as_tibble(cars[1:2,])
  new_cars$n <- 1:2
  hb2 <- hdbase_table_update(hb, table_slug = "new_cars", new_cars)
  expect_equal(hb2$hdtables_slugs(), c("cars", "mtcars", "new_cars"))

})
