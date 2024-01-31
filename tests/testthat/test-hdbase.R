test_that("Create hdbase ", {

  # NULL case
  ts <- NULL
  hb <- hdbase(ts)

  expect_null(hb$hdtables)

  # From a list of hdtables
  t1 <- hdtable(cars, name = "CARS")
  t2 <- hdtable(iris)
  ts <- list(t1,t2)
  hb <- hdbase(ts)
  hbts <- hb$hdtables
  expect_equal(names(hbts), hb$hdtables_slugs())
  expect_true(are_hdtables(hbts))
  slugs <- hdtables_slugs(hbts)

  expect_equal(slugs, c("cars", "iris"))
  expect_equal(names(hb$metadata()$hdtables_meta), c("cars", "iris"))


  # From a data.frame
  hb <- hdbase(iris, license = "MIT")
  expect_equal(hb$name, "iris")

  hb$hdtables_slugs()

  hdts <- hb$hdtables
  expect_equal(hdtables_slugs(hdts), "iris")

  hb$metadata()
  expect_equal(names(hb$metadata()$hdtables_meta), "iris")
  expect_equal(hb$meta$license, "MIT")

  # From a list of data.frames

  l <- list(cars = head(cars), iris = head(iris))
  hb <- hdbase(l, license = "MIT")

  today <- as.character(Sys.Date())
  hdbase(l, lazy = FALSE, last_updated = today)

  hb$hdtables_slugs()

  expect_null(names(hb$hdtables_slugs()))



  ht1 <- hb$hdtables[[1]]
  expect_equal(ht1$name, "cars")
  ht2 <- hb$hdtables[[2]]
  expect_equal(ht2$name, "iris")
  expect_equal(names(hb$metadata()$hdtables_meta),c("cars","iris"))

  expect_equal(hb$hdbase_type, paste0(ht1$hdtable_type,"__",ht2$hdtable_type))
  expect_equal(hb$hdbase_type_group,
               paste0(sort(c(ht1$hdtable_type, ht2$hdtable_type)), collapse = "__"))

  # From a list of data.frames with a list of dictionaries

  l <- list(cars = head(cars), iris = head(mtcars))
  dics <- lapply(l, hdtable::create_dic)
  hdts <- l
  hb <- hdbase(l, dic = dics)
  expect_true(is_hdbase(hb))
  expect_equal(hdbase_hdtables(hb)[[1]]$data, head(cars),
               ignore_attr = TRUE)

})


test_that("hdbase from folder of CSVs", {

  path <- "tmp/folder"
  if(!dir.exists(path)) dir.create(path, recursive = TRUE)

  readr::write_csv(iris, file.path(path,"iris.csv"))
  readr::write_csv(cars, file.path(path,"cars.csv"))

  h <- hdbase(path)

  expect_true(is_hdbase(h))

  unlink(path, recursive = TRUE)

})



test_that("hdbase convenience funs", {


  # b <- hdbase:::base_samples$cars_iris
  #
  # expect_equal(names(hdbase_hdtables(b)), c("cars", "iris"))
  #
  # expect_equal(hdbase_hdtables_types(b),
  #              list(
  #                cars = hdtable::hdtable_type("Num-Num"),
  #                iris = hdtable::hdtable_type("Num-Num-Num-Num-Cat")
  #              ))


})



