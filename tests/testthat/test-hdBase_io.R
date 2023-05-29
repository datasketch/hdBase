test_that("hdbase io", {

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  hdb <- hdbase(d, name = "Los Carros", mas = "fda", formats = "xlsx")

  hdb$write("tmp/hdb")


  expected_write_ext <- c(".base.json",".csv", ".dic.csv", ".dic.json", ".json",
                          ".meta.json", ".xlsx")
  expect_equal(list.files("tmp/hdb/los-carros"),
               paste0(hdb$slug, expected_write_ext))


})





test_that("hdbase read write", {

  ####

  l <- list(cars = head(cars), iris = head(iris))
  hb <- hdbase(l, license = "MIT")

  # Check how it is saved
  metadata <- hb$metadata()
  jsonlite::write_json(metadata, "tmp/metadata.json",
                       auto_unbox = TRUE, pretty = TRUE)

  expect_true(is_hdbase(hb))

  hb$name
  hb$metadata()

  hdbase_write(hb, "tmp")
  expect_true(dir.exists(file.path("tmp", hb$slug)))

  hb_meta <- jsonlite::read_json(
    file.path("tmp", hb$slug,
              paste0(hb$slug,".base.json"))
  )
  expect_equal(hb$name, hb_meta$name)

  ## Read

  path <- "tmp/l"

  hb_read <- hdbase_read(path)

  expect_equal(hb$name, hb_read$name)
  expect_equal(hb$license, hb_read$license)

  expect_equal(hb$hdtables, hb_read$hdtables)

  unlink("tmp", recursive = TRUE)


})

test_that("Read large db", {

  path <- "tmp/large_files"
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  file_path_rand <- file.path(path, "rand.csv")
  file_path_rand_dic <- file.path(path, "rand.dic.csv")
  file_path_flights <- file.path(path, "nycflights.csv")
  file_path_flights_dic <- file.path(path, "nycflights.dic.csv")

  # Create large table
  rand <- rep(dstools::random_name(100000), 10000)
  rand <- tibble::tibble(rand = rand, n = 1:length(rand))


  # tictoc::tic()
  vroom::vroom_write(rand, file_path_rand)
  x <- nycflights13::flights
  vroom::vroom_write(x, file_path_flights)
  # tictoc::toc()

  dic_rand <- tibble::tibble(id = c("rand", "n"),
                        hdtype = c("Cat", "Num"))
  vroom::vroom_write(dic_rand, file_path_rand_dic)
  dic_flights <- hdtable::create_dic(head(x, 1000))
  vroom::vroom_write(dic_flights, file_path_flights_dic)

  ## Read HDBASE
  ts <- path
  h <- hdbase(ts)

  t1 <- h$hdtables[[1]]
  expect_null(t1$dd)
  data <- t1$data
  expect_equal(nrow(t1$dd), nrow(x))
  expect_equal(h$hdtables_slugs(), c("nycflights", "rand"))

  path <- "tmp/large_files_base"
  hdbase_write(h, path)


  t2 <- hdbase_read(file.path(path, "ts"))





  # tictoc::tic()
  # rand <- vroom::vroom(file_path_rand, show_col_types = FALSE)
  # flights <- vroom::vroom(file_path_flights, show_col_types = FALSE)
  # tictoc::toc()

  unlink("tmp/large_files", recursive = TRUE)

})




test_that("read database",{


  with_na <- tibble::tibble(a = 1:5, z = rep(NA,5))
  h <- hdbase(list(with_na = with_na, cars = cars))
  hdbase_write(h, "tmp/with_na")
  path <- "tmp/with_na/list-with_na-with_na-cars-cars/"
  h2 <- hdbase_read(path)

  expect_equal(h, h2)

  h_meta <- hdbase_hdtables_meta(h)
  expect_equal(h_meta$cars$nrow, 50)

  h2_meta <- hdbase_hdtables_meta(h)
  expect_equal(h_meta, h2_meta)


})








