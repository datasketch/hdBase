test_that("hdBase io", {

  d <- tibble::tibble("Helloo X" = 1, "x 43" = as.Date("2020-04-21"))
  hdb <- hdBase(d, name = "Los Carros", mas = "fda", formats = "xlsx")

  hdb$write("tmp/hdb")


  expected_write_ext <- c(".base.json",".csv", ".dic.csv", ".dic.json", ".json",
                          ".meta.json", ".xlsx")
  expect_equal(list.files("tmp/hdb/los-carros"),
               paste0(hdb$slug, expected_write_ext))


  l <- list(cars = head(cars), iris = head(iris))
  hb <- hdBase(l, license = "MIT")

  expect_true(is_hdBase(hb))

  hb$name

  hdBase_write(hb, "tmp")
  expect_true(dir.exists(file.path("tmp", hb$slug)))

  hb_meta <- jsonlite::read_json(
    file.path("tmp", hb$slug,
              paste0(hb$slug,".base.json"))
    )
  expect_equal(hb$name, hb_meta$name)


  ## Read

  path <- "tmp/l"

  hb_read <- hdBase_read(path)

  expect_equal(hb$name, hb_read$name)
  expect_equal(hb$license, hb_read$license)

  expect_equal(hb$hdTables, hb_read$hdTables)



})
