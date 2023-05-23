
## code to prepare `DATASET` dataset goes here

b <- hdbase(list(cars = cars, iris = iris),
            name = "Cars and Iris"
            )
hdbases <- list(cars_iris = b)

usethis::use_data(hdbases, overwrite = TRUE)
