
hdView <- R6Class(
  "hdView",
  public = list(
    slug = NULL,
    statement = NULL,
    tables = NULL,
    type = NULL, # flat or nested
    data = NULL,

    initialize = function(slug = NULL, statement = NULL, tables = NA) {
      self$slug <- slug
      self$statement <- statement
      self$tables <- tables
      self$data <- NULL
      self$type <- "flat"
    },

    run = function(data) {
      self$data <- data
    },

    write = function() {
      return(self$data)
    }
  )
)
