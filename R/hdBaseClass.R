
hdBaseClass <- R6::R6Class(
  "hdBase",
  public = list(
    name = NULL,
    description = NULL,
    slug = NULL,
    hdTables = NULL,
    meta = NULL,
    hdBaseType = NULL,

    initialize = function(hdTables, dic = NULL, hdTableType = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, meta = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)



      if(are_hdTables(hdTables)){
        self$hdTables <- hdTables
      }

      self$name <- name
      self$description <- description
      self$slug <- slug
      self$meta <- meta

    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        nrow = self$nrow,
        ncol = self$ncol,
        credits = self$credits
      )
      hdts_info <- purrr::map(self$hdTables, function(hdt){
        hdt$metadata()
      })
      names(hdts_info) <- hdTables_slugs(self$hdTables)
      hdts_info <-  list("hdTables" = hdts_info)
      c(base_info, self$meta, hdts_info)
    },
    hdTables_slugs = function(){
      self$hdTables[[1]]$slug
      hdTables_slugs(self$hdTables)
    },
    write = function(){
      # Returns list of tibbles ready to write
      purrr::map(self$hdTables, function(hdt){

      })
    }
  )
)

are_hdTables <- function(ts){
  all(purrr::map_lgl(ts, is_hdTable))
}

hdTables_slugs <- function(ts){
  purrr::map_chr(ts, ~ .$slug)
}







