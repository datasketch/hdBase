
hdbaseClass <- R6::R6Class(
  "hdbase",
  public = list(
    name = NULL,
    description = NULL,
    slug = NULL,
    formats = NULL,
    hdtables = NULL,
    meta = NULL,
    hdbase_type = NULL,
    hdbase_type_group = NULL,

    initialize = function(hdtables, dic = NULL, hdtable_type = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, formats = NULL,
                          meta = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)
      formats <- unique(c(c('csv', 'json'), formats))


      if(are_hdtables(hdtables)){
        self$hdtables <- hdtables
      }

      self$name <- name
      self$description <- description
      self$slug <- slug
      self$formats <- formats
      self$meta <- meta
      self$hdbase_type <- self$hdbase_type_creator()
      self$hdbase_type_group <- self$hdbase_type_group_creator()

    },
    hdtables_slugs = function(){
      self$hdtables[[1]]$slug
      hdtables_slugs(self$hdtables)
    },
    hdbase_type_creator = function(){
      hdbase <- hdtables_hdtable_types(self$hdtables)
      paste(hdbase, collapse = "__")
    },
    hdbase_type_group_creator = function(){
      hdbase_group <- hdtables_hdtable_types(self$hdtables)
      paste(sort(hdbase_group), collapse = "__")
    },
    hdtables_meta = function(){
      hdts_meta <- purrr::map(self$hdtables, function(hdt){
        metadata <- hdt$metadata()
        metadata$hdtable_type <- as.character(metadata$hdtable_type)
        metadata
      })
      names(hdts_meta) <- hdtables_slugs(self$hdtables)
      hdts_meta
    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        credits = self$credits,
        hdtable_type = as.character(self$hdtable_type),
        hdtable_type_group = as.character(self$hdtable_type_group),
        hdtables_slugs = self$hdtables_slugs()
      )
      hdts_meta <- self$hdtables_meta()
      hdts_meta <-  list("hdtables_meta" = hdts_meta)
      c(base_info, self$meta, hdts_meta)
    },
    write_meta_json = function(path = ""){
      if(!dir.exists(path)) dir.create(path, recursive = TRUE)
      save_path <- file.path(path,paste0(self$slug,".base.json"))
      metadata <- self$metadata()
      jsonlite::write_json(metadata, save_path,
                           auto_unbox = TRUE, pretty = TRUE)
    },
    write = function(path){
      path <- file.path(path, self$slug)
      self$write_meta_json(path)
      # Returns list of tibbles ready to write
      purrr::walk(self$hdtables, function(hdt){
        hdtable_write(hdt, path)
      })
    }
  )
)


hdtables_slugs <- function(ts){
  purrr::map_chr(ts, ~ .$slug) |> unname()
}

hdtables_hdtable_types <- function(ts){
  purrr::map_chr(ts, ~ .$hdtable_type)
}

hdtables_hdtable_types <- function(ts){
  purrr::map_chr(ts, ~ .$hdtable_type)
}




are_hdtables <- function(ts){
  all(purrr::map_lgl(ts, is_hdtable))
}

are_data_frames <- function(ts){
  if(is.null(ts)) return(FALSE)
  all(purrr::map_lgl(ts, ~ inherits(. ,"data.frame")))
}







