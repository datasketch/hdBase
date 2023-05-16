
hdbaseClass <- R6::R6Class(
  "hdbase",
  public = list(
    name = NULL,
    description = NULL,
    slug = NULL,
    formats = NULL,
    hdtables = NULL,
    meta = NULL,
    hdbaseType = NULL,
    hdbaseTypeGroup = NULL,

    initialize = function(hdtables, dic = NULL, hdtableType = NULL,
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
      self$hdbaseType <- self$hdbaseType_creator()
      self$hdbaseTypeGroup <- self$hdbaseTypeGroup_creator()

    },
    hdtables_slugs = function(){
      self$hdtables[[1]]$slug
      hdtables_slugs(self$hdtables)
    },
    hdbaseType_creator = function(){
      hdbase <- hdtables_hdtableTypes(self$hdtables)
      paste(hdbase, collapse = "__")
    },
    hdbaseTypeGroup_creator = function(){
      hdbaseGroup <- hdtables_hdtableTypeGroups(self$hdtables)
      paste(sort(hdbaseGroup), collapse = "__")
    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        credits = self$credits,
        hdbaseType = as.character(self$hdbaseType),
        hdbaseTypeGroup = as.character(self$hdbaseTypeGroup),
        hdtables_slugs = self$hdtables_slugs()
      )
      hdts_info <- purrr::map(self$hdtables, function(hdt){
        metadata <- hdt$metadata()
        metadata$hdtableType <- as.character(metadata$hdtableType)
        metadata
      })
      names(hdts_info) <- hdtables_slugs(self$hdtables)
      hdts_info <-  list("hdtables" = hdts_info)
      c(base_info, self$meta, hdts_info)
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
  purrr::map_chr(ts, ~ .$slug)
}

hdtables_hdtableTypes <- function(ts){
  purrr::map_chr(ts, ~ .$hdtableType)
}

hdtables_hdtableTypeGroups <- function(ts){
  purrr::map_chr(ts, ~ .$hdtableTypeGroup)
}




are_hdtables <- function(ts){
  all(purrr::map_lgl(ts, is_hdtable))
}

are_data_frames <- function(ts){
  if(is.null(ts)) return(FALSE)
  all(purrr::map_lgl(ts, ~ inherits(. ,"data.frame")))
}







