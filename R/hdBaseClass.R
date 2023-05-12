
hdBaseClass <- R6::R6Class(
  "hdBase",
  public = list(
    name = NULL,
    description = NULL,
    slug = NULL,
    formats = NULL,
    hdTables = NULL,
    meta = NULL,
    hdBaseType = NULL,
    hdBaseTypeGroup = NULL,

    initialize = function(hdTables, dic = NULL, hdTableType = NULL,
                          name = NULL, description = NULL,
                          slug = NULL, formats = NULL,
                          meta = NULL) {

      name <- name %||% deparse(substitute(d))
      description <- description %||% ""
      slug <- slug %||% dstools::create_slug(name)
      formats <- unique(c(c('csv', 'json'), formats))


      if(are_hdTables(hdTables)){
        self$hdTables <- hdTables
      }

      self$name <- name
      self$description <- description
      self$slug <- slug
      self$formats <- formats
      self$meta <- meta
      self$hdBaseType <- self$hdBaseType_creator()
      self$hdBaseTypeGroup <- self$hdBaseTypeGroup_creator()

    },
    hdTables_slugs = function(){
      self$hdTables[[1]]$slug
      hdTables_slugs(self$hdTables)
    },
    hdBaseType_creator = function(){
      hdBase <- hdTables_hdTableTypes(self$hdTables)
      paste(hdBase, collapse = "__")
    },
    hdBaseTypeGroup_creator = function(){
      hdBaseGroup <- hdTables_hdTableTypeGroups(self$hdTables)
      paste(sort(hdBaseGroup), collapse = "__")
    },
    metadata = function(){
      base_info <- list(
        name = self$name,
        description = self$description,
        slug = self$slug,
        formats = self$formats,
        credits = self$credits,
        hdBaseType = as.character(self$hdBaseType),
        hdBaseTypeGroup = as.character(self$hdBaseTypeGroup),
        hdTables_slugs = self$hdTables_slugs()
      )
      hdts_info <- purrr::map(self$hdTables, function(hdt){
        metadata <- hdt$metadata()
        metadata$hdTableType <- as.character(metadata$hdTableType)
        metadata
      })
      names(hdts_info) <- hdTables_slugs(self$hdTables)
      hdts_info <-  list("hdTables" = hdts_info)
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
      purrr::walk(self$hdTables, function(hdt){
        hdTable_write(hdt, path)
      })
    }
  )
)


hdTables_slugs <- function(ts){
  purrr::map_chr(ts, ~ .$slug)
}

hdTables_hdTableTypes <- function(ts){
  purrr::map_chr(ts, ~ .$hdTableType)
}

hdTables_hdTableTypeGroups <- function(ts){
  purrr::map_chr(ts, ~ .$hdTableTypeGroup)
}




are_hdTables <- function(ts){
  all(purrr::map_lgl(ts, is_hdTable))
}

are_data_frames <- function(ts){
  if(is.null(ts)) return(FALSE)
  all(purrr::map_lgl(ts, ~ inherits(. ,"data.frame")))
}







